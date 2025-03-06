unique_species_dfs <- function(park){
  
  load(here::here("data", "derived", "veg_cleaned.rda"))
  
  park_unique_species <- veg_joined$plot_veg_cover %>% # list of unique species found in each park (i.e., species not found in any other parks)
    ungroup() %>%
    select(UnitCode, SciName_cor) %>%
    unique() %>%
    group_by(SciName_cor) %>%
    reframe(parks = as.list(unique(UnitCode))) %>%
    group_by(SciName_cor) %>%
    mutate(park_count = length(parks)) %>%
    filter(park_count == 1) %>%
    unnest(cols = c(parks)) %>%
    select("UnitCode" = parks, SciName_cor, park_count) %>%
    filter(UnitCode == park)
  
  park_unique_species_count <- veg_joined$plot_veg_cover %>% # park-level count of unique species across all years
    ungroup() %>%
    select(UnitCode, SciName_cor) %>%
    unique() %>%
    group_by(SciName_cor) %>%
    reframe(parks = as.list(unique(UnitCode))) %>%
    group_by(SciName_cor) %>%
    mutate(park_count = length(parks)) %>%
    filter(park_count == 1) %>%
    unnest(cols = c(parks)) %>%
    select("UnitCode" = parks, SciName_cor, park_count) %>%
    group_by(UnitCode) %>%
    summarise(n_unique = n_distinct(SciName_cor)) %>% 
    ungroup() %>% 
    filter(UnitCode == park) %>%
    pull(., n_unique)
  
  site_unique_species_count_per_year <- veg_joined$plot_veg_cover %>% # site-level count of unique species in each year
    ungroup() %>%
    select(UniqueID, UnitCode, Year_chr, SciName_cor) %>%
    unique() %>%
    left_join(., veg_joined$plot_veg_cover %>% # unique species found in each park (i.e., species not found in any other parks)
                ungroup() %>%
                select(UniqueID, UnitCode, SciName_cor) %>%
                unique() %>%
                group_by(SciName_cor) %>%
                reframe(parks = as.list(unique(UnitCode))) %>%
                group_by(SciName_cor) %>%
                mutate(park_count = length(parks)) %>%
                filter(park_count == 1) %>%
                unnest(cols = c(parks)) %>%
                select("UnitCode" = parks, SciName_cor, park_count), 
              by = c("UnitCode", "SciName_cor")) %>%
    replace_na(list(park_count = 0)) %>%
    group_by(UniqueID, UnitCode, Year_chr) %>%
    summarise(site_n_unique = sum(park_count)) %>%
    group_by(UnitCode) %>%
    nest() %>%
    #  add in 0s for sites where no unique species were found in a particular year 
    mutate(data = map(data, ~pivot_wider(.x, names_from = "Year_chr", values_from = "site_n_unique", values_fill = 0) %>%
                        pivot_longer(., cols = -c("UniqueID"), names_to = "Year_chr", values_to = "site_n_unique") %>%
                        mutate(year = as.numeric(Year_chr),
                               year_num = as.numeric(year-min(year))))) %>% 
    unnest(cols = c(data)) %>%
    # add in plot counts for each site in each year for use in regression
    left_join(.,
              veg_cleaned$plot_perc_cover %>%
                ungroup() %>%
                select(UnitCode, UniqueID, Year_chr, EventID) %>%
                group_by(UnitCode, UniqueID, Year_chr) %>%
                summarise(plot_count = n_distinct(EventID)),
              by = c("UnitCode", "UniqueID", "Year_chr")) %>%
    filter(!is.na(plot_count)) %>%
    filter(UnitCode == park)
  
  all_yrs_species <- veg_joined$site_veg_frq %>% # species that were found in every year at each park
    group_by(UnitCode, Year_chr, SciName_cor) %>%
    summarise(mean_frq = mean(frq, na.rm = TRUE)) %>%
    group_by(UnitCode, Year_chr) %>%
    arrange(UnitCode, SciName_cor, Year_chr) %>%
    group_by(UnitCode, SciName_cor) %>%
    mutate(n_years = n(),
           all_years = case_when(UnitCode == "ASIS" & n_years >= 6 ~ TRUE,
                                 UnitCode == "COLO" & n_years >= 5 ~ TRUE,
                                 UnitCode == "FIIS" & n_years >= 5 ~ TRUE,
                                 UnitCode == "GATE" & n_years >= 5 ~ TRUE,
                                 UnitCode == "GEWA" & n_years >= 3 ~ TRUE,
                                 UnitCode == "SAHI" & n_years >= 5 ~ TRUE,
                                 T ~ FALSE)) %>%
    ungroup() %>%
    filter(all_years == TRUE & UnitCode == park) %>%
    select(UnitCode, SciName_cor) %>%
    distinct()
  
  park_unique_species_count_per_year <- site_unique_species_count_per_year %>% # park-level count of unique species in each year for plotting
    group_by(UnitCode, Year_chr) %>%
    summarise(n_unique = mean(site_n_unique, na.rm = TRUE),
              se_n_unique = sd(site_n_unique)/sqrt(length(site_n_unique)))
  
  unique_species_mods <- site_unique_species_count_per_year %>% # poisson glm for species richness
    group_by(UnitCode) %>%
    nest() %>%
    mutate(pois_mod = map(data, ~glm(site_n_unique ~ year_num + offset(log(plot_count)), family = "poisson", data = .x)), # https://stats.stackexchange.com/questions/237963/how-to-formulate-the-offset-of-a-glm
           dispersion = map(pois_mod, ~AER::dispersiontest(.x, trafo = 1)), # test for overdispersion
           summary_mod = map(pois_mod, ~summary(.x)),
           null_mod = map(data, ~glm(site_n_unique ~ 1 + offset(log(plot_count)), family = "poisson", data = .x)),
           summary_null = map(null_mod, ~summary(.x)),
           chisq_calc = map2_dbl(null_mod, pois_mod, ~lmtest::lrtest(.x, .y)$`Chisq`[[2]]),
           LR_chisq = format_chi(chisq_calc),
           pval = map_dbl(summary_mod, ~coef(.x)[8]),
           b1 =  map_dbl(summary_mod, ~coef(.x)[2]),
           b1_se = map_dbl(summary_mod, ~coef(.x)[4]), 
           change_count = map_dbl(b1, ~(exp(.x*1)-1)*100), # https://stats.stackexchange.com/questions/344291/interpreting-poisson-regression-coefficients
           change_count_se = map_dbl(b1_se, ~abs((exp(.x*1)-1)*100)),
           format_p = format_pval(pval)
    )
  
  unique_plot_betas <- unique_species_mods %>%
    mutate(pois_mod_no_offset = map(data, ~glm(site_n_unique ~ year_num, family = "poisson", data = .x)),
           min_date = map(data, ~as.Date(paste0(min(.x$year), "-01-01"))-(0.4*365.25)),
           min_date_num = map(data, ~min(.x$year_num)-(1)),
           max_date_num = map(data, ~max(.x$year_num)+(2)),
           nd = map2(min_date_num, max_date_num, ~data.frame("year_num" = seq(.x, .y, by = 0.5))),
           pred = map2(pois_mod_no_offset, nd, ~data.frame(bind_cols(nd, "site_n_unique" = predict(.x, newdata = .y, type = "response")) %>% mutate(site_n_unique = as.numeric(site_n_unique))))) %>%
    dplyr::select(UnitCode, pred, min_date, min_date_num, max_date_num) %>%
    unnest(cols = c(pred, min_date, min_date_num, max_date_num)) %>%
    mutate(Year = as.Date(min_date) + (year_num*365.25),
           richness_metric = "n_unique")
  
  results <- park_unique_species_count_per_year %>%
    group_by(UnitCode) %>%
    summarise(mean_n_unique = mean(n_unique),
              se_n_unique = signif(sd(n_unique)/sqrt(length(n_unique)), 1)) %>%
    mutate(mean_se_text = if_else(se_n_unique < 1, 
                                  paste0(signif(mean_n_unique, 1), " ± ", signif(se_n_unique, 1), " species"),
                                  paste0(signif(mean_n_unique, 1), " ± ", signif(se_n_unique, 2), " species"))) %>%
    left_join(., unique_species_mods, by = "UnitCode") %>%
    ungroup() %>%
    mutate(change_count_se_text = paste0(round(change_count, 0), " ± ", signif(change_count_se, 1), "% sp/yr"),
           change_count_lab = paste0("Unique sp: ", change_count_se_text)) 
  
  park_unique_species_text <- park_unique_species %>% 
    group_by(UnitCode) %>% 
    arrange(SciName_cor) %>%
    nest() %>% 
    mutate(sp_list = map(data, ~knitr::combine_words(if_else(lengths(strsplit(.x$SciName_cor, "\\W+")) > 1,
                                                             paste0("*",.x$SciName_cor,"*"),
                                                             paste0("an unidentified ", "*", .x$SciName_cor, "*", " species"))))) %>% 
    ungroup() %>% 
    dplyr::select(-data)
  
  all_yrs_species_text <- all_yrs_species %>% 
    group_by(UnitCode) %>% 
    arrange(SciName_cor) %>%
    nest() %>% 
    mutate(sp_list = map(data, ~knitr::combine_words(if_else(lengths(strsplit(.x$SciName_cor, "\\W+")) > 1,
                                                             paste0("*",.x$SciName_cor,"*"),
                                                             paste0("an unidentified ", "*", .x$SciName_cor, "*", " species"))))) %>% 
    ungroup() %>% 
    dplyr::select(-data)
  
  return(list(
    "park_unique_species" = park_unique_species,
    "park_unique_species_count" = park_unique_species_count, 
    "site_unique_species_count_per_year" = site_unique_species_count_per_year,
    "all_yrs_species" = all_yrs_species,
    "park_unique_species_count_per_year" = park_unique_species_count_per_year,
    "unique_species_mods" = unique_species_mods,
    "unique_plot_betas" = unique_plot_betas,
    "results" = results,
    "park_unique_species_text" = park_unique_species_text, 
    "all_yrs_species_text" = all_yrs_species_text
  ))
}