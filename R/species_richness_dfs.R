species_richness_dfs <- function(...){
  
  site_species_richness_per_yr <- veg_joined$plot_veg_cover %>% # site-level species richness in each year
    group_by(UniqueID, UnitCode, Year_chr) %>%
    summarise(site_richness = n_distinct(SciName_cor)) %>%
    # add in plot counts for each site in each year for use in regression
    left_join(.,
              veg_cleaned$plot_perc_cover %>%
                ungroup() %>%
                select(UnitCode, UniqueID, Year_chr, EventID) %>%
                group_by(UnitCode, UniqueID, Year_chr) %>%
                summarise(plot_count = n_distinct(EventID)),
              by = c("UnitCode", "UniqueID", "Year_chr"))
  
  total_park_species_richness <- veg_joined$plot_veg_cover %>% # total park-level species richness across all years
    group_by(UnitCode) %>%
    summarise(plot_num = n_distinct(EventID),
              site_num = n_distinct(UniqueID),
              total_richness = n_distinct(SciName_cor)) %>%
    ungroup() %>% 
    pluck(., "total_richness")
  
  park_species_richness_per_yr <- site_species_richness_per_yr %>% # park-level species richness in each year (for plotting)
    group_by(UnitCode, Year_chr) %>%
    summarise(total_richness = mean(site_richness, na.rm = TRUE),
              se_richness = sd(site_richness)/sqrt(length(site_richness)))
  
  species_richness_mods <- site_species_richness_per_yr %>% # poisson glm for species richness
    group_by(UnitCode) %>%
    mutate(year = as.numeric(Year_chr), 
           year_num = as.numeric(year-min(year))) %>%
    nest()  %>%
    mutate(pois_mod = map(data, ~glm(site_richness ~ year_num + offset(log(plot_count)), family = "poisson", data = .x)), # https://stats.stackexchange.com/questions/237963/how-to-formulate-the-offset-of-a-glm
           dispersion = map(pois_mod, ~AER::dispersiontest(.x, trafo = 1)), # test for overdispersion
           summary_mod = map(pois_mod, ~summary(.x)),
           null_mod = map(data, ~glm(site_richness ~ 1 + offset(log(plot_count)), family = "poisson", data = .x)),
           summary_null = map(null_mod, ~summary(.x)),
           chisq_calc = map2_dbl(null_mod, pois_mod, ~lmtest::lrtest(.x, .y)$`Chisq`[[2]]),
           LR_chisq = format_chi(chisq_calc),, 
           pval = map_dbl(summary_mod, ~coef(.x)[8]),
           b1 =  map_dbl(summary_mod, ~coef(.x)[2]),
           b1_se = map_dbl(summary_mod, ~coef(.x)[4]), 
           change_count = map_dbl(b1, ~(exp(.x*1)-1)*100), # https://stats.stackexchange.com/questions/344291/interpreting-poisson-regression-coefficients
           change_count_se = map_dbl(b1_se, ~abs((exp(.x*1)-1)*100)),
           format_p = format_pval(pval)
    )
  
  richness_plot_betas <- species_richness_mods %>%
    mutate(pois_mod_no_offset = map(data, ~glm(site_richness ~ year_num, family = "poisson", data = .x)),
           min_date = map(data, ~as.Date(paste0(min(.x$year), "-01-01"))-(0.4*365.25)),
           min_date_num = map(data, ~min(.x$year_num)-(1)),
           max_date_num = map(data, ~max(.x$year_num)+(2)),
           nd = map2(min_date_num, max_date_num, ~data.frame("year_num" = seq(.x, .y, by = 0.5))),
           pred = map2(pois_mod_no_offset, nd, ~data.frame(bind_cols(nd, "site_richness" = predict(.x, newdata = .y, type = "response")) %>% mutate(site_richness = as.numeric(site_richness))))) %>%
    dplyr::select(UnitCode, pred, min_date, min_date_num, max_date_num) %>%
    unnest(cols = c(pred, min_date, min_date_num, max_date_num)) %>%
    mutate(Year = as.Date(min_date) + (year_num*365.25),
          richness_metric = "total richness")
  
  results <- park_species_richness_per_yr %>%
    group_by(UnitCode) %>%
    summarise(mean_richness = round(mean(total_richness), 0),
              se_richness = signif(sd(total_richness)/sqrt(length(total_richness)), 1)) %>%
    mutate(mean_se_text = if_else(se_richness < 10, 
                                  paste0(mean_richness, " ± ", signif(se_richness, 1), " species"),
                                  paste0(mean_richness, " ± ", signif(se_richness, 2), " species"))) %>%
    left_join(., species_richness_mods, by = "UnitCode") %>%
    ungroup() %>%
    mutate(change_count_se_text = paste0(round(change_count, 0), " ± ", signif(change_count_se, 1), "% sp/yr"),
           change_count_lab = paste0("Species richness: ", change_count_se_text)) 
  
  return(list(
    "site_species_richness_per_yr" = site_species_richness_per_yr,
    "total_park_species_richness" = total_park_species_richness,
    "park_species_richness_per_yr" = park_species_richness_per_yr,
    "species_richness_mods" = species_richness_mods,
    "richness_plot_betas" = richness_plot_betas,
    "results" = results
  ))
}