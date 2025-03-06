source(here::here("R", "salinity_class.R"))

salinity_dfs <- function(...){
  
  sal_category_count_per_park <- veg_cleaned$site_frq %>% # count of species in each salinity category in each park
    ungroup() %>%
    sal_class(.) %>%
    mutate(sal_code = if_else(is.na(sal_code), "unknown", sal_code)) %>%
    group_by(UnitCode, sal_code) %>%
    summarise(sp_count = n_distinct(SciName_cor)) %>%
    split(.$sal_code)
  
  sal_category_sps_list <- veg_cleaned$site_frq %>% # count of species in each salinity category in each park
    ungroup() %>%
    sal_class(.) %>%
    mutate(sal_code = if_else(is.na(sal_code), "unknown", sal_code)) %>% 
    group_by(UnitCode, sal_code) %>%
    distinct(SciName_cor) %>%
    arrange(SciName_cor) %>%
    nest() %>%
    mutate(sp_list = map(data, ~knitr::combine_words(if_else(lengths(strsplit(.x$SciName_cor, "\\W+")) > 1,
                                                             paste0("*",.x$SciName_cor,"*"),
                                                             paste0("an unidentified ", "*", .x$SciName_cor, "*", " species"))))) %>% 
    ungroup() %>% 
    dplyr::select(-data)
  
  site_sal_count <- veg_cleaned$site_frq %>% 
    ungroup() %>%
    sal_class(.) %>%
    group_by(UniqueID, UnitCode, Year_chr, sal_code) %>%
    filter(!is.na(sal_code)) %>%
    # the following lines were used to add in 0's for salinity categories were no species were found at a site in a particular year
    mutate(f = if_else(frq == 0, "a", "b")) %>%
    group_by(UniqueID, UnitCode, Year_chr, sal_code, f) %>%
    summarise(site_species_count = n_distinct(SciName_cor)) %>%
    mutate(site_species_count2 = if_else(f == "a", 0, site_species_count)) %>%
    group_by(UniqueID, UnitCode, Year_chr, sal_code) %>%
    summarise(site_species_count = sum(site_species_count2)) %>%
    ungroup() %>%
    {if (params$park == "SAHI") 
      add_row(., UnitCode = "SAHI", UniqueID = "SAHI_SAG1", sal_code = "Low", Year_chr = c("2009", "2011", "2013", "2015", "2017"), site_species_count = 0)
      else
        .} %>%
    # add in plot counts to be used in regressions
    left_join(.,
              veg_cleaned$plot_perc_cover %>%
                ungroup() %>%
                select(UnitCode, UniqueID, Year_chr, EventID) %>%
                group_by(UnitCode, UniqueID, Year_chr) %>%
                summarise(plot_count = n_distinct(EventID)),
              by = c("UnitCode", "UniqueID", "Year_chr"))
  
  site_sal_frq <- veg_cleaned$plot_perc_cover %>% 
    ungroup() %>%
    select(EventID, UniqueID, UnitCode, Year_chr, SciName_cor, SalinityCode, SalinityTolerance) %>%
    sal_class(.) %>%
    select(-c(SalinityCode, SalinityTolerance)) %>%
    group_by(UniqueID, UnitCode, Year_chr) %>%
    mutate(total_site_plots = n_distinct(EventID)) %>%
    group_by(UniqueID, UnitCode, Year_chr, sal_code) %>%
    mutate(total_site_present = n_distinct(EventID)) %>%
    mutate(site_frq = total_site_present/total_site_plots) %>%
    group_by(UniqueID, UnitCode, Year_chr, sal_code) %>%
    summarise(mean_site_frq = mean(site_frq)*100) %>%
    filter(!is.na(sal_code)) %>%
    # the following lines were used to add in 0's for salinity categories where no species were found at a site in a particular year
    group_by(UnitCode) %>%
    nest() %>%
    mutate(df = map(data, ~pivot_wider(.x, names_from = "Year_chr", values_from = "mean_site_frq", values_fill = 0)),
           df2 = case_when(UnitCode == "ASIS" ~ map(df, ~add_row(.x, UniqueID = c("ASIS_A3", "ASIS_A3", "ASIS_A4", "ASIS_A4", "ASIS_A9", "ASIS_A8", "ASIS_A8"), sal_code = c("Medium", "Low", "Medium", "Low", "Low", "Medium", "Low"))),
                           UnitCode == "FIIS" ~ map(df, ~add_row(.x, UniqueID = "FIIS_F6", sal_code = c("Medium", "Low"))),
                           UnitCode == "GATE" ~ map(df, ~add_row(.x, UniqueID = "GATE_GSH3", sal_code = "Low")),
                           UnitCode == "SAHI" ~ map(df, ~add_row(.x, UniqueID = "SAHI_SAG1", sal_code = "Low")),
                           T ~ df),
           df3 = map(df2, ~pivot_longer(.x, cols = -c(UniqueID, sal_code), names_to = "Year_chr", values_to = "mean_site_frq") %>%
                       mutate(mean_site_frq = if_else(is.na(mean_site_frq), 0, mean_site_frq)))) %>%
    ungroup() %>%
    select(-c(data, df, df2)) %>%
    unnest(cols = c(df3)) %>%
    left_join(.,
              veg_cleaned$plot_perc_cover %>%
                ungroup() %>%
                select(UnitCode, UniqueID, Year_chr, EventID) %>%
                group_by(UnitCode, UniqueID, Year_chr) %>%
                summarise(plot_count = n_distinct(EventID)),
              by = c("UnitCode", "UniqueID", "Year_chr")) %>%
    filter(!is.na(plot_count))
  
  site_sal_cover <- veg_cleaned$site_perc_cover %>% 
    ungroup() %>%
    select(UniqueID, UnitCode, Year_chr, SciName_cor, SalinityCode, SalinityTolerance, mean_site_cover) %>%
    sal_class(.) %>%
    filter(!is.na(sal_code)) %>%
    group_by(UniqueID, UnitCode, Year_chr, sal_code) %>%
    summarise(mean_site_cover = sum(mean_site_cover)) %>%
    group_by(UnitCode) %>%
    nest() %>%
    mutate(df = map(data, ~pivot_wider(.x, names_from = "Year_chr", values_from = "mean_site_cover", values_fill = 0) %>%
                      pivot_longer(., cols = -c(UniqueID, sal_code), names_to = "Year_chr", values_to = "mean_site_cover"))) %>%
    ungroup() %>%
    select(-c(data)) %>%
    unnest(cols = c(df)) %>%
    left_join(., 
              veg_cleaned$plot_perc_cover %>% 
                ungroup() %>% 
                select(UnitCode, UniqueID, Year_chr, EventID) %>% 
                group_by(UnitCode, UniqueID, Year_chr) %>% 
                summarise(plot_count = n_distinct(EventID)),
              by = c("UnitCode", "UniqueID", "Year_chr")) %>%
    filter(!is.na(plot_count)) %>%
    ungroup() %>%
    {if (params$park == "SAHI")
      add_row(., UnitCode = "SAHI", UniqueID = "SAHI_SAG1", sal_code = "Low", Year_chr = c("2009", "2011", "2013", "2015", "2017"), mean_site_cover = 0, plot_count = c(50, 40, 45, 46, 44))
      else
        .}
  
  
  sal_count_mods <- site_sal_count %>%
    filter(!(UnitCode== "SAHI" & sal_code == "Low")) %>%
    filter(!(UnitCode == "GEWA" & sal_code == "High")) %>%
    group_by(UnitCode) %>%
    mutate(year = as.numeric(Year_chr),
           year_num = as.numeric(year-min(year))) %>%
    group_by(UnitCode, sal_code) %>%
    nest() %>%
    mutate(pois_mod = map(data, ~glm(site_species_count ~ year_num + offset(log(plot_count)), family = "poisson", data = .x)),
           dispersion = map(pois_mod, ~AER::dispersiontest(.x, trafo = 1)), # test for overdispersion
           summary_mod = map(pois_mod, ~summary(.x)),
           null_mod = map(data, ~glm(site_species_count ~ 1 + offset(log(plot_count)), family = "poisson", data = .x)),
           summary_null = map(null_mod, ~summary(.x)),
           chisq_calc = map2_dbl(null_mod, pois_mod, ~lmtest::lrtest(.x, .y)$`Chisq`[[2]]),
           LR_chisq = format_chi(chisq_calc),, 
           pval = map_dbl(summary_mod, ~coef(.x)[8]),
           b1 =  map_dbl(summary_mod, ~coef(.x)[2]),
           b1_se = map_dbl(summary_mod, ~coef(.x)[4]), 
           change_count = map_dbl(b1, ~(exp(.x*1)-1)*100), # https://stats.stackexchange.com/questions/344291/interpreting-poisson-regression-coefficients
           change_count_se = map_dbl(b1_se, ~abs((exp(.x*1)-1)*100)),
           format_p = format_pval(pval),
           sal_code_plot_lab = if_else(sal_code == "Medium", "Med", sal_code),
           change_count_lab = paste0(sal_code_plot_lab, ": ", format_sps(change_count), " ± ", format_sps(change_count_se), "% sp/yr"),
           change_count_text = paste0(format_sps(change_count), " ± ", format_sps(change_count_se), "% of species/year"),
           vjust = case_when(sal_code == "Medium" & UnitCode == "ASIS" ~ 3.25,
                             T ~ NA_real_
           )
    )
  
  sal_frq_mods <- site_sal_frq %>% 
    filter(!(UnitCode == "SAHI" & sal_code %in% c("High", "Low"))) %>% # couldn't run model for SAHI since high salinity was 100% frq in all years and low salinity was 0 % frq in all years
    group_by(UnitCode) %>%
    mutate(year = as.numeric(Year_chr),
           year_num = as.numeric(year-min(year)),
           binom_mean_site_frq = mean_site_frq/100) %>%
    group_by(UnitCode, sal_code) %>%
    nest() %>%
    mutate(binom_mod = map(data, ~glm(binom_mean_site_frq ~ year_num, data = .x, weights = plot_count, family = "binomial")), #https://stats.stackexchange.com/questions/623503/how-to-interpret-binomial-glm-coefficient-w-proportion-as-outcome?rq=1
           summary_mod = map(binom_mod, ~summary(.x)),
           chisq_calc = map_dbl(binom_mod, ~stats::drop1(.x, scope = ~year_num, test = "Chisq")[[4]][[2]]),
           LR_chisq = format_chi(chisq_calc),
           pval = map_dbl(summary_mod, ~coefficients(.x)[[8]]),
           int = map_dbl(summary_mod, ~coefficients(.x)[[1]]),
           b1 =  map_dbl(summary_mod, ~coefficients(.x)[[2]]),
           b1_se = map_dbl(summary_mod, ~coefficients(.x)[[4]]), 
           margs = map2(binom_mod, data, ~margins::margins(model = .x, data = .y, unit_ses = TRUE)),
           change_frq = map_dbl(margs, ~mean(.x$dydx_year_num)*100),
           change_frq_se = map_dbl(margs, ~mean(.x$SE_dydx_year_num)*100),
           r2 = map_dbl(summary_mod, ~1-.x$deviance/.x$null.deviance), #https://www.statology.org/glm-r-squared/
           sal_code_plot_lab = if_else(sal_code == "Medium", "Med", sal_code),
           change_frq_lab = paste0(sal_code_plot_lab, ": ", format_sps(change_frq), " ± ", format_sps(change_frq_se), "% plots/yr"),
           change_count_text = paste0(format_sps(change_frq), " ± ", format_sps(change_frq_se), "% of plots/year"),
           format_p = format_pval(pval),
           preds = map(binom_mod, ~data.frame(predict(.x, type = "response")*100)),
           vjust = case_when((sal_code == "High" & pval < 0.05) | (UnitCode == "ASIS" & sal_code == "Medium") | (UnitCode == "FIIS" & sal_code == "Medium") ~ 3.25,
                             UnitCode %in% c("COLO") & sal_code == "Medium" ~ 4.75,
                             UnitCode == "COLO" & sal_code == "Low" ~ 6.25,
                             T ~ NA_real_
           )
    )
  
  sal_cover_mods <- site_sal_cover %>% 
    filter(!(UnitCode == "SAHI" & sal_code %in% c("Low"))) %>% # couldn't run model for SAHI since high salinity was 100% frq in all years and low salinity was 0 % frq in all years
    group_by(UnitCode) %>%
    mutate(year = as.numeric(Year_chr),
           year_num = as.numeric(year-min(year)),
           beta_mean_site_cover = mean_site_cover/100) %>%
    group_by(UnitCode, sal_code) %>%
    nest() %>%
    mutate(beta_mod = #map(data, ~glm(binom_mean_site_cover ~ year_num, data = .x, weights = plot_count, family = "binomial")), #https://stats.stackexchange.com/questions/623503/how-to-interpret-binomial-glm-coefficient-w-proportion-as-outcome?rq=1
             map(data, ~betareg(beta_mean_site_cover~year_num,  data = .x, link = "loglog")),
           summary_mod = map(beta_mod, ~summary(.x)),
           int = map_dbl(summary_mod, ~.x$coefficients[[1]][[1]]),
           b1 =  map_dbl(summary_mod, ~.x$coefficients[[1]][[2]]),
           b1_se = map_dbl(summary_mod, ~.x$coefficients[[1]][[4]]), 
           change_cover = map2_dbl(int, b1, ~(plogis(.x + .y) - plogis(.x)))*100,
           # chisq_calc = map_dbl(binom_mod, ~stats::drop1(.x, scope = ~year_num, test = "Chisq")[[4]][[2]]),
           # LR_chisq = format_chi(chisq_calc), 
           pval = map_dbl(summary_mod, ~coefficients(.x)[[1]][[8]]),
           # int = map_dbl(summary_mod, ~coefficients(.x)[[1]]),
           # b1 =  map_dbl(summary_mod, ~coefficients(.x)[[2]]),
           # b1_se = map_dbl(summary_mod, ~coefficients(.x)[[4]]), 
           # margs = map2(binom_mod, data, ~margins::margins(model = .x, data = .y, unit_ses = TRUE)),
           # change_cover = map_dbl(margs, ~mean(.x$dydx_year_num)*100),
           # change_cover_se = map_dbl(margs, ~mean(.x$SE_dydx_year_num)*100),
           # r2 = map_dbl(summary_mod, ~1-.x$deviance/.x$null.deviance), #https://www.statology.org/glm-r-squared/
           # sal_code_plot_lab = if_else(sal_code == "Medium", "Med", sal_code),
           # change_cover_lab = paste0(sal_code_plot_lab, ": ", format_sps(change_cover), " ± ", format_sps(change_cover_se), "%/yr"),
           # change_count_text = paste0(format_sps(change_cover), " ± ", format_sps(change_cover_se), "%/yr"),
           # format_p = format_pval(pval),
           # preds = map(binom_mod, ~data.frame(predict(.x, type = "response")*100)),
           vjust = case_when(sal_code == "High" & pval < 0.05 ~ 3.25,
                             (UnitCode %in% c("COLO", "GEWA") & sal_code == "Medium") | (UnitCode == "FIIS" & sal_code == "Low") ~ 4.75,
                             UnitCode == "COLO" & sal_code == "Low" ~ 6.25,
                             T ~ NA_real_
           )
    )
  
  park_sal_count <- site_sal_count %>% # park-level mean count per year in each park - for bar labels on plots
    group_by(UnitCode, Year_chr, sal_code) %>%
    summarise(species_count = mean(site_species_count, na.rm = TRUE)) %>%
    mutate(value_label = if_else(round(species_count) < 2, NA, round(species_count)))
  
  park_sal_frq <- site_sal_frq %>% # park-level mean frq per year in each park - for bar labels on plots
    group_by(UnitCode, Year_chr, sal_code) %>%
    summarise(mean_frq = mean(mean_site_frq, na.rm = TRUE)) %>%
    mutate(value_label = if_else(round(mean_frq) < 10, NA, round(mean_frq)))
  
  park_sal_cover <- site_sal_cover %>% # park-level mean cover per year in each park - for bar labels on plots
    group_by(UnitCode, Year_chr, sal_code) %>%
    summarise(mean_cover = mean(mean_site_cover, na.rm = TRUE)) %>%
    mutate(value_label = if_else(round(mean_cover) < 10, NA, round(mean_cover))) 
  
  sal_mean_count_all_yrs <- park_sal_count %>% # yearly mean count of each salinity category in each park
    group_by(UnitCode, sal_code) %>%
    summarise(yearly_mean_park_count = format_sps(mean(species_count)),
              yearly_se_park_count = format_sps(sd(species_count)/sqrt(length(species_count)))) %>%
    mutate(mean_se_count = paste0(yearly_mean_park_count, " ± ", yearly_se_park_count, " species")) %>%
    split(.$sal_code)
  
  sal_mean_frq_all_yrs <- park_sal_frq %>% # yearly mean frq of each salinity category in each park
    group_by(UnitCode, sal_code) %>%
    summarise(yearly_mean_frq = format_sps(mean(mean_frq)),
              yearly_se_frq = format_sps(sd(mean_frq)/sqrt(length(mean_frq)))) %>%
    mutate(mean_se_frq = if_else(UnitCode == "SAHI" & sal_code == "High", 
                                 paste0(yearly_mean_frq, "% of plots"), 
                                 paste0(yearly_mean_frq, " ± ", yearly_se_frq, "% of plots"))) %>%
    split(.$sal_code)
  
  sal_mean_cover_all_yrs <- park_sal_cover %>% # yearly mean cover of each salinity category in each park
    group_by(UnitCode, sal_code) %>%
    summarise(yearly_mean_park_cover = format_sps(mean(mean_cover)),
              yearly_se_park_cover = format_sps(sd(mean_cover)/sqrt(length(mean_cover)))) %>%
    mutate(mean_se_cover = paste0(yearly_mean_park_cover, " ± ", yearly_se_park_cover, "%")) %>%
    split(.$sal_code)
  
  sal_count_results_text <- sal_count_mods %>%
    split(.$sal_code)
  
  sal_frq_results_text <- sal_frq_mods %>%
    split(.$sal_code)
  
  sal_cover_results_text <- sal_cover_mods %>%
    split(.$sal_code)
  
  return(list(
    "sal_category_count_per_park" = sal_category_count_per_park,
    "sal_category_sps_list" = sal_category_sps_list,
    "site_sal_count" = site_sal_count,
    "site_sal_frq" = site_sal_frq,
    "site_sal_cover" = site_sal_cover,
    "sal_count_mods" = sal_count_mods,
    "sal_frq_mods" = sal_frq_mods,
    "sal_cover_mods" = sal_cover_mods,
    "park_sal_count" = park_sal_count, 
    "park_sal_frq" = park_sal_frq,
    "park_sal_cover" = park_sal_cover,
    "sal_mean_count_all_yrs" = sal_mean_count_all_yrs,
    "sal_mean_frq_all_yrs" = sal_mean_frq_all_yrs,
    "sal_mean_cover_all_yrs" = sal_mean_cover_all_yrs,
    "sal_count_results_text" = sal_count_results_text,
    "sal_frq_results_text" = sal_frq_results_text,
    "sal_cover_results_text" = sal_cover_results_text
  ))
}