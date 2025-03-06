invasive_species_dfs <- function(...){
  
  inv_species_cover <- veg_cleaned$park_perc_cover %>%
    mutate(is_invasive = case_when(if_any(starts_with("invasive_"), ~ .x == "T") ~ "T")) %>%
    filter(is_invasive == "T")
  
  inv_species_frq <- veg_cleaned$park_frq %>%
    mutate(is_invasive = case_when(if_any(starts_with("invasive_"), ~ .x == "T") ~ "T")) %>%
    filter(is_invasive == "T")
  
  # "plot_lims_x" = list(
  #   scale_x_date(limits = c(as.Date("2008-01-01"), as.Date("2018-01-01")), expand = c(.04, 0), sec.axis = dup_axis(), date_labels = "'%y"),
  #   scale_x_date(limits = c(as.Date("2008-01-01"), as.Date("2018-01-01")), expand = c(.04, 0), sec.axis = dup_axis(), date_labels = "'%y"),
  #   scale_x_date(limits = c(as.Date("2009-01-01"), as.Date("2019-01-01")), expand = c(.04, 0), sec.axis = dup_axis(), date_labels = "'%y")
  # )
  # 
  # "plot_lims_y_cover" = list(
  #   scale_y_continuous(limits = c(0, 3.2), breaks = seq(0,3, by = 1), expand = c(.04, 0), sec.axis = dup_axis(), name = "Cover of invasive\nspecies (%)"),
  #   scale_y_continuous(limits = c(0, 8.5), breaks = seq(0,8, by = 2), expand = c(.04, 0), sec.axis = dup_axis(), name = "Cover of invasive\nspecies (%)"),    
  #   scale_y_continuous(limits = c(0, 6.2), breaks = seq(0,6, by = 2), expand = c(.04, 0), sec.axis = dup_axis(), name = "Cover of invasive\nspecies (%)")
  # )
  # 
  # "plot_lims_y_frq" = list(
  #   scale_y_continuous(limits = c(0, 3.2), breaks = seq(0,3, by = 1), expand = c(.04, 0), sec.axis = dup_axis(), name = "Freq. of invasive\nspecies (% of plots)"),
  #   scale_y_continuous(limits = c(0, 25), breaks = seq(0,30, by = 10), expand = c(.04, 0), sec.axis = dup_axis(), name = "Freq. of invasive\nspecies (% of plots)"),
  #   scale_y_continuous(limits = c(0, 10.5), breaks = seq(0,9, by = 3), expand = c(.04, 0), sec.axis = dup_axis(), name = "Freq. of invasive\nspecies (% of plots)")
  # )
  
  if(nrow(inv_species_cover) == 0) {
    return(list( 
      "inv_species_cover" = inv_species_cover,
      "inv_species_frq" = inv_species_frq))
  } else if(nrow(inv_species_cover) > 0) {
    
    inv_species_cover_mods <- veg_cleaned$site_perc_cover %>% # logistic regression with binomial distribution for invasive species cover
      mutate(is_invasive = case_when(if_any(starts_with("invasive_"), ~ .x == "T") ~ "T")) %>%
      filter(is_invasive == "T") %>% 
      ungroup() %>%
      left_join(., 
                veg_cleaned$plot_perc_cover %>% 
                  ungroup() %>% 
                  select(UnitCode, UniqueID, Year_chr, EventID) %>% 
                  group_by(UnitCode, UniqueID, Year_chr) %>% 
                  summarise(plot_count = n_distinct(EventID)),
                by = c("UnitCode", "UniqueID", "Year_chr")) %>%
      group_by(UnitCode) %>%
      mutate(year = as.numeric(Year_chr),
             year_num = as.numeric(year-min(year)),
             binom_mean_site_cover = mean_site_cover/100) %>%
      group_by(UnitCode, SciName_cor) %>%
      nest() %>%
      mutate(binom_mod = map(data, ~glm(binom_mean_site_cover ~ year_num, data = .x, weights = plot_count, family = "quasibinomial")), #https://stats.stackexchange.com/questions/623503/how-to-interpret-binomial-glm-coefficient-w-proportion-as-outcome?rq=1
             summary_mod = map(binom_mod, ~summary(.x)),
             LR_chisq = format(round(map_dbl(binom_mod, ~stats::drop1(.x, scope = ~year_num, test = "Chisq")[[4]][[2]]), 2), nsmall = 2),
             pval = map_dbl(summary_mod, ~coefficients(.x)[[8]]),
             int = map_dbl(summary_mod, ~coefficients(.x)[[1]]),
             b1 =  map_dbl(summary_mod, ~coefficients(.x)[[2]]),
             b1_se = map_dbl(summary_mod, ~coefficients(.x)[[4]]), 
             margs = map2(binom_mod, data, ~margins::margins(model = .x, data = .y, unit_ses = TRUE)),
             change_cover = map_dbl(margs, ~mean(.x$dydx_year_num)*100),
             change_cover_se = map_dbl(margs, ~mean(.x$SE_dydx_year_num)*100),
             format_p = format_pval(pval)
      )
    
    inv_species_frq_mods <- veg_cleaned$site_frq %>% # logistic regression with binomial distribution for invasive species frq
      mutate(is_invasive = case_when(if_any(starts_with("invasive_"), ~ .x == "T") ~ "T")) %>%
      filter(is_invasive == "T") %>% 
      ungroup() %>%
      left_join(., 
                veg_cleaned$plot_perc_cover %>% 
                  ungroup() %>% 
                  select(UnitCode, UniqueID, Year_chr, EventID) %>% 
                  group_by(UnitCode, UniqueID, Year_chr) %>% 
                  summarise(plot_count = n_distinct(EventID)),
                by = c("UnitCode", "UniqueID", "Year_chr")) %>%
      group_by(UnitCode) %>%
      mutate(year = as.numeric(Year_chr),
             year_num = as.numeric(year-min(year)),
             binom_mean_site_frq = frq/100) %>%
      group_by(UnitCode, SciName_cor) %>%
      nest() %>%
      mutate(binom_mod = map(data, ~glm(binom_mean_site_frq ~ year_num, data = .x, weights = plot_count, family = "quasibinomial")), #https://stats.stackexchange.com/questions/623503/how-to-interpret-binomial-glm-coefficient-w-proportion-as-outcome?rq=1
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
             format_p = format_pval(pval)
      )
    
    results <- inv_species_cover %>%
      left_join(., inv_species_frq, by = c("UnitCode", "Year_chr", "SciName_cor")) %>%
      rename(., "mean_park_frq" = mean_frq) %>%
      group_by(UnitCode, SciName_cor) %>%
      summarise(mean_cover = format_sps(mean(mean_park_cover)),
                mean_frq = format_sps(mean(mean_park_frq)),
                se_cover = format_sps(sd(mean_park_cover)/sqrt(length(mean_park_cover))),  
                se_frq = format_sps(sd(mean_park_frq)/sqrt(length(mean_park_frq)))) %>%
      mutate(mean_se_text_cover = paste0(mean_cover, " ± ", se_cover, "%"),
             mean_se_text_frq = paste0(mean_frq, " ± ", se_frq, "% of plots")) %>%
      left_join(., inv_species_cover_mods %>%
                  select(UnitCode, SciName_cor, "LR_chisq_cover" = LR_chisq, "pval_cover" = pval, "format_p_cover" = format_p, "cover_rate" = change_cover, "cover_rate_se" = change_cover_se), by = c("UnitCode", "SciName_cor")) %>%
      left_join(., inv_species_frq_mods %>%
                  select(UnitCode, SciName_cor, "LR_chisq_frq" = LR_chisq, "pval_frq" = pval, "format_p_frq" = format_p, "frq_rate" = change_frq, "frq_rate_se" = change_frq_se), by = c("UnitCode", "SciName_cor")) %>%
      ungroup() %>%
      mutate(change_cover = map(cover_rate, ~format_sps(.x)),
             change_cover_se = map(cover_rate_se, ~format_sps(.x)),
             change_cover_se_text = paste0(change_cover, " ± ", change_cover_se, "%/yr"),
             change_cover_lab_sp = if_else(pval_cover < 0.05,
                                           paste0(str_to_upper(gsub("(([A-Za-z]{2})[a-z& ]*)", "\\2", str_to_title(SciName_cor))),": ", change_cover_se_text),
                                           NA),
             change_frq = map(frq_rate, ~format_sps(.x)),
             change_frq_se = map(frq_rate_se, ~format_sps(.x)),
             change_frq_se_text  = paste0(change_frq, " ± ", change_frq_se, "% plots/yr"),
             change_frq_lab_sp = if_else(pval_frq < 0.05,
                                         paste0(str_to_upper(gsub("(([A-Za-z]{2})[a-z& ]*)", "\\2", str_to_title(SciName_cor))),": ", change_frq_se_text),
                                         NA)
      ) %>%
      mutate(SciName_cor = janitor::make_clean_names(SciName_cor, allow_dupes = TRUE)) %>%
      split(.$SciName_cor)
    
    inv_cover_results_text <- inv_species_cover_mods %>%
      mutate(SciName_cor = janitor::make_clean_names(SciName_cor, allow_dupes = TRUE)) %>%
      split(.$SciName_cor)
    
    inv_frq_results_text <- inv_species_frq_mods %>%
      mutate(SciName_cor = janitor::make_clean_names(SciName_cor, allow_dupes = TRUE)) %>%
      split(.$SciName_cor)
    
    invasive_species_text <- inv_species_cover %>%
      ungroup() %>%
      select(UnitCode, SciName_cor, is_invasive) %>%
      group_by(UnitCode) %>%
      unique() %>%
      arrange(SciName_cor) %>%
      nest() %>%
      mutate(sp_list = map(data, ~knitr::combine_words(if_else(lengths(strsplit(.x$SciName_cor, "\\W+")) > 1,
                                                               paste0("*", .x$SciName_cor, "*"),
                                                               paste0("an unidentified ", "*", .x$SciName_cor, "*", " species"))))) %>%
      ungroup() %>%
      dplyr::select(-data)
    
    return(list( 
      "inv_species_cover" = inv_species_cover,
      "inv_species_frq" = inv_species_frq,
      "inv_species_cover_mods" = inv_species_cover_mods, 
      "inv_species_frq_mods" = inv_species_frq_mods, 
      "results" = results,
      "inv_cover_results_text" = inv_cover_results_text,
      "inv_frq_results_text" = inv_frq_results_text,
      "invasive_species_text" = invasive_species_text
    ))
  }
}