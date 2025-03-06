manage_species_dfs <- function(...) {
  
  manage_species_cover <- veg_cleaned$park_perc_cover %>%
    rename("NPS_Tags" = `NPS Tags`) %>%
    filter(!is.na(NPS_Tags))
  
  manage_species_frq <- veg_cleaned$park_frq %>%
    rename("NPS_Tags" = `NPS Tags`) %>%
    filter(!is.na(NPS_Tags))
  
  # "plot_lims_x" = list(
  #   scale_x_date(limits = c(as.Date("2008-01-01"), as.Date("2018-01-01")), expand = c(.04, 0), sec.axis = dup_axis(), date_labels = "'%y"),
  #   scale_x_date(limits = c(as.Date("2008-01-01"), as.Date("2018-01-01")), expand = c(.04, 0), sec.axis = dup_axis(), date_labels = "'%y"),
  #   scale_x_date(limits = c(as.Date("2009-01-01"), as.Date("2019-01-01")), expand = c(.04, 0), sec.axis = dup_axis(), date_labels = "'%y"),
  #   scale_x_date(limits = c(as.Date("2008-01-01"), as.Date("2018-01-01")), expand = c(.04, 0), sec.axis = dup_axis(), date_labels = "'%y")
  # )
  # 
  # "plot_lims_y_cover" = list(
  #   scale_y_continuous(limits = c(0, 8.5), breaks = seq(0,8, by = 2), expand = c(.04, 0), sec.axis = dup_axis(), name = "Cover of managed\nspecies (%)"),
  #   scale_y_continuous(limits = c(0, 8.5), breaks = seq(0,8, by = 2), expand = c(.04, 0), sec.axis = dup_axis(), name = "Cover of managed\nspecies (%)"),    
  #   scale_y_continuous(limits = c(0, 12.5), breaks = seq(0,12, by = 3), expand = c(.04, 0), sec.axis = dup_axis(), name = "Cover of managed\nspecies (%)"),
  #   scale_y_continuous(limits = c(0, 6.5), breaks = seq(0,6, by = 2), expand = c(.04, 0), sec.axis = dup_axis(), name = "Cover of managed\nspecies (%)")
  # )
  # 
  # "plot_lims_y_frq" = list(
  #   scale_y_continuous(limits = c(0, 32), breaks = seq(0,30, by = 10), expand = c(.04, 0), sec.axis = dup_axis(), name = "Freq. of managed\nspecies (% of plots)"),
  #   scale_y_continuous(limits = c(0, 32), breaks = seq(0,30, by = 10), expand = c(.04, 0), sec.axis = dup_axis(), name = "Freq. of managed\nspecies (% of plots)"),
  #   scale_y_continuous(limits = c(0, 25), breaks = seq(0,27, by = 5), expand = c(.04, 0), sec.axis = dup_axis(), name = "Freq. of managed\nspecies (% of plots)"),
  #   scale_y_continuous(limits = c(0, 4.5), breaks = seq(0,4, by = 1), expand = c(.04, 0), sec.axis = dup_axis(), name = "Freq. of managed\nspecies (% of plots)")
  # )
  
  if(nrow(manage_species_cover) == 0) {
    return(list(
      "manage_species_cover" = manage_species_cover,
      "manage_species_frq" = manage_species_frq
    ))
  } else if(nrow(manage_species_cover) > 0) {
    
    manage_species_cover_mods <- veg_cleaned$site_perc_cover %>% # logistic regression with binomial distribution for NPSpecies cover
      rename("NPS_Tags" = `NPS Tags`) %>%
      filter(!is.na(NPS_Tags)) %>%  
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
             chisq_calc = map_dbl(binom_mod, ~stats::drop1(.x, scope = ~year_num, test = "Chisq")[[4]][[2]]),
             LR_chisq = format_chi(chisq_calc),
             pval = map_dbl(summary_mod, ~coefficients(.x)[[8]]),
             int = map_dbl(summary_mod, ~coefficients(.x)[[1]]),
             b1 =  map_dbl(summary_mod, ~coefficients(.x)[[2]]),
             b1_se = map_dbl(summary_mod, ~coefficients(.x)[[4]]), 
             margs = map2(binom_mod, data, ~margins::margins(model = .x, data = .y, unit_ses = TRUE)),
             change_cover = map_dbl(margs, ~mean(.x$dydx_year_num)*100),
             change_cover_se = map_dbl(margs, ~mean(.x$SE_dydx_year_num)*100),
             format_p = format_pval(pval)
      )
    
    manage_species_frq_mods <- veg_cleaned$site_frq %>% # logistic regression with binomial distribution for NPSpecies frq
      rename("NPS_Tags" = `NPS Tags`) %>%
      filter(!is.na(NPS_Tags)) %>%    
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
    
    cover_plot_betas <- manage_species_cover_mods  %>%
      mutate(
        min_date = map(data, ~as.Date(paste0(min(.x$year), "-01-01"))-(0.4*365.25)),
        min_date_num = if_else(UnitCode == "GATE",
                               map(data, ~min(.x$year_num)-(2)),
                               map(data, ~min(.x$year_num)-(1))),
        max_date_num = map(data, ~max(.x$year_num)+(2)),
        nd = map2(min_date_num, max_date_num, ~data.frame("year_num" = seq(.x, .y, by = 0.5))),
        pred = map2(binom_mod, nd, ~data.frame(bind_cols(nd, "cover" = predict(.x, newdata = .y, type = "response"))))
      ) %>%
      select(UnitCode, SciName_cor, pred, min_date, min_date_num, max_date_num) %>%
      unnest(cols = c(pred, min_date, min_date_num, max_date_num)) %>%
      mutate(Year = as.Date(min_date) + (year_num*365.25),
             cover = cover * 100)
    
    cover_plot_betas <- manage_species_cover_mods  %>%
      filter(pval < 0.05) %>%
      mutate(
        min_date = map(data, ~as.Date(paste0(min(.x$year), "-01-01"))-(0.4*365.25)),
        min_date_num = map(data, ~min(.x$year_num)-(1)),
        max_date_num = map(data, ~max(.x$year_num)+(2)),
        nd = map2(min_date_num, max_date_num, ~data.frame("year_num" = seq(.x, .y, by = 0.5))),
        pred = map2(binom_mod, nd, ~data.frame(bind_cols(nd, "cover" = predict(.x, newdata = .y, type = "response"))))
      ) %>%
      select(UnitCode, SciName_cor, pred, min_date, min_date_num, max_date_num) %>%
      unnest(cols = c(pred, min_date, min_date_num, max_date_num)) %>%
      mutate(Year = as.Date(min_date) + (year_num*365.25),
             cover = cover * 100)
    
    frq_plot_betas <- manage_species_frq_mods %>% 
      mutate(min_date = map(data, ~as.Date(paste0(min(.x$year), "-01-01"))-(0.4*365.25)),
             min_date_num = if_else(UnitCode == "GATE",
                                    map(data, ~min(.x$year_num)-(2)), 
                                    map(data, ~min(.x$year_num)-(1))),
             max_date_num = map(data, ~max(.x$year_num)+(2)),
             nd = map2(min_date_num, max_date_num, ~data.frame("year_num" = seq(.x, .y, by = 0.5))),
             pred = map2(binom_mod, nd, ~data.frame(bind_cols(nd, "frq" = predict(.x, newdata = .y, type = "response"))))
      ) %>%
      select(UnitCode, SciName_cor, pred, min_date, min_date_num, max_date_num) %>%
      unnest(cols = c(pred, min_date, min_date_num, max_date_num)) %>%
      mutate(Year = as.Date(min_date) + (year_num*365.25),
             frq = frq * 100)
    
    results <- manage_species_cover %>%
      left_join(., manage_species_frq, by = c("UnitCode", "Year_chr", "SciName_cor")) %>%
      rename(., "mean_park_frq" = mean_frq) %>%
      group_by(UnitCode, SciName_cor) %>%
      summarise(mean_cover = format_sps(mean(mean_park_cover)),
                mean_frq = format_sps(mean(mean_park_frq)),
                se_cover = format_sps(sd(mean_park_cover)/sqrt(length(mean_park_cover))),  
                se_frq = format_sps(sd(mean_park_frq)/sqrt(length(mean_park_frq)))) %>%
      mutate(mean_se_text_cover = paste0(mean_cover, " ± ", se_cover, "%"),
             mean_se_text_frq = paste0(mean_frq, " ± ", se_frq, "% of plots")) %>%
      left_join(., manage_species$manage_species_cover_mods %>%
                  select(UnitCode, SciName_cor, "LR_chisq_cover" = LR_chisq, "pval_cover" = pval, "format_p_cover" = format_p, "cover_rate" = change_cover, "cover_rate_se" = change_cover_se), by = c("UnitCode", "SciName_cor")) %>%
      left_join(., manage_species$manage_species_frq_mods %>%
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
    
    manage_species_text <- manage_species_cover %>%
      ungroup() %>%
      select(UnitCode, SciName_cor, NPS_Tags) %>%
      group_by(UnitCode, NPS_Tags) %>%
      unique() %>%
      arrange(SciName_cor) %>%
      nest() %>%
      mutate(sp_list = map(data, ~knitr::combine_words(if_else(lengths(strsplit(.x$SciName_cor, "\\W+")) > 1,
                                                               paste0("*", .x$SciName_cor, "*"),
                                                               paste0("an unidentified ", "*", .x$SciName_cor, "*", " species"))))) %>%
      ungroup() %>%
      dplyr::select(-data)
  }
}