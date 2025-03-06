total_cover_dfs <- function(...){
  total_cover_site <- veg_joined$plot_veg_cover %>%
    group_by(EventID, UniqueID, UnitCode, Year_chr) %>%
    summarise(mean_year_plot_total_cover = mean(total_percent_cover, na.rm = TRUE)) %>%
    group_by(UniqueID, UnitCode, Year_chr) %>%
    summarise(mean_year_site_total_cover = mean(mean_year_plot_total_cover, na.rm = TRUE),
              se_year_site_total_cover = sd(mean_year_plot_total_cover)/sqrt(length(mean_year_plot_total_cover)))
  
  total_cover_park <- veg_joined$plot_veg_cover %>%
    group_by(EventID, UniqueID, UnitCode, Year_chr) %>%
    summarise(mean_year_plot_total_cover = mean(total_percent_cover, na.rm = TRUE)) %>%
    group_by(UniqueID, UnitCode, Year_chr) %>%
    summarise(mean_year_site_total_cover = mean(mean_year_plot_total_cover, na.rm = TRUE),
              se_year_site_total_cover = sd(mean_year_site_total_cover)/sqrt(length(mean_year_site_total_cover))) %>%
    group_by(UnitCode, Year_chr) %>%
    summarise(mean_year_total_cover = mean(mean_year_site_total_cover, na.rm = TRUE),
              se_year_total_cover = sd(mean_year_site_total_cover)/sqrt(length(mean_year_site_total_cover)))
  
  total_cov_mods <- veg_joined$plot_veg_cover %>% # beta regression for total cover
    group_by(EventID, UniqueID, UnitCode, Year_chr) %>%
    summarise(mean_year_plot_total_cover = mean(total_percent_cover, na.rm = TRUE)) %>%
    group_by(UniqueID, UnitCode, Year_chr) %>%
    summarise(mean_year_site_total_cover = mean(mean_year_plot_total_cover, na.rm = TRUE)) %>%
    mutate(mean_year_site_total_cover = if_else(mean_year_site_total_cover > 100, 100, mean_year_site_total_cover)) %>%
    group_by(UnitCode) %>%
    mutate(year = as.numeric(Year_chr),
           year_num = as.numeric(year-min(year)),
           beta_cover = mean_year_site_total_cover/100) %>%
    filter(UnitCode != "GEWA") %>%
    nest() %>%
    mutate(beta_mod = map(data, ~betareg(beta_cover ~ year_num, data = .x, link = "loglog")), # https://www.andrewheiss.com/blog/2021/11/08/beta-regression-guide/#a-beta-regression
           null_mod = map(data, ~betareg(beta_cover ~ 1, data = .x, link = "loglog")),
           summary_null = map(null_mod, ~summary(.x)),
           summary_mod = map(beta_mod, ~summary(.x)),
           pval = map_dbl(summary_mod, ~.x$coefficients[[1]][[8]])) %>%
    mutate(int = map_dbl(summary_mod, ~.x$coefficients[[1]][[1]]),
           b1 =  map_dbl(summary_mod, ~.x$coefficients[[1]][[2]]),
           b1_se = map_dbl(summary_mod, ~.x$coefficients[[1]][[4]]), 
           change_cover = map2_dbl(int, b1, ~plogis(.x + .y) - plogis(.x))*100,
           change_cover_se = map2_dbl(int, b1_se, ~plogis(.x + .y) - plogis(.x))*100,
           r2 = map_dbl(summary_mod, ~.x$pseudo.r.squared),
           chisq_calc = map2_dbl(summary_null, summary_mod, ~ -2 * (.x$loglik - .y$loglik)),
           LR_chisq = format_chi(chisq_calc), 
           format_p = format_pval(pval)
    )
  
  plot_betas <- total_cov_mods %>%
    mutate(min_date = map(data, ~as.Date(paste0(min(.x$year), "-01-01"))-(0.4*365.25)),
           min_date_num = map(data, ~min(.x$year_num)-(1)),
           max_date_num = map(data, ~max(.x$year_num)+(2)),
           nd = map2(min_date_num, max_date_num, ~data.frame("year_num" = seq(.x, .y, by = 0.5))),
           pred = map2(beta_mod, nd, ~data.frame(bind_cols(nd, "mean_year_total_cover" = predict(.x, newdata = .y, type = "response")) %>% mutate(mean_year_total_cover = mean_year_total_cover * 100)))
    ) %>%
    select(UnitCode, pred, min_date, min_date_num, max_date_num) %>%
    unnest(cols = c(pred, min_date, min_date_num, max_date_num)) %>%
    mutate(Year = as.Date(min_date) + (year_num*365.25))
  
  results <- total_cover_park %>%
    group_by(UnitCode) %>%
    summarise(mean_total_cover = mean(mean_year_total_cover, na.rm = TRUE),
              se_total_cover = sd(mean_year_total_cover)/sqrt(length(mean_year_total_cover))) %>%
    rowwise() %>%
    mutate(mean_se_text = paste0(round(mean_total_cover, 0), "% (± ", round(se_total_cover, 0), ")")) %>%
    left_join(., total_cov_mods, by = "UnitCode") %>%
    rowwise() %>%
    mutate(change_cover_se_text = paste0(round(change_cover, 1), " ± ", round(change_cover_se, 1), "%/yr"),
           change_cover_lab = paste0("Total cover: ", round(change_cover, 1), " ± ", round(change_cover_se, 1), "%/yr"),
    ) %>%
    select(-c(data, beta_mod, null_mod, summary_mod, summary_null))
  
  return(list("total_cover_site" = total_cover_site,
              "total_cover_park" = total_cover_park, 
              "total_cov_mods" = total_cov_mods, 
              "plot_betas" = plot_betas,
              "results" = results
  ))
}
