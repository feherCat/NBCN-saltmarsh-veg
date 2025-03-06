total_cover_plot <- function(...){
  
  if(params$park == "ASIS") {
    site_levels <- c("A2", "A3", "A4", "A5", "A6", "A8", "A9", "A10", "A11", "Park average")
  }
  
  site_num <- n_distinct(total_cover$total_cover_site$UniqueID)
  
  plot_total_cover <- total_cover$total_cover_park %>%
    mutate(UniqueID = "Park average") %>%
    select(UniqueID, UnitCode, Year_chr, "mean_year_site_total_cover" = mean_year_total_cover, "se_year_site_total_cover" = se_year_total_cover) %>%
    bind_rows(., total_cover$total_cover_site) %>%
    mutate(Year = as.Date(paste0(Year_chr, "-01-01")),
           UniqueID = sub(".*_", "", UniqueID)) %>%
    mutate(UniqueID = factor(UniqueID, levels = site_levels))
  
  ggplot(data = plot_total_cover %>% filter(UniqueID != "Park average"), aes(x = Year, y = mean_year_site_total_cover, group = UniqueID)) +
    geom_line(data = plot_total_cover %>% filter(UniqueID == "Park average"), 
              aes(x = Year, y = mean_year_site_total_cover, size = UniqueID)) +
    geom_point(data = plot_total_cover %>% filter(UniqueID == "Park average"), 
               aes(x = Year, y = mean_year_site_total_cover, size = UniqueID), fill = "black", size = 2, shape = 24) +
    geom_line(aes(color = UniqueID)) +
    geom_point(aes(fill = UniqueID), shape = 21) +
    {if (total_cover$total_cov_mods$pval < 0.05)
      geom_line(data = total_cover$plot_betas, aes(x = Year, y = mean_year_total_cover), color = "black", linetype = "dashed", inherit.aes = FALSE, size = 0.7)
    } +
    {if (total_cover$total_cov_mods$pval < 0.05) 
      geom_text(data = total_cover$results,
                aes(x = structure(-Inf, class = "Date"), y = Inf, label = change_cover_lab),
                hjust = -0.05, vjust = 3.95, size = 3.5, inherit.aes = F)
    } +
    {if (total_cover$total_cov_mods$pval > 0.05)
      geom_text(data = total_cover$results,
                aes(x = structure(-Inf, class = "Date"), y = Inf, label = format_p),
                hjust = -0.05, vjust = 3.95, size = 3.5, fontface = "italic", inherit.aes = F)
    } +
    scale_color_manual(values = rcartocolor::carto_pal(site_num, "Vivid"), name = "Site") +
    scale_fill_manual(values = rcartocolor::carto_pal(site_num, "Vivid"), name = "Site") +
    scale_size_manual(values = c(0.75), name = "") +
    scale_y_continuous(breaks = seq(0,100, by = 20), name = "Average total plot cover (%)") +
    {if (total_cover$total_cov_mods$pval < 0.05)
      scale_x_date(expand = c(0,0), date_labels = "'%y") 
      else
        scale_x_date(expand = c(.04, 0), date_labels = "'%y")
    } +
    guides(size = guide_legend(order = 2), fill = guide_legend(order = 1), color = guide_legend(order = 1)) +
    theme(
      panel.background = element_rect(fill = NA),
      panel.border = element_rect(color = "black", fill = NA),
      text = element_text(color = "black"),
      panel.grid.major = element_line(color = "grey", linewidth = 0.25),
      panel.grid.minor = element_blank()
    )
}
