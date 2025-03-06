total_cover_plot <- function(...){
  
  #ggdraw(
  total_cover$total_cover_df %>%
    mutate(Year = as.Date(paste0(Year_chr, "-01-01"))) %>%
    ggplot(., aes(x = Year, y = mean_year_total_cover)) +
    
    {if (total_cover$total_cov_mods$pval < 0.05)
      geom_line(data = total_cover$plot_betas, aes(x = Year, y = mean_year_total_cover), color = "black", size = 0.7)} +
    
    geom_line(show.legend = F) +
    geom_errorbar(aes(ymin = mean_year_total_cover - se_year_total_cover, ymax = mean_year_total_cover + se_year_total_cover), color = "black") +
    geom_point(fill = "white", show.legend = F, shape = 21, color = "black", size = 2) +
    
    {if (total_cover$total_cov_mods$pval < 0.05) 
      geom_text(data = total_cover$results,
                aes(x = structure(-Inf, class = "Date"), y = Inf, label = change_cover_lab),
                hjust = -0.05, vjust = 3.95, size = 3.5, family = "serif", inherit.aes = F) +
        geom_text(data = total_cover$results,
                  aes(x = structure(-Inf, class = "Date"), y = Inf, label = format_p),
                  hjust = -0.05, vjust = 5.5, size = 3.5, family = "serif", fontface = "italic", inherit.aes = F)} +
    {if (total_cover$total_cov_mods$pval > 0.05)
      geom_text(data = total_cover$results,
                aes(x = structure(-Inf, class = "Date"), y = Inf, label = format_p),
                hjust = -0.05, vjust = 3.95, size = 3.5, family = "serif", fontface = "italic", inherit.aes = F)} +
    {if (total_cover$total_cov_mods$pval < 0.05)
      scale_x_date(expand = c(0,0), sec.axis = dup_axis(), date_labels = "'%y") 
      else 
        scale_x_date(expand = c(.04, 0), sec.axis = dup_axis(), date_labels = "'%y")} +
    scale_y_continuous(limits = c(0,160), breaks = seq(0,150, by = 25), labels = c("0", "", "50", "", "100", "", "150"), name = "Average total plot cover (%)", sec.axis = dup_axis()) +
    lfeheR::theme(base_size = 12) +
    theme(
      text = element_text(family = "serif", size = 12),
      legend.position = "none",
      axis.title.y = element_text(margin = margin(r = 10, unit = "pt")),
      panel.grid.major = element_line(color = "grey", linewidth = 0.5, linetype = "dashed"),
      strip.text = element_blank()
    )#,
  #ylim = c(-0.15,1)) +
  # draw_label(bquote(bold("Figure 2.")~"Average total plot cover at"~.(params$park)*". Black lines in panels B and C represent the rate"), fontfamily = "serif", x = 0.02, y = 0.04, size = 12, hjust = 0) +
  # draw_label(bquote(plain("of change in mean total plot cover at COLO and FIIS, respectively. The rate of change in mean")), fontfamily = "serif", x = 0.02, y = -0.01, size = 12, hjust = 0) +
  # draw_label(bquote(plain("total plot cover was either not significant (i.e., rate of change = 0 %/yr) for ASIS, GATE, and SAHI,")), fontfamily = "serif", x = 0.02, y = -0.06, size = 12, hjust = 0) +
  # draw_label(bquote(plain("or, in the case of GEWA, could not be estimated since there were only 3 sampling events.")), fontfamily = "serif", x = 0.02, y = -0.11, size = 12, hjust = 0) 
}