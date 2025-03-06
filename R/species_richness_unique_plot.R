species_richness_unique_plot <- function(y_expansion = 0.4){
  
  #ggdraw(
  species_richness$park_species_richness_per_yr %>%
    left_join(., unique_species$park_unique_species_count_per_year, by = c("UnitCode", "Year_chr")) %>%
    pivot_longer(., cols = c(total_richness, n_unique), names_to = "richness_metric", values_to = "richness") %>%
    mutate(Year = as.Date(paste0(Year_chr, "-01-01")),
           richness = if_else(is.na(richness), 0, richness),
           se = if_else(richness_metric == "total_richness", se_richness, se_n_unique)) %>%
    ungroup() %>%
    ggplot(., aes(x = Year, y = richness, group = richness_metric)) +
    {if
      (species_richness$species_richness_mods$pval < 0.05)
      geom_line(data = species_richness$richness_plot_betas, aes(x = Year, y = site_richness), color = "black", size = 0.7)} +
    {if
      (unique_species$unique_species_mods$pval < 0.05)
      geom_line(data = unique_species$unique_plot_betas, aes(x = Year, y = site_n_unique), color = "black", size = 0.7)} +
    geom_errorbar(aes(x = Year, ymin = richness - se, ymax = richness + se), color = "black") +
    geom_line(show.legend = F) +
    geom_point(aes(shape = richness_metric), show.legend = T, size = 2, color = "black", fill = "white") +
    
    
    {if (species_richness$species_richness_mods$pval > 0.05)
      geom_text(data = species_richness$results %>%
                  mutate(f = pmap_chr(list("Species richness: ", format_p), function(a, b) deparse(bquote(plain(.(a)~italic(.(b))))))),
                aes(x = structure(-Inf, class = "Date"), y = Inf, label = f),
                hjust = -0.05, vjust = 5.5, size = 3.5, family = "serif", fontface = "italic", parse = TRUE, inherit.aes = F)} +
    
    {if (unique_species$unique_species_mods$pval > 0.05)
      geom_text(data = unique_species$results %>%
                  mutate(f = pmap_chr(list("Unique species: ", format_p), function(a, b) deparse(bquote(plain(.(a)~italic(.(b))))))),
                aes(x = structure(-Inf, class = "Date"), y = Inf, label = f),
                hjust = -0.05, vjust = 10.15, size = 3.5, family = "serif", fontface = "italic", parse = TRUE, inherit.aes = F)} +
    
    {if (species_richness$species_richness_mods$pval < 0.05)
      geom_text(data = species_richness$results,
                aes(x = structure(-Inf, class = "Date"), y = Inf, label = change_count_lab),
                hjust = -0.05, vjust = 3.95, size = 3.5, family = "serif", inherit.aes = F)} +
    {if (species_richness$species_richness_mods$pval < 0.05)
      geom_text(data = species_richness$results,
                aes(x = structure(-Inf, class = "Date"), y = Inf, label = format_p),
                hjust = -0.05, vjust = 5.5, size = 3.5, family = "serif", fontface = "italic", inherit.aes = F)} +
    
    {if (unique_species$unique_species_mods$pval < 0.05)
      geom_text(data = unique_species$results,
                aes(x = structure(-Inf, class = "Date"), y = Inf, label = change_count_lab),
                hjust = -0.05, vjust = 8.6, size = 3.5, family = "serif", inherit.aes = F)} +
    {if (unique_species$unique_species_mods$pval < 0.05)
      geom_text(data = unique_species$results,
                aes(x = structure(-Inf, class = "Date"), y = Inf, label = format_p),
                hjust = -0.05, vjust = 10.15, size = 3.5, family = "serif", fontface = "italic", inherit.aes = F)} +
    
    scale_shape_manual(values = c(21, 23), breaks = c("total_richness", "n_unique"), labels = c("Total park-level species richness   ", "Unique species count")) +
    {if 
      (species_richness$results$pval < 0.05 | unique_species$results$pval < 0.05)
      scale_x_date(expand = c(0, 0), sec.axis = dup_axis(), date_labels = "'%y")
      else 
        scale_x_date(expand = c(.04, 0), sec.axis = dup_axis(), date_labels = "'%y")} +
    scale_y_continuous(expand = expansion(mult = c(0.05, y_expansion)), limits = c(0,NA), name = "Park-level species richness (n)", labels = function(x) sprintf("%0.0f", x), sec.axis = dup_axis()) +
    lfeheR::theme(base_size = 12) +
    theme(
      text = element_text(family = "serif", size = 12),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key = element_blank(),
      legend.margin = margin(t = -10, unit = "pt"),
      axis.title.y = element_text(margin = margin(r = 10, unit = "pt")),
      panel.grid.major = element_line(color = "grey", linewidth = 0.5, linetype = "dashed"),
      strip.text = element_blank()
    )#,
  #ylim = c(-0.2,1)) +
  #draw_label(bquote(bold("Figure 3.")~"Total park-level species richness (circles) and count of species unique to each park"), x = 0.02, y = 0.04, fontfamily = "serif", hjust = 0, size = 12) +
  #draw_label(bquote("(diamonds). Black lines in panels D and E represent the rate of change in total park-level species"), x = 0.02, y = -0.01, fontfamily = "serif", hjust = 0, size = 12) +
  #draw_label(bquote("richness and the count of unique species at GATE and GEWA, respectively. The rate of change in"), fontfamily = "serif", x = 0.02, y = -0.06, size = 12, hjust = 0) +
  #draw_label(bquote("total park-level species richness and count of unique species was not significant (i.e., rate of change ="), fontfamily = "serif", x = 0.02, y = -0.11, size = 12, hjust = 0) +
  #draw_label(bquote("0 % sp/yr) at ASIS, COLO, FIIS, and SAHI."), fontfamily = "serif", x = 0.02, y = -0.16, size = 12, hjust = 0)
}