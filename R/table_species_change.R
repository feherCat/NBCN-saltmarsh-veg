table_species_change <- function(...) {
  species_change$results %>%
    list_flatten() %>%
    list_rbind() %>%
    mutate(sci_name = if_else(lengths(strsplit(SciName_cor, "\\_+")) > 1,
                              str_to_sentence(str_replace_all(SciName_cor, "_", " ")),
                              paste0(str_to_sentence(str_replace(SciName_cor, "_", " "))," sps.")),
           cover_rate = if_else(!is.na(pval_cover), as.character(unlist(change_cover)), "-"),
           frq_rate = if_else(!is.na(pval_frq), as.character(unlist(change_frq)), "-")) %>% 
    ungroup() %>%
    select(UnitCode, sci_name, mean_cover, cover_rate, mean_frq, frq_rate) %>%
    
    flextable(., col_keys = c("UnitCode", "sci_name", "mean_cover", "cover_rate", "dummy1", "mean_frq", "frq_rate")) %>%
    set_header_labels(., UnitCode = "Park", sci_name = "Species", mean_cover = "Mean\n(%)", cover_rate = "Rate of change\n(%/yr)", dummy1 = "", mean_frq = "Mean\n(% of plots)", frq_rate = "Rate of change\n(% of plots/yr)") %>%
    add_header_row(., values = c("Park", "Species", "Cover", "", "Frequency"), colwidths = c(1, 1, 2, 1, 2)) %>% 
    add_header_row(., values = paste0("Table 4. Mean percent cover and frequency of species with rates of change that were significantly different from 0 at ", params$park, "."), colwidths = 7) %>%
    merge_at(i = c(2,3), j = 1, part = "header") %>%
    merge_at(i = c(2,3), j = 2, part = "header") %>%
    merge_v(j = 1, part = "body") %>%
    align(align = "center", part = "all") %>%
    align(align = "left", part = "header", i = 1) %>%
    mk_par(j = "sci_name", value = as_paragraph(as_i(sci_name))) %>%
    color(i = ~cover_rate != "-" & cover_rate < 0, j = 4, color = "red", part = "body") %>%
    color(i = ~cover_rate != "-" & cover_rate > 0, j = 4, color = "forestgreen", part = "body") %>%
    color(i = ~frq_rate != "-" & frq_rate < 0, j = 7, color = "red", part = "body") %>%
    color(i = ~frq_rate != "-" & frq_rate > 0, j = 7, color = "forestgreen", part = "body") %>%
    width(j = 1:7, width = c(0.5, 2, 0.6, 0.9, 0.3, 0.7, 0.9)) %>%
    fontsize(size = 10, part = "all") %>%
    fix_border_issues()
}