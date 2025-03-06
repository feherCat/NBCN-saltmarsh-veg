table_sampling_scheme <- function(...){
veg_joined$plot_veg_cover %>%
  group_by(UnitCode) %>%
  summarise(site_count = n_distinct(UniqueID),
            min_year = min(as.numeric(Year_chr)),
            max_year = max(as.numeric(Year_chr)),
            sample_events = n_distinct(Year_chr),
            years_sampled = toString(unique(sort(Year_chr))),
            sites = toString(unique(sort(stri_extract(UniqueID, regex = "(?<=_).*"))))) %>% 
  mutate(full_park_name = case_when(
    UnitCode == "ASIS" ~ "Assateague Island National Seashore",
    #UnitCode == "CACO" ~ "Cape Cod National Seashore",
    UnitCode == "COLO" ~ "Colonial National Historical Park",
    UnitCode == "FIIS" ~ "Fire Island National Seashore",
    UnitCode == "GATE" ~ "Gateway National Recreation Area",
    UnitCode == "GEWA" ~ "George Washington Birthplace National Monument",
    UnitCode == "SAHI" ~ "Sagamore Hill National Historic Site"
  ),
  State = case_when(
    UnitCode == "ASIS" ~ "MD, VA",
    #UnitCode == "CACO" ~ "MA",
    UnitCode == "COLO" ~ "VA",
    UnitCode == "FIIS" ~ "NY",
    UnitCode == "GATE" ~ "NJ, NY",
    UnitCode == "GEWA" ~ "VA",
    UnitCode == "SAHI" ~ "NY"
  ),
  plot_count = case_when(
    UnitCode == "ASIS" ~ 450,
    UnitCode == "COLO" ~ 400,
    UnitCode == "FIIS" ~ 450,
    UnitCode == "GATE" ~ 150,
    UnitCode == "GEWA" ~ 50,
    UnitCode == "SAHI" ~ 50
  )) %>%
  flextable(., col_keys = c("full_park_name", "UnitCode", "State", "site_count", "sites", "plot_count", "years_sampled", "sample_events")) %>%
  align(., align = "center", part = "all") %>%
  set_table_properties(., width = 1, layout = "autofit") %>%
  add_header_lines(., values = c("Table 1. Site count, plot count, monitoring years, and count of sampling events at each park utilizing the NCBN salt marsh vegetation monitoring protocol.")) %>%
  set_header_labels(., full_park_name = "Park", UnitCode = "Unit Code", State = "Location", site_count = "Number of Sites", sites = "Site List", plot_count = "Number of Plots", years_sampled = "Sample Years", sample_events = "Sampling Events") %>%
  align(., i = 1, align = "left", part = "header") %>%
  border_inner_h(., border = fp_border(color = "transparent"), part = "body") %>%
  padding(., part = "body", padding.top = 3, padding.bottom = 3)
}