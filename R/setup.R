set_flextable_defaults(
  font.size = 12,
  font.family = "times new roman",
  padding = 1
)

color_pals <- list(

  "park_pal" = c("#ef476f", "#f78c6b", "#ffd166", "#0bd59d", "#118ab2", "#073b4c"#, "#6E51AE" 
  ),
  
  "asis_pal" = monochromeR::generate_palette("#ef476f", modification = "go_lighter", n_colours = 9),
  #"caco_pal" = monochromeR::generate_palette("#f78c6b", modification = "go_lighter", n_colours = 2),
  "colo_pal" = monochromeR::generate_palette("#f78c6b", modification = "go_lighter", n_colours = 8),
  "fiis_pal" = monochromeR::generate_palette("#ffd166", modification = "go_lighter", n_colours = 9),
  "gate_pal" = monochromeR::generate_palette("#0bd59d", modification = "go_lighter", n_colours = 3),
  "gewa_pal" = "#118ab2",
  "sahi_pal" = "#073b4c"
)

color_pals[["site_pal"]] <- c(color_pals$asis_pal, #color_pals$caco_pal, 
                              color_pals$colo_pal, color_pals$fiis_pal, color_pals$gate_pal, color_pals$gewa_pal, color_pals$sahi_pal)



"study_years" = veg_cleaned$park_perc_cover %>%
  group_by(UnitCode) %>%
  summarise(min_year = min(as.numeric(Year_chr)),
            max_year = max(as.numeric(Year_chr))) 

format_pval <- function(x) {
  if(is.na(x) | is.null(x)){
    NA
  } else if(x > 0.05) {
    "p = ns"
  } else if(x <= 0.05 & x > 0.01) {
    "p < 0.05"
  } else if(x <= 0.01 & x > 0.001) {
    "p < 0.01"
  } else if(x <= 0.001) {
    "p < 0.001"
  } 
}

format_chi <- function(x) {
  if(is.na(x)){
    NA
  } else if(is.null(x)){
    NULL
  } else if(x < 0.01){
    format(x, scientific = TRUE, digits = 2)
  } else if(x > 0.01){
    as.character(format(round(x, 2), nsmall = 2))
  } 
}

format_sps <- function(x) {
  if (is.na(x)) {
    NA
  } else if (x == 0) {
    format(x, nsmall = 0)
  } else if (x < 0.01 & x > -0.01 & x != 0) {
    format(x, scientific = TRUE, digits = 1)
  } else if (x > 0.01 | x < -0.01 & x != 0) {
    sprintf("%.2f", round(x, 2))
  } else {
    "-"
  }
}

site_descriptions <- list(
  "year_site_plot_count" = veg_joined$plot_veg_cover %>%
    group_by(UnitCode) %>%
    summarise(year_count = n_distinct(Year_chr),
              site_count = n_distinct(UniqueID),
              min_year = min(as.integer(Year_chr)),
              max_year = max(as.integer(Year_chr))) %>%
    mutate(plot_count = case_when(
      UnitCode == "ASIS" ~ 450,
      UnitCode == "COLO" ~ 400,
      UnitCode == "FIIS" ~ 450,
      UnitCode == "GATE" ~ 150,
      UnitCode == "GEWA" ~ 50,
      UnitCode == "SAHI" ~ 50
    )),
  
  "site_list" = veg_joined$plot_veg_cover %>%
    group_by(UnitCode) %>%
    distinct(UniqueID) %>%
    mutate(site = sub('.+_(.+)', '\\1', UniqueID)),
  
  "site_year_list" = veg_joined$plot_veg_cover %>%
    group_by(UnitCode, Year_chr) %>%
    distinct(UniqueID) %>%
    mutate(site = sub('.+_(.+)', '\\1', UniqueID),
           site_number = as.numeric(str_extract(site, "\\d+"))) %>%
    arrange(site_number) %>%
    summarise(site_list = str_flatten_comma(site, last = ", and ")) %>%
    group_by(UnitCode, site_list) %>%
    summarise(year_list = str_flatten_comma(Year_chr, last = ", and "),
              year_name = str_flatten(Year_chr, collapse = "_")) %>%
    mutate(year_name = paste0("a", year_name)) %>%
    split(.$year_name)#,
  
  # "caco_site_list" = veg_joined$veg %>%
  #   filter(UnitCode == "CACO") %>%
  #   group_by(UnitCode) %>%
  #   distinct(UniqueID) %>%
  #   mutate(site = sub('.+_(.+)', '\\1', UniqueID))
)