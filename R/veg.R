library(tidyverse)
library(taxize)
library(readxl)
library(TNRS)

load(here::here("data", "derived", "nekton_veg.rda"))

algae <- read.csv(here::here("data", "corrected_tsn.csv")) %>%
  filter(other == "algae") %>%
  pull(ScientificName)

families <- read.csv(here::here("data", "corrected_tsn.csv")) %>%
  filter(other == "family") %>%
  pull(ScientificName)

plot_veg_cover_raw <- veg %>%
  filter(TSN > 0 & TSN != 99999) %>%
  # mark algae species and species recorded as family names
  mutate(
    ScientificName = case_when(
      ScientificName %in% c(algae) ~ "Algae",
      ScientificName %in% c(families) ~ "family name",
      T ~ ScientificName),
  ) %>%
  # filter out algae and family names
  filter(ScientificName != "Algae" & ScientificName != "family name" & ScientificName != "Ruppia maritima") %>%
  filter(Alive == 1) %>% # filter to only alive since alive vs. dead was not recorded consistently - in some years they recorded the same value for a single species as both alive and dead - see EventID 70019 as an example
  # filter out CACO because sites were not monitored consistently over the entire study period
  filter(UnitCode!= "CACO") %>%
  left_join(., read.csv(here::here("data", "corrected_tsn.csv")), by = "ScientificName", relationship = "many-to-one") %>%
  select(-other) %>%
  mutate(TSN = if_else(!is.na(corrected_tsn), corrected_tsn, TSN),
         Year_chr = as.character(Year)) %>%
  group_by(EventID, UnitCode, SubunitCode, UniqueID, Year_chr) %>%
  mutate(total_percent_cover = sum(PercentCover),
         Date = as.Date(as.character.POSIXt(StartDate, tz = "GMT"))) %>%
  select(EventID, UniqueID, UnitCode, SubunitCode, Year, Year_chr, Date, TSN,
         ScientificName, PercentCover, total_percent_cover) %>%
  distinct()

site_veg_frq_raw <- veg %>%
  filter(TSN > 0 & TSN != 99999) %>%
  # mark algae species and species recorded as family names
  mutate(
    ScientificName = case_when(
      ScientificName %in% c(algae) ~ "Algae",
      ScientificName %in% c(families) ~ "family name",
      T ~ ScientificName),
  ) %>%
  # filter out algae and family names
  filter(ScientificName != "Algae" & ScientificName != "family name" & ScientificName != "Ruppia maritima") %>%
  filter(Alive == 1) %>% # filter to only alive since alive vs. dead was not recorded consistently - in some years they recorded the same value for a single species as both alive and dead - see EventID 70019 as an example
  # filter out CACO because sites were not monitored consistently over the entire study period
  filter(UnitCode!= "CACO") %>%
  left_join(., read.csv(here::here("data", "corrected_tsn.csv")), by = "ScientificName", relationship = "many-to-one") %>%
  select(-other) %>%
  mutate(TSN = if_else(!is.na(corrected_tsn), corrected_tsn, TSN),
         Year_chr = as.character(Year)) %>%
  group_by(UnitCode, SubunitCode, UniqueID, Year_chr) %>%
  mutate(total_plots = n_distinct(EventID)) %>%
  group_by(UnitCode, SubunitCode, UniqueID, Year_chr, ScientificName, TSN) %>%
  mutate(plot_count = n(),
         frq = (plot_count/total_plots)) %>%
  select(-c(EventID, LocationID, StartDate, StartTime, Flowering, LocID2, PercentCover, Alive)) %>%
  distinct() %>%
  select(UniqueID, UnitCode, SubunitCode, Year, Year_chr, TSN, ScientificName, total_plots, plot_count, frq)

join_usda_data <- function(x) { 
  
  usda_data <- read.csv(here::here("data", "usdaplants.csv")) %>%
  filter(X != 862) %>%
  select(-c(X, betydb.species.id, Genus, Species, created_at, updated_at)) %>% # remove these columns to prevent duplicates from joining with cover data
  distinct() %>%
  #mutate(ITIS_TSN = as.character(ITIS_TSN)) %>%
  mutate(ITIS_TSN = case_when( # correct these specific TSNs so that they will match with the veg data
    ScientificName == "Eupatoriadelphus dubius" ~ 780331,
    ScientificName == "Eupatoriadelphus maculatus var. bruneri" ~ 80333,
    ScientificName == "Euthamia caroliniana" ~ 513470,
    ScientificName == "Rhus typhina" ~ 28777,
    T ~ ITIS_TSN
  ))
  
  df <- x %>%
  left_join(., usda_data, by = c("TSN"="ITIS_TSN"), relationship = "many-to-one") %>%
  rename("SciName_obs" = "ScientificName.x", "SciName_cor" = "ScientificName.y") %>%
  
  # correct growth habit for Phrag
  mutate(
    GrowthHabit = if_else(
      SciName_obs == "Phragmites australis", "Graminoid", GrowthHabit),
  ) %>%
  
  # find species present in ASIS, COLO, GEWA that are invasive in VA
  left_join(., readxl::read_excel(here::here("data", "invasive species lists", "invasives_va.xlsx")) %>%
              select("Scientific Name", "VA Invasiveness Rank"), 
            by = c("SciName_cor" = "Scientific Name"),
            relationship = "many-to-one") %>%
  mutate(invasive_asis_va = if_else(UnitCode == "ASIS" & !is.na(`VA Invasiveness Rank`), "T", NA),
         invasive_colo = if_else(UnitCode == "COLO" & !is.na(`VA Invasiveness Rank`), "T", NA),
         invasive_gewa = if_else(UnitCode == "GEWA" & !is.na(`VA Invasiveness Rank`), "T", NA)) %>%
  select(-c("VA Invasiveness Rank")) %>%
  
  # find species present in ASIS that are invasive in MD
  left_join(., readxl::read_excel(here::here("data", "invasive species lists", "invasives_md.xlsx")) %>%
              select("Accepted Symbol", "state"), by = c("AcceptedSymbol" = "Accepted Symbol"),
            relationship = "many-to-one") %>%
  mutate(invasive_asis_md = if_else(UnitCode == "ASIS" & !is.na(state), "T", NA)) %>%
  select(-state) %>%
  
  # find species present in GATE that are invasive in NJ
  left_join(., read.csv(here::here("data", "invasive species lists", "invasives_nj.csv")) %>%
              select("Accepted.Symbol", "Invasive.Noxious.Status"), by = c("AcceptedSymbol" = "Accepted.Symbol"),
            relationship = "many-to-one") %>%
  mutate(invasive_gate_nj = if_else(UnitCode == "GATE" & !is.na(Invasive.Noxious.Status), "T", NA)) %>%
  select(-c(Invasive.Noxious.Status)) %>%
  
  # find species present in GATE, FIIS, SAHI that are invasive in NY
  left_join(., read.csv(here::here("data", "invasive species lists", "invasives_ny.csv")) %>%
              select("Accepted.Symbol", "state"), by = c("AcceptedSymbol" = "Accepted.Symbol"),
            relationship = "many-to-one") %>%
  mutate(invasive_gate_ny = if_else(UnitCode == "GATE" & !is.na(state), "T", NA),
         invasive_fiis = if_else(UnitCode == "FIIS" & !is.na(state), "T", NA),
         invasive_sahi = if_else(UnitCode == "SAHI" & !is.na(state), "T", NA)) %>%
  select(-state) %>%
  
  # find species present in CACO that are invasive in MA
  left_join(., readxl::read_excel(here::here("data", "invasive species lists", "invasives_ma.xlsx")) %>%
              select("Species", "Category"),
            by = c("SciName_cor" = "Species"),
            relationship = "many-to-one") %>%
  mutate(invasive_caco = if_else(UnitCode == "CACO" & !is.na(Category.y), "T", NA)) %>%
  select(-Category.y) %>%
  
  # find invasive species identified in Kate Miller's 2020 paper
  left_join(., read_csv(here::here("data", "invasive species lists", "Miller_et_al_2020_Invasive_List.csv"), name_repair = "universal_quiet", show_col_types = FALSE) %>%
              select(Latin_Name, Accepted.Name, TSN),
            by = c("SciName_cor" = "Latin_Name"), relationship = "many-to-one") %>%
  # left_join(., read_csv(here::here("data", "invasive species lists", "Miller_et_al_2020_Invasive_List.csv"), name_repair = "universal_quiet", show_col_types = FALSE) %>%
  #             select(Latin_Name, Accepted.Name, TSN),
  #           by = c("SciName_cor" = "Accepted.Name"), relationship = "many-to-one") %>%
  left_join(., read_csv(here::here("data", "invasive species lists", "Miller_et_al_2020_Invasive_List.csv"), name_repair = "universal_quiet", show_col_types = FALSE) %>%
              select(Latin_Name, Accepted.Name, TSN),
            by = c("SciName_obs" = "Latin_Name")) %>%
  # left_join(., read_csv(here::here("data", "invasive species lists", "Miller_et_al_2020_Invasive_List.csv"), name_repair = "universal_quiet") %>%
  #             select("Latin_Name", "Accepted.Name", "TSN"),
  #           by = c("SciName_obs" = "Accepted.Name")) %>%
  # left_join(., read_csv(here::here("data", "invasive species lists", "Miller_et_al_2020_Invasive_List.csv"), name_repair = "universal_quiet") %>%
  #             select("Latin_Name", "Accepted.Name", "TSN"),
  #           by = c("TSN" = "TSN")) %>%
  #mutate(NER_invasive = if_else(!is.na(Latin_Name), "T", NA)) %>%
  #select(-c(Latin_Name, Accepted.Name, TSN)) %>%
    
  # find species that are TE at the federal level
  mutate(fed_TE = if_else(Federal_TE_Status %in% c("Endangered","Threatened"), "T", NA)) %>%
  
  # find species present in ASIS, COLO, GEWA that are rare in VA
  left_join(., readxl::read_excel(here::here("data", "rare species lists", "rare_va.xlsx")) %>%
              select(scientific_name, state_status), 
            by = c("SciName_cor" = "scientific_name"),
            relationship = "many-to-one") %>%
  mutate(rare_asis_va = if_else(UnitCode == "ASIS" & !is.na(state_status), state_status, NA),
         rare_colo = if_else(UnitCode == "COLO" & !is.na(state_status), state_status, NA),
         rare_gewa = if_else(UnitCode == "GEWA" & !is.na(state_status), state_status, NA)) %>%
  select(-state_status) %>%
  
  # find species present in ASIS that are rare in MD
  left_join(., readxl::read_excel(here::here("data", "rare species lists", "rare_md.xlsx")) %>%
              select(scientific_name, state_status), 
            by = c("SciName_cor" = "scientific_name"),
            relationship = "many-to-one") %>%
  mutate(rare_asis_md = if_else(UnitCode == "ASIS" & !is.na(state_status), state_status, NA)) %>%
  select(-state_status) %>%
  
  #find species present in GATE that are rare in NJ
  left_join(., readxl::read_excel(here::here("data", "rare species lists", "rare_nj.xlsx")) %>%
              select(accepted_symbol, state), 
            by = c("AcceptedSymbol" = "accepted_symbol"),
            relationship = "many-to-one") %>%
  mutate(rare_gate_nj = if_else(UnitCode == "GATE" & !is.na(state), "T", NA)) %>%
  select(-state) %>%
  
  # find species present in GATE, FIIS, SAHI that are rare in NY
  left_join(., readxl::read_excel(here::here("data", "rare species lists", "rare_ny.xlsx")) %>%
              select(scientific_name, state_status), 
            by = c("SciName_cor" = "scientific_name"),
            relationship = "many-to-one") %>%
  mutate(rare_gate_ny = if_else(UnitCode == "GATE" & !is.na(state_status), state_status, NA),
         rare_fiis = if_else(UnitCode == "FIIS" & !is.na(state_status), state_status, NA),
         rare_sahi = if_else(UnitCode == "SAHI" & !is.na(state_status), state_status, NA)) %>%
  select(-state_status) %>%
  
  # find species present in CACO that are rare in MA
  left_join(., readxl::read_excel(here::here("data", "rare species lists", "rare_ma.xlsx")) %>%
              select(scientific_name, state_status),
            by = c("SciName_cor" = "scientific_name"),
            relationship = "many-to-one") %>%
  mutate(rare_ma = if_else(UnitCode == "CACO" & !is.na(state_status), state_status, NA)) %>%
  select(-state_status) %>%
  
  # find wetland indicator species for the NE region
  mutate(wetland_status = str_extract(
    str_extract(string = RegionalWetlandIndicatorStatus, pattern = "(?<=1).*(?=2)"), pattern = "[A-Z]+")) %>%
  left_join(., read.csv(here::here("data", "additional_wetland_species.csv")), by = c("AcceptedSymbol"), relationship = "many-to-one") %>%
  mutate(wetland_status = if_else(is.na(wetland_status) & !is.na(status), status, wetland_status)) %>% # correct these species that were labeled with the status "NI"
  mutate(wetland_status = case_when(SciName_obs == "Agrostis gigantea" ~ "FACW",
                                    SciName_obs == "Rhus copallinum" ~ "UPL",
                                    T ~ wetland_status)) %>%
  
  # join salinity code, nativity, and community columns back in to DF
  left_join(., veg %>% select(ScientificName, SalinityCode, Nativity, Community_Class) %>% distinct(),
            by = c("SciName_obs" = "ScientificName"),
            relationship = "many-to-one") %>%
  
  # fix species with missing growth habit    
  left_join(., read.csv(here::here("data", "corrected_growth_habit.csv")), by = c("SciName_cor"), 
            relationship = "many-to-one") %>%
  mutate(GrowthHabit = if_else(!is.na(corrected_GrowthHabit), corrected_GrowthHabit, GrowthHabit))
}

# Join USDA plants data to % percent cover data
plot_veg_cover <- join_usda_data(plot_veg_cover_raw) #%>%
  select(EventID, UniqueID, UnitCode, SubunitCode, Year_chr, Date, SciName_cor, SciName_obs, TSN, PercentCover,
         total_percent_cover, 
         CommonName, AcceptedSymbol, State, Community_Class, Duration, GrowthHabit, SalinityCode, 
         SalinityTolerance, fed_TE, Nativity, starts_with("invasive_"), starts_with("rare"), wetland_status) %>%
  distinct()

# Join USDA plants data to frq data
site_veg_frq <- join_usda_data(site_veg_frq_raw) %>%
  select(UniqueID, UnitCode, SubunitCode, Year_chr, SciName_cor, SciName_obs, TSN, total_plots, plot_count,
         frq, CommonName, AcceptedSymbol, State, Community_Class, Duration, GrowthHabit, SalinityCode, 
         SalinityTolerance, fed_TE, Nativity, starts_with("invasive_"), starts_with("rare"), wetland_status) %>%
  distinct()

# Join NPspecies lists with veg data for identification of species with management concerns or T&E
## Get updated names & TSNs with TNRS package
data_species_resolve <- plot_veg_cover %>%
  ungroup() %>%
  select(SciName_cor) %>%
  distinct(SciName_cor) %>%
  mutate(ID = row_number()) %>%
  select(ID, "taxon" = SciName_cor) %>%
  TNRS(taxonomic_names = .) %>%
  select("data_ID" = ID, 
         "data_Name_submitted" = Name_submitted, 
         "data_Taxonomic_status" = Taxonomic_status, 
         "data_Accepted_species" = Accepted_species, 
         "data_Overall_score" = Overall_score, 
         "data_Name_matched_id" = Name_matched_id, 
         "data_Accepted_name_id" = Accepted_name_id) %>%
  filter(data_Accepted_species != "" & data_Taxonomic_status != "Illegitimate")  # filter out genus-level entries 
 
## Get updated names & TSNs for NPSpecies list 
# npspecies <- readxl::read_xlsx(here::here("data", "NPSpecies", "NPSpecies_Combined_List.xlsx")) %>% 
#   slice(4001:4565) %>%
#   select("Scientific Name") %>%
#   distinct(`Scientific Name`) %>%
#   mutate(ID = row_number()) %>%
#   select(ID, "taxon" = `Scientific Name`) %>%
#   TNRS(taxonomic_names = .) %>%
#   select(ID, Name_submitted, Taxonomic_status, Accepted_species, Overall_score, Name_matched_id, Accepted_name_id)
# save(npspecies, file = "./data/NPSpecies/NPSpecies_4001_4565.rda")

# Get NPS species resolved names and join with species list from veg data
npspecies_resolve <- list.files("C:/Users/lfeher/OneDrive - DOI/Network Review/salt marsh veg nekton analysis/data/NPSpecies",
                                pattern = ".rda", recursive = TRUE, full.names = TRUE) %>%
  map_dfr(~get(load(.))) %>%
  select("nps_ID" = ID,
         "nps_Name_submitted" = Name_submitted, 
         "nps_Taxonomic_status" = Taxonomic_status,
         "nps_Accepted_species" = Accepted_species,
         "nps_Overall_score" = Overall_score, 
         "nps_Name_matched_id" = Name_matched_id,
         "nps_Accepted_name_id" = Accepted_name_id) %>%
  distinct(nps_Taxonomic_status, nps_Accepted_species, nps_Overall_score, nps_Name_matched_id, nps_Accepted_name_id, .keep_all = TRUE) %>%
  mutate(genus_only = str_detect(nps_Name_submitted, " ", negate = TRUE)) %>% # filter out genus-level entries 
  filter(genus_only == FALSE & nps_Taxonomic_status != "Illegitimate" & nps_Taxonomic_status != "No opinion") %>%
  left_join(., readxl::read_xlsx(here::here("data", "NPSpecies", "NPSpecies_Combined_List.xlsx")) %>%
              select(`Park Code`, `Scientific Name`, `NPS Tags`, `T&E`, `State Status`, GRank, SRank) %>%
              distinct(),
            by = c("nps_Name_submitted" = "Scientific Name"),
            relationship = "one-to-many") %>%
  # limit NPspecies to species with management tags, T&E, or state T&E 
  filter(!is.na(`NPS Tags`) | `T&E` %in% c("E", "SC", "T", "UR") | !is.na(`State Status`)) %>% 
  nest_join(data_species_resolve, ., by = c("data_Accepted_species" = "nps_Accepted_species"), name = "npspecies", keep = TRUE) %>%
  mutate(npspecies_matches = map_dbl(npspecies, nrow)) %>%
  filter(npspecies_matches > 0) %>%
  # Check if any of the TE species from NPSpecies are currently listed
  left_join(., read.csv(here::here("data", "fed_te_species_Dec2024.csv")), by = c("data_Accepted_species" = "Scientific.Name")) %>%
  unnest(cols = c(npspecies), keep_empty = TRUE)

# Join % cover data to NPspecies lists
plot_veg_cover <- plot_veg_cover %>%
  left_join(., npspecies_resolve, by = c("SciName_cor" = "data_Name_submitted", "UnitCode" = "Park Code"), keep = TRUE, relationship = "many-to-one") %>%
  select(-c(data_ID, data_Taxonomic_status, data_Overall_score, data_Name_matched_id, data_Accepted_name_id,
            nps_ID, nps_Taxonomic_status, nps_Overall_score, nps_Name_matched_id, nps_Accepted_name_id,
            genus_only, npspecies_matches, Common.Name, Family, Species.Group, Where.Listed))

# Join frq data to NPspecies lists
site_veg_frq <- site_veg_frq %>%
  left_join(., npspecies_resolve, by = c("SciName_cor" = "data_Name_submitted", "UnitCode" = "Park Code"), keep = TRUE) %>%
  select(-c(data_ID, data_Taxonomic_status, data_Overall_score, data_Name_matched_id, data_Accepted_name_id,
            nps_ID, nps_Taxonomic_status, nps_Overall_score, nps_Name_matched_id, nps_Accepted_name_id,
            genus_only, npspecies_matches, Common.Name, Family, Species.Group, Where.Listed)) %>%
  mutate(frq = frq*100) # covert frequency to % of plots

# pivot wider % cover data to add 0s for species found in a site that weren't found in each plot - needed for calculating %cover according to protocol PIP
plot_perc_cover_old <- plot_veg_cover %>%
  ungroup() %>%
  select(EventID, UniqueID, UnitCode, Year_chr, SciName_cor, PercentCover) %>% 
  pivot_wider(., names_from = SciName_cor, values_from = PercentCover, values_fill = 0) %>% 
  pivot_longer(., cols = -c("EventID", "UniqueID", "UnitCode", "Year_chr"), names_to = "SciName_cor", values_to = "PercentCover") %>%
  left_join(., plot_veg_cover, by = c("EventID", "UniqueID", "UnitCode", "Year_chr", "SciName_cor", "PercentCover")) %>% # rejoin other data columns from USDA & NPSpecies
  group_by(UniqueID, UnitCode, Year_chr, SciName_cor) %>%
  mutate(total_species_site_cover = sum(PercentCover)) %>%
  filter(total_species_site_cover > 0)  # filter out species that weren't found at any plots within each site

site_perc_cover_old <- plot_perc_cover_old %>% # summarise %cover to site-level
  group_by(UniqueID, UnitCode, Year_chr, SciName_cor) %>%
  summarise(mean_site_cover = mean(PercentCover, na.rm = TRUE),
            se_site_cover = sd(PercentCover, na.rm = TRUE)/sqrt(length(PercentCover))) %>%
  left_join(., plot_veg_cover %>% # rejoin other data columns from USDA & NPSpecies
              ungroup() %>%
              select(-c(EventID, SubunitCode, PercentCover, Date, total_percent_cover)) %>%
              distinct(),
            by = c("UniqueID", "UnitCode", "Year_chr", "SciName_cor"),
            relationship = "many-to-one")

plot_perc_cover <- plot_veg_cover %>%
  ungroup() %>%
  select(EventID, UniqueID, UnitCode, Year_chr, SciName_cor, PercentCover) %>% 
  pivot_wider(., names_from = SciName_cor, values_from = PercentCover, values_fill = 0) %>% 
  pivot_longer(., cols = -c("EventID", "UniqueID", "UnitCode", "Year_chr"), names_to = "SciName_cor", values_to = "PercentCover") %>%
  #group_by(UnitCode, SciName_cor) %>%
  group_by(UnitCode, UniqueID, SciName_cor) %>%
  mutate(total_species_site_cover = sum(PercentCover, na.rm = TRUE)) %>%
  left_join(., plot_veg_cover, by = c("EventID", "UniqueID", "UnitCode", "Year_chr", "SciName_cor", "PercentCover")) %>% # rejoin other data columns from USDA & NPSpecies
  filter(total_species_site_cover != 0) %>%
  mutate(`State Status` = if_else(SciName_cor == "Myrica gale", "NJ: R", `State Status`))

site_perc_cover <- plot_perc_cover %>% # summarise %cover to site-level
  group_by(UniqueID, UnitCode, Year_chr, SciName_cor) %>%
  summarise(mean_site_cover = mean(PercentCover, na.rm = TRUE),
            se_site_cover = sd(PercentCover, na.rm = TRUE)/sqrt(length(PercentCover))) %>%
  left_join(., plot_veg_cover %>% # rejoin other data columns from USDA & NPSpecies
              ungroup() %>%
              select(-c(EventID, SciName_obs, UniqueID, Year_chr, SubunitCode, PercentCover, Date, total_percent_cover)) %>%
              distinct(),
            by = c("UnitCode", "SciName_cor"),
            relationship = "many-to-one")  %>%
  mutate(`State Status` = if_else(SciName_cor == "Myrica gale", "NJ: R", `State Status`))
 
# site_rel_cover <- plot_perc_cover %>% # Decided not to use relative cover b/c it results in a loss of info 
#   group_by(UniqueID, UnitCode, Year_chr, SciName_cor) %>%
#   summarise(sum_site_species_cover = sum(PercentCover)) %>%
#   group_by(UniqueID, UnitCode, Year_chr) %>%
#   mutate(sum_site_total_cover = sum(sum_site_species_cover)) %>%
#   group_by(UniqueID, UnitCode, Year_chr, SciName_cor) %>%
#   mutate(rel_cover = (sum_site_species_cover/sum_site_total_cover)*100) %>%
#   left_join(., plot_veg_cover %>% # rejoin other data columns from USDA & NPSpecies
#               ungroup() %>%
#               select(-c(EventID, SubunitCode, PercentCover, Date, total_percent_cover)) %>%
#               distinct(),
#             by = c("UniqueID", "UnitCode", "Year_chr", "SciName_cor"),
#             relationship = "many-to-one")

park_perc_cover_old <- site_perc_cover_old  %>% # summarise %cover to park-level
  group_by(UnitCode, Year_chr, SciName_cor) %>%
  summarise(mean_park_cover = mean(mean_site_cover, na.rm = TRUE), 
            se_park_cover = sd(mean_site_cover, na.rm = TRUE)/sqrt(length(mean_site_cover))) %>%
  left_join(., plot_veg_cover %>% # rejoin other data columns from USDA & NPSpecies
              ungroup() %>%
              select(-c(EventID, UniqueID, SubunitCode, PercentCover, Date, total_percent_cover)) %>%
              distinct(),
            by = c("UnitCode", "Year_chr", "SciName_cor"),
            relationship = "many-to-one")

park_perc_cover <- site_perc_cover %>% # summarise %cover to park-level
  group_by(UnitCode, Year_chr, SciName_cor) %>%
  summarise(mean_park_cover = mean(mean_site_cover, na.rm = TRUE), 
            se_park_cover = sd(mean_site_cover, na.rm = TRUE)/sqrt(length(mean_site_cover))) %>%
  left_join(., plot_veg_cover %>% # rejoin other data columns from USDA & NPSpecies
              ungroup() %>%
              select(-c(EventID, SciName_obs, UniqueID, Year_chr, SubunitCode, PercentCover, Date, total_percent_cover)) %>%
              distinct(),
            by = c("UnitCode", "SciName_cor"),
            relationship = "many-to-one")  %>%
  mutate(`State Status` = if_else(SciName_cor == "Myrica gale", "NJ: R", `State Status`))

# park_ave_rel_cover <- site_rel_cover %>%
#   group_by(UnitCode, Year_chr, SciName_cor) %>%
#   summarise(mean_rel_cover = mean(rel_cover)) %>%
#   left_join(., plot_veg_cover %>% # rejoin other data columns from USDA & NPSpecies
#               ungroup() %>%
#               select(-c(EventID, UniqueID, SubunitCode, PercentCover, Date, total_percent_cover)) %>%
#               distinct(),
#             by = c("UnitCode", "Year_chr", "SciName_cor"),
#             relationship = "many-to-one")

# pivot wider frequency data to add 0s for species found in a site that weren't found in each plot - needed for calculating frq according to protocol PIP
site_frq_old <- site_veg_frq %>% 
  ungroup() %>%
  select(UniqueID, UnitCode, Year_chr, SciName_cor, frq) %>%
  pivot_wider(., names_from = "SciName_cor", values_from = "frq", values_fill = 0) %>%
  pivot_longer(., cols = -c("UniqueID", "UnitCode", "Year_chr"), names_to = "SciName_cor", values_to = "frq") %>%
  left_join(., site_veg_frq, by = c("UniqueID", "UnitCode", "Year_chr", "SciName_cor", "frq")) %>% # rejoin other data columns from USDA & NPSpecies
  group_by(UniqueID, UnitCode, Year_chr, SciName_cor) %>%
  mutate(total_species_frq = sum(frq)) %>%
  filter(total_species_frq > 0) # filter out species that weren't found at any plots within each site

site_frq <- site_veg_frq %>% 
  ungroup() %>%
  select(UniqueID, UnitCode, Year_chr, SciName_cor, frq) %>%
  pivot_wider(., names_from = "SciName_cor", values_from = "frq", values_fill = 0) %>%
  pivot_longer(., cols = -c("UniqueID", "UnitCode", "Year_chr"), names_to = "SciName_cor", values_to = "frq") %>%
  group_by(UnitCode, SciName_cor) %>%
  mutate(total_species_frq = sum(frq)) %>%
  filter(total_species_frq != 0) %>% # filter out species that weren't found at any plots within each site
  left_join(., site_veg_frq %>%
              ungroup() %>%
              select(-c(SciName_obs, UniqueID, Year_chr, SubunitCode, total_plots, plot_count, frq)) %>%
              distinct(), by = c("UnitCode","SciName_cor")) %>% # rejoin other data columns from USDA & NPSpecies
  mutate(`State Status` = if_else(SciName_cor == "Myrica gale", "NJ: R", `State Status`))

park_frq_old <- site_frq_old %>%
  group_by(UnitCode, Year_chr, SciName_cor) %>% 
  summarise(mean_frq = mean(frq, na.rm = TRUE), # summarise frq to the park-level
            se_frq = sd(frq, na.rm = TRUE)/sqrt(length(frq))) %>%
  left_join(., site_veg_frq %>% # rejoin other data columns from USDA & NPSpecies
              ungroup() %>%
              select(-c(UniqueID, SubunitCode, frq, total_plots, plot_count)) %>%
              distinct(),
            by = c("UnitCode", "Year_chr", "SciName_cor"),
            relationship = "many-to-one")

park_frq <- site_frq %>% 
  group_by(UnitCode, Year_chr, SciName_cor) %>% 
  summarise(mean_frq = mean(frq, na.rm = TRUE), # summarise frq to the park-level
            se_frq = sd(frq, na.rm = TRUE)/sqrt(length(frq))) %>%
  left_join(., site_veg_frq %>% # rejoin other data columns from USDA & NPSpecies
              ungroup() %>%
              select(-c(UniqueID, SciName_obs, SubunitCode, Year_chr, total_plots, plot_count, frq)) %>%
              distinct(),
            by = c("UnitCode", "SciName_cor"),
            relationship = "many-to-one") %>%
  mutate(`State Status` = if_else(SciName_cor == "Myrica gale", "NJ: R", `State Status`))
  
veg_joined <- list("veg" = veg, "plot_veg_cover" = plot_veg_cover, "site_veg_frq" = site_veg_frq) # contains raw data from DB query ("veg") and raw cover and frequency data joined with USDA Plants and NPSpecies characteristics
veg_cleaned <- list("plot_perc_cover" = plot_perc_cover, "plot_perc_cover_old" = plot_perc_cover_old, "site_perc_cover" = site_perc_cover, "site_perc_cover_old" = site_perc_cover_old, "park_perc_cover" = park_perc_cover, "park_perc_cover_old" = park_perc_cover_old, "site_frq" = site_frq, "site_frq_old" = site_frq_old, "park_frq" = park_frq, "park_frq_old" = park_frq_old) # transformed cover and frequency data summarised to the plot-, site-, or park-levels

save(veg_joined, veg_cleaned, file = "./data/derived/veg_cleaned.rda")
rm(list=setdiff(ls(), c("veg_joined", "veg_cleaned")))
