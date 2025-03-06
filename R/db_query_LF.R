## author: C. Peck
## description: recreate SQL queries in R

## ----------------------------------------------------------------------------
## libraries ----
library(here)
library(dplyr)
library(lubridate)
library(Hmisc)
library(readxl)
library(RODBC)

## read-in MS Access db ----
#db <- Hmisc::mdb.get("./data/raw/NCBN_Saltmarsh_Monitoring_BE_20191002.accdb")
db <- file.path("Z:/Files/PROJECTS/SM_Neptune_Analysis/20200211_MMI_Results/data/raw/NCBN_Saltmarsh_Monitoring_BE_20191002.accdb") #connect database.
channel <- odbcConnectAccess2007(db) #internal RODBC function

NektonEventData <- sqlFetch(channel,"NektonEventData") #read particular table from Access database file.
lu_NektonEventGear <- sqlFetch(channel, "lu_NektonEventGear")
lu_Tide <- sqlFetch(channel, "lu_Tide")
lu_Habitat <- sqlFetch(channel, "lu_Habitat")
Event <- sqlFetch(channel, "Event")
lu_SampleOrder <- sqlFetch(channel, "lu_SampleOrder")
NektonCollection <- sqlFetch(channel, "NektonCollection")
NektonLength <- sqlFetch(channel, "NektonLength")
lu_Species <- sqlFetch(channel, "lu_Species")
lu_ResidencyCode <- sqlFetch(channel, "lu_ResidencyCode")
Location <- sqlFetch(channel, "Location")
lu_Protocol <- sqlFetch(channel, "lu_Protocol")
lu_SalinityCode <- sqlFetch(channel, "lu_SalinityCode")
Site <- sqlFetch(channel, "Site")

## ----------------------------------------------------------------------------
## replicate 'qry_MMI_Export_Nekton.txt' ----
## grab db tables and rename variables per .txt file ----
#NektonEventData <- db[["NektonEventData"]]
NektonEventData$DO_ugL <- NektonEventData$DO.MicrogramPerLiter
#lu_NektonEventGear <- db[["lu_NektonEventGear"]]
lu_NektonEventGear$Gear <- lu_NektonEventGear$Label
#lu_Tide <- db[["lu_Tide"]]
lu_Tide$TideLabel <- lu_Tide$Label
#lu_Habitat <- db[["lu_Habitat"]]
lu_Habitat$Habitat <- lu_Habitat$Summary
#Event <- db[["Event"]]
Event$EventID <- Event$ID
#Event$Year <- year(mdy_hms(Event$StartDate))
Event$Year <- year(Event$StartDate)
#lu_SampleOrder <- db[["lu_SampleOrder"]]
# NektonCollection <- db[["NektonCollection"]]
# NektonLength <- db[["NektonLength"]]
# lu_Species <- db[["lu_Species"]]
# lu_ResidencyCode <- db[["lu_ResidencyCode"]]
lu_ResidencyCode$ResidencyCode <- lu_ResidencyCode$Code
# Location <- db[["Location"]]
Location$LocID2 <- Location$UniqueID
# lu_Protocol <- db[["lu_Protocol"]]
# lu_SalinityCode <- db[["lu_SalinityCode"]]
lu_SalinityCode$SalinityCode <- lu_SalinityCode$Code
#Site <- db[["Site"]]

## SQL joins ----
NektonEventData <- left_join(NektonEventData, lu_NektonEventGear, 
                             by = c("GearID" = "ID"))
NektonEventData <- left_join(NektonEventData, lu_Tide, 
                             by = c("TideID" = "ID"))
NektonEventData <- left_join(NektonEventData, lu_Habitat, 
                             by = c("HabitatID" = "ID"))
Event <- left_join(Event, lu_SampleOrder, by = c("SampleOrderID" = "ID"))
NektonCollection <- left_join(NektonCollection, NektonLength, 
                              by = c("TSN", "EventID"))
NektonCollection <- left_join(NektonCollection, lu_Species, by = c("TSN"))
NektonCollection <- left_join(NektonCollection, lu_ResidencyCode, 
                              by = c("ResidencyCodeID" = "ID"))
Event <- left_join(Event, NektonCollection, by = c("ID" = "EventID"))
Event <- inner_join(Event, NektonEventData, by = c("ID" = "EventID"))
Location <- inner_join(Location, Event, by = c("ID" = "LocationID"))
lu_Protocol <- inner_join(lu_Protocol, Location, 
                          by = c("ID" = "ProtocolID"))
lu_SalinityCode <- right_join(lu_SalinityCode, lu_Protocol, 
                              by = c("ID" = "SalinityCodeID"))
Site <- inner_join(Site, lu_SalinityCode, by = c("ID" = "SiteID"))

## SQL select ----
## note: selected additional variable 'UniqueID'
# nekton <- Site[, c("EventID", "ID", "LocID2", "UnitCode", "SubunitCode",
#                    "Year", "StartDate", "StartTime", "EndTime", "Gear",
#                    "NetArea.m2", "WaterDepth.cm", "CreekDepth.cm", 
#                    "TideLabel", "Habitat", "WaterTemp.C", "Salinity.ppt",
#                    "DO_ugL", "TSN", "ScientificName", "SpeciesCount", 
#                    "NektonLength.mm", "ResidencyCode", "SalinityCode",
#                    "UniqueID")]
nekton <- Site[, c("EventID", "ID", "LocID2", "UnitCode", "SubunitCode",
                          "Year", "StartDate", "StartTime", "EndTime", "Gear",
                          "NetArea_m2", "WaterDepth_cm", "CreekDepth_cm", 
                          "TideLabel", "Habitat", "WaterTemp_C", "Salinity_ppt",
                          "DO_MicrogramPerLiter", "TSN", "ScientificName", "SpeciesCount", 
                          "NektonLength_mm", "ResidencyCode", "SalinityCode",
                          "UniqueID")]
nekton <- distinct(nekton)
rm(list = setdiff(ls(), c("nekton", "db"))) ## clean up env

## ----------------------------------------------------------------------------
## replicate 'qry_MMI_Export_Veg.txt' ----
## grab db tables and rename variables per .txt file ----
db <- file.path("Z:/Files/PROJECTS/SM_Neptune_Analysis/20200211_MMI_Results/data/raw/NCBN_Saltmarsh_Monitoring_BE_20191002.accdb") #connect database.
channel <- odbcConnectAccess2007(db) #internal RODBC function

Event <- sqlFetch(channel, "Event")
Location <- sqlFetch(channel, "Location")
Site <- sqlFetch(channel, "Site")
lu_Species <- sqlFetch(channel, "lu_Species")
lu_Method <- sqlFetch(channel, "lu_Method")
VegData <- sqlFetch(channel, "VegData")
lu_YesNoField <- sqlFetch(channel, "lu_YesNoField")
lu_SalinityCode <- sqlFetch(channel, "lu_SalinityCode")
lu_Protocol <- sqlFetch(channel, "lu_Protocol")
lu_CoverClass <- sqlFetch(channel, "lu_CoverClass")

#Event <- db[["Event"]]
Event$EventID <- Event$ID
#Event$Year <- year(mdy_hms(Event$StartDate))
Event$Year <- year(Event$StartDate)
#Location <- db[["Location"]]
#Site <- db[["Site"]]
#lu_Species <- db[["lu_Species"]]
#lu_Method <- db[["lu_Method"]]
lu_Method$Method <- lu_Method$Summary
#VegData <- db[["VegData"]]
#lu_YesNoField <- db[["lu_YesNoField"]]
lu_YesNoField$Flowering <- lu_YesNoField$Code
#lu_SalinityCode <- db[["lu_SalinityCode"]]
lu_SalinityCode$SalinityCode <- lu_SalinityCode$Code
#lu_Protocol <- db[["lu_Protocol"]]
lu_Protocol <- filter(lu_Protocol, Code == "SMV")
#lu_CoverClass <- db[["lu_CoverClass"]]

## SQL joins ----
Event <- inner_join(Event, Location, by = c("LocationID" = "ID"))
Event <- inner_join(Event, Site, by = c("SiteID" = "ID"))
Event <- inner_join(Event, lu_Protocol, by = c("ProtocolID" = "ID"))
Event <- inner_join(Event, VegData, by = c("ID" = "EventID"))
Event <- left_join(Event, lu_Method, by = c("MethodID" = "ID"))
Event <- left_join(Event, lu_CoverClass, by = c("CoverClassID" = "ID"))
Event <- left_join(Event, lu_Species, by = c("TSN" = "TSN"))
Event <- left_join(Event, lu_SalinityCode, by = c("SalinityCodeID" = "ID"))
lu_YesNoField <- inner_join(lu_YesNoField, Event, by = c("ID" = "IsFlowering"))

lu_YesNoField$PercentCover <- ifelse(lu_YesNoField$Summary.x == "50 Point Intercept",
                                     as.character(lu_YesNoField$PercentCover), 
                                     #as.character(lu_YesNoField$Label.percent))
                                     as.character(lu_YesNoField$Label_percent))

## SQL select ----
## note: selected additional variable 'Nativity' and 'Community_Class'
veg <- lu_YesNoField[,c("EventID","LocationID","UniqueID","UnitCode",
                        "SubunitCode", "Year","StartDate","StartTime",
                        "EndTime","TSN","ScientificName", "Method",
                        "PercentCover","Alive","Flowering","SalinityCode",
                        "Nativity", #"Community.Class",
                        "Community_Class", "UniqueID.y")]
colnames(veg)[grep("UniqueID.y", colnames(veg))] <- "LocID2"
veg <- distinct(veg)

## df post-processing ----
## convert Braun-Blanquet ranges to midpoint per Nagel et al. (2013)
veg <- veg %>% 
  mutate(PercentCover = recode(PercentCover, "<1" = "0.5", "0-1" = "0.5",
                                             "1-5" = "3", "11-25" = "18",
                                             "26-50" = "38", "51-75" = "63",
                                             "6-10" = "8", "6-25" = "15.5",
                                             "76-100" = "88"))
veg$PercentCover <- as.numeric(as.character(veg$PercentCover))

# veg <- veg %>% mutate(UniqueID = recode(UniqueID, "CACO_HHR22" = "CACO_HHR"))
rm(list = setdiff(ls(), c("nekton", "veg"))) ## clean up env

## correct 'Alive' var in CACO per email from Dennis Skidds ----
## all Alive except: bare ground, disturbed ground, peat, Phrag litter, sand, 
## and Wrack/Litter
not_alive <- c("bare ground","disturbed","peat","peat layer","Phragmites litter",
               "sand","Wrack","Wrack & litter")
'%ni%' <- Negate('%in%')
veg$Alive <- ifelse((veg$UnitCode == "CACO" & 
                      veg$Year > 2006 & 
                      veg$ScientificName %ni% not_alive), 1, veg$Alive)
rm(not_alive)

# ## classify CACO Hatches Harbor veg plots into HHR vs HHUR ----
# id_df <- read_excel("./client_docs/Location_ID_veg_plots_Hatches_Harbor_2008_2013.xlsx")
# id_df$UniqueID <- ifelse(id_df$Site_Code == "Hatches_Harbor_Restricted", "CACO_HHR", 
#                          ifelse(id_df$Site_Code == "Hatches_Harbor_Unrestricted", "CACO_HHUR", NA))
# id_df <- id_df %>% select(Location_ID, UniqueID) %>% distinct()
# veg$UniqueID <- as.character(veg$UniqueID)
# for(i in 1:nrow(id_df)){
#   veg$UniqueID <- ifelse(veg$LocID2 == id_df$Location_ID[i], id_df$UniqueID[i], veg$UniqueID)
# }
# rm(i,id_df)
# 
# nekton$UniqueID <- as.character(nekton$UniqueID)
# nekton$UniqueID[grep("HHR", nekton$LocID2)] <- "CACO_HHR"
# nekton$UniqueID[grep("HHUR", nekton$LocID2)] <- "CACO_HHUR"

veg$EventID <- as.character(veg$EventID)
veg$Year_chr <- as.character(veg$Year)
veg <- veg %>%
  mutate(Community_Class = case_when(
    Community_Class == 1 ~ "salt marsh",
    Community_Class == 2 ~ "salt marsh open ditch",
    Community_Class == 3 ~ "salt marsh pool/panne",
    Community_Class == 4 ~ "tidal creek",
    Community_Class == 5 ~ "tidal lagoon",
    Community_Class == 6 ~ "freshwater wetland",
    Community_Class == 7 ~ "salt marsh plugged ditch",
    Community_Class == 8 ~ "salt marsh shoreline",
    Community_Class == 9 ~ "9",
    is.na(Community_Class) ~ NA
  ),
  SalinityCode = case_when(
    SalinityCode == "HS" ~ "high >18 ppt",
    SalinityCode == "MS" ~ "medium 0.5-18 ppt",
    SalinityCode == "NV" ~ "non-vegetated",
    SalinityCode == "LS" ~ "low <0.5 ppt",
    SalinityCode == "US" ~ "unknown",
    SalinityCode == "OTHR" ~ "other",
    SalinityCode == "NS" ~ "no",
    is.na(SalinityCode) ~ NA
  ))
## ----------------------------------------------------------------------------
## save query output to file ----
save(nekton, veg, file = "./data/derived/nekton_veg.rda")

