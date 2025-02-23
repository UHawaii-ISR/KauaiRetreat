# Abbreviations key:
# SLR: Sea level rise
# CE: Coastal erosion
# WF: Annual high wave flooding
# PF: Passive flooding
# XA: Exposure Area
# TMK: Tax map key 
# CPR: Condominium property regime
# AO: All-at-once
# TB: Threshold-based
# RE: Reactive
# VEG: vegetation line

# Load libraries
library(dplyr)
library(data.table)
library(sf)

setwd(workdir)

# Disable scientific notation
options(scipen = 999)

# Import Assessor's data and Assign data frame.  make sure TMK column cell is not in scientific notation
assessorsshp <- st_read(noncprshpfolder,layer=noncprshplayer) 
assessorsshpcpr <- st_read(cprshpfolder,layer=cprshplayer)
assessorscpr <- as.data.frame(assessorsshpcpr)
assessors <- as.data.frame(assessorsshp)

#column definitions:
#COTMK = TMK8
#CPR_UNIT = CPR number. blank if no CPR
#PARID = TMK12 (alternatively use PARTXT which is a text character version of this column)
#APRTOTMKT = Appraised Total Market Value (use for total value)
#ASMTTOT = Total Property Assessed Value (use for tax loss: (Assessed Value-Exemptions) x Tax Rate = Taxes)
#TOTEXEMPT = Total Property Exemption
#NETTAXABLE = Total Net Taxable Value (ASMTTOT - TOTEXEMPT)
#APRLANDMKT = Appraised Land Market Value
#APRBLDGMKT = Appraised Building Market Value
#ASMTLAND = Land Assessed Value
#ASMTBLDG = Building Assessed Value
#CLASS = use for identifying study site
#TAXCLASS = use for calculating taxes

#join the dataframes. in ArcGIS, use the building-TMK-join method1 "centroid within" for non-CPR, method2 "intersect" for CPR units. 
#if there are < 3 CPR for a COTMK, these are false CPR and should treat them as non-CPR
assessorscpr <- assessorscpr[!is.na(assessorscpr$CPR_UNIT),] #remove nonCPR from CPR df
falseCPR <- assessorscpr %>% #find the false CPRs that are basically really just split parcels and not apartments
  group_by(COTMK) %>%
  filter(n_distinct(PARID) < 4) %>%
  pull(PARID) %>%
  unique()
assessors_falseCPR <- assessors %>% #create a df to store the false CPR
  filter(PARID %in% falseCPR) 
assessorscpr <- assessorscpr %>% # remove false CPRs from CPR df 
  filter(!PARID %in% falseCPR)
assessors <- assessors[is.na(assessors$CPR_UNIT),] #remove CPR and falseCPR from nonCPR df
#join all dataframes together - falseCPR, nonCPR, and CPR
assessorscpr <- assessorscpr[,intersect(names(assessors),names(assessorscpr))]
assessors <- assessors[,intersect(names(assessors),names(assessorscpr))]
assessors <- rbind(assessors,assessorscpr) 
assessors <- rbind(assessors,assessors_falseCPR) 

# Import OSDS counts data frame 
# data from here: https://github.com/cshuler/Act132_Cesspool_Prioritization/blob/main/Projected_data/OSDS_v6/Exploding_Multi_unit_TMKs/Outs/OSDSv6_Exploded_ALL.csv
osds <- read.csv(osdsfile) 

#just keep the columns we need
osds <- osds[, c('TMK','OSDS_QTY_calc')]

# Join dataframes together
#this is joining osds file by TMK9. because of CPR, there may be multiple rows that could join. if there are multiple rows, this picks the first 
assessors <- assessors %>%
  left_join(osds %>% select(TMK, OSDS_QTY_calc), by = "TMK",multiple='first') 

#if NA in osds column, set wastewater as 1 (assume that if they don't have osds, they have wastewater)
assessors$WASTEWATER[is.na(assessors$OSDS_QTY_calc)] <- 1

# Clean assessor's data, only keep important columns. no issue with duplicate TMK in kauai data
clean_assessors <- assessors[, c("PARID","COTMK","CPR_UNIT","TAXCLASS",
                                 "Community","ahupuaa","moku","devplan_","LittrlCell","devplan_id","district","dp","ballottype","NewB",
                                 "APRBLDGMKT","ASMTBLDG","APRLANDMKT","ASMTLAND",
                                 "APRTOTMKT","ASMTTOT","TOTEXEMPT","NETTAXABLE",
                                 "TARGET_FID","GIS_SQFT","NEAR_VEG",
                                 "NEAR_CE05","NEAR_CE11","NEAR_CE20","NEAR_CE32",
                                 "NEAR_PF05","NEAR_PF11","NEAR_PF20","NEAR_PF32",
                                 "NEAR_WF05","NEAR_WF11","NEAR_WF20","NEAR_WF32",
                                 "NEAR_XA05","NEAR_XA11","NEAR_XA20","NEAR_XA32",
                                 'area_og','SA_CE05','SA_CE11','SA_CE20','SA_CE32',
                                 'SA_WF05','SA_WF11','SA_WF20','SA_WF32',
                                 'SA_XA05','SA_XA11','SA_XA20','SA_XA32',
                                 'SA_PF05','SA_PF11','SA_PF20','SA_PF32',
                                 'OSDS_QTY_calc','WASTEWATER')]


#Create baseline SA_2023_[hazard] with values of 0 (for easy integration in for loops). 
#All are assigned 0 as this is our baseline (no parcels have lost value yet, assessors value is that of current "original" parcel area)
clean_assessors$SA_2023_WF <- 0
clean_assessors$SA_2023_CE <- 0
clean_assessors$SA_2023_PF <- 0
clean_assessors$SA_2023_XA <- 0

# rename columns
clean_assessors <- clean_assessors %>%
  rename(TMK = PARID,              #new name = old name
         #NEAR_VEG = NEAR__VEG,
         BuildingID = TARGET_FID,
         #CPR_PER_BLDG = Join_Count,
         BLDG_SQFT = GIS_SQFT,
         SA_2030_CE = SA_CE05,
         SA_2050_CE = SA_CE11,
         SA_2075_CE = SA_CE20,
         SA_2100_CE = SA_CE32,
         SA_2030_WF = SA_WF05,
         SA_2050_WF = SA_WF11,
         SA_2075_WF = SA_WF20,
         SA_2100_WF = SA_WF32,
         SA_2030_XA = SA_XA05,
         SA_2050_XA = SA_XA11,
         SA_2075_XA = SA_XA20,
         SA_2100_XA = SA_XA32,
         SA_2030_PF = SA_PF05,
         SA_2050_PF = SA_PF11,
         SA_2075_PF = SA_PF20,
         SA_2100_PF = SA_PF32,
         OSDS = OSDS_QTY_calc)


# create column summing number of CPR units per building (need this later for dividing retreat costs per CPR'd building)
clean_assessors <- clean_assessors %>%
  group_by(BuildingID) %>%
  mutate(Number_CPRbldg = n())

# add apartment identification by CPR count. >15 CPR units per building is apartment building
clean_assessors$apartment <- ifelse(clean_assessors$Number_CPRbldg >15,1,0)

#buildings count per parcel
clean_assessors <- clean_assessors %>%
  group_by(TMK) %>%
  mutate(buildings = n())

#data cleaning: remove TMK 0
clean_assessors <- clean_assessors[clean_assessors$TMK>0,]

#make a separate dataframe where all buildings are retained
clean_assessors_bldg <- clean_assessors
clean_assessors_bldg <- clean_assessors_bldg %>%
  group_by(COTMK) %>%
  mutate(
    TMK_str = as.character(TMK),
    # Check if the group has mixed TMKs, indicating that it's a TMK with CPRs
    has_mixed_TMKs = any(grepl("^[0-9]{11}[1-9]$", TMK_str)),
    # add a 0 indicator on -0000 TMKs to indicate if they house CPRs (used later for building count retreat)
    CPR_UNIT = ifelse(has_mixed_TMKs & grepl("0000$", TMK_str) & is.na(CPR_UNIT), '0', CPR_UNIT)
  ) %>%
  ungroup() %>%
  select(-has_mixed_TMKs,-TMK_str)
#manually adjust this this parcel and add the 0 indicator - it houses CPRs but does not share the same COTMK
clean_assessors_bldg$CPR_UNIT[clean_assessors_bldg$TMK == 280170090000] <- 0

# if there are >1 buildings on a single non-CPR'd parcel, keep only the row with the most makai building that is >300sqft
clean_assessors <- clean_assessors %>%
  group_by(TMK) %>%
  filter(
    buildings == 1 | 
      (buildings > 1 & 
         (BLDG_SQFT > 300 & NEAR_VEG == min(NEAR_VEG)))
  ) %>%
  ungroup()
#as an extra precaution, make sure there are no duplicates of TMK's that happen to have buildings equally far from veg
clean_assessors = clean_assessors[!duplicated(clean_assessors$TMK),] 

#create a dataframe for calculating amount of hazard coverage, not filtered to only residential parcels. make sure no duplicate TMKs
clean_assessors_parcels <- clean_assessors_bldg
clean_assessors_parcels <- clean_assessors_parcels[!duplicated(clean_assessors_parcels$TMK),] 

# Calculate current assessed tax revenue ($)

# Define the tax rates per $1000:
#residential <- 5.45 / 1000
#vacation rental <- 9.85 / 1000
#homestead <- 2.59 / 1000
#residential investor <- 9.40 / 1000
#commercialized home use <- 5.05 / 1000
#hotel and resort <- 10.85 / 1000
#commercial <- 8.10 / 1000
#industrial <- 8.10 / 1000
#agricultural <- 6.75 / 1000
#conservation <- 6.75 / 1000         # doesn't appear in the dataset

#remove TMK's that are not residential, but keep non-res those that are in CPR'd building with res

#fix this specific row - beach park that is incorrectly labeled as 'residential investor'
clean_assessors$TAXCLASS[clean_assessors$TMK == 180080430000] <- '6:CONSERVATION'

#Filter TMK8 ending in 0000 general parcel. If residential, keep all CPR’s within
#If CPR but no 0000 parcel, only keep those that are residential
residential <- c("1:RESIDENTIAL", "2:VACATION RENTAL","8:HOMESTEAD","9:Residential Investor","10:Commercialized Home Use") 

#VERSION 1: keep all COTMK groups where ANY row is residential
# clean_assessors <- clean_assessors %>%
#   filter(COTMK %in% filter(clean_assessors, TAXCLASS %in% residential)$COTMK | 
#       TAXCLASS %in% residential)
# clean_assessors_bldg <- clean_assessors_bldg %>%
#   filter(COTMK %in% filter(clean_assessors_bldg, TAXCLASS %in% residential)$COTMK | 
#       TAXCLASS %in% residential)

#VERSION 2: keep only COTMK groups where ALL rows are residential
# clean_assessors <- clean_assessors %>%
#   group_by(COTMK) %>%
#   filter(!any(!TAXCLASS %in% residential)) %>% #confusing, but this returns T if group has no non-residential classes
#   ungroup()
# 
# clean_assessors_bldg <- clean_assessors_bldg %>%
#   group_by(COTMK) %>%
#   filter(!any(!TAXCLASS %in% residential)) %>%
#   ungroup()

#VERSION 3: no filter - keep all tax classes



# Calculate the current tax revenue based on land class
clean_assessors$Current_Tax_Revenue <- case_when(
  clean_assessors$TAXCLASS == "1:RESIDENTIAL" ~ clean_assessors$NETTAXABLE * 0.00545,
  clean_assessors$TAXCLASS == "2:VACATION RENTAL" ~ clean_assessors$NETTAXABLE * 0.00985,
  clean_assessors$TAXCLASS == "3:COMMERCIAL" ~ clean_assessors$NETTAXABLE * 0.00810,
  clean_assessors$TAXCLASS == "4:INDUSTRIAL" ~ clean_assessors$NETTAXABLE * 0.00810,
  clean_assessors$TAXCLASS == "5:AGRICULTURAL" ~ clean_assessors$NETTAXABLE * 0.00675,
  clean_assessors$TAXCLASS == "6:CONSERVATION" ~ clean_assessors$NETTAXABLE * 0.00675,
  clean_assessors$TAXCLASS == "7:HOTEL AND RESORT" ~ clean_assessors$NETTAXABLE * 0.01085,
  clean_assessors$TAXCLASS == "8:HOMESTEAD" ~ clean_assessors$NETTAXABLE * 0.00259,
  clean_assessors$TAXCLASS == "9:Residential Investor" ~ clean_assessors$NETTAXABLE * 0.00940,
  clean_assessors$TAXCLASS == "10:Commercialized Home Use" ~ clean_assessors$NETTAXABLE * 0.00505)



## MERGE DATA


# import seawall data & parcel hazard area
seawallhazardshp <- st_read(seawallfile) 
seawallhazarddf <- as.data.frame(seawallhazardshp)

#just keep the columns we need
seawallhazarddf <- seawallhazarddf[, c('PARID','SS_direct','SS_FID','SS_len_m')]
#if SS_direct: 1 = direct, 2 = indirect, 0 = no seawall
#SS_len_m: total length of given seawallID. needs to be divided by number of TMK's that share seawall


# Convert SLR intervals to years
# Define the new column names
seawallhazarddf <- seawallhazarddf %>%
  rename(TMK = PARID,
         Seawall_Direct = SS_direct,
         SeawallID = SS_FID,
         Seawall_Len_M = SS_len_m)

# Convert column names in Clean Parcel Hazard Area to uppercase
names(seawallhazarddf) <- toupper(names(seawallhazarddf))

#calculate length of seawall in front of each parcel (dividing total length evenly amongst each direct parcel)
seawallhazarddf <- seawallhazarddf %>%
  group_by(SEAWALLID,SEAWALL_DIRECT) %>%
  mutate(SEAWALL_LEN_PERTMK = SEAWALL_LEN_M/n())
#if it's an indirect seawall parcel, make sure that attributed seawall length is 0
seawallhazarddf$SEAWALL_LEN_PERTMK <- ifelse(seawallhazarddf$SEAWALL_DIRECT == 2,0,seawallhazarddf$SEAWALL_LEN_PERTMK)
seawallhazarddf$SEAWALL_LEN_PERTMK <- ifelse(seawallhazarddf$SEAWALL_DIRECT == 0,0,seawallhazarddf$SEAWALL_LEN_PERTMK)

# Join Building Footprint with assessor's and TMK with Parcel with hazard areas
clean_retreat_calcs <- left_join(clean_assessors, seawallhazarddf, by = c("TMK"))

# sum parcel area for CPR'd parcels 
clean_retreat_calcs <- clean_retreat_calcs %>%
  group_by(COTMK) %>%
  mutate(OG_PARCEL_AREA = sum(area_og,na.rm=T))




