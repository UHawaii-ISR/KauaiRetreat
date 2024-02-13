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
setwd("F:/slr/kauai/kauai_retreat_code/")

# Disable scientific notation
options(scipen = 999)

# Import Assessor's data and Assign data frame.  make sure TMK column cell is not in scientific notation
#assessorsshp <- st_read("F:/slr/kauai/2023_Real_Property_Tax_Data") ** make sure correct datasets are imported!
assessorsshp <- st_read("F:/slr/kauai/Practice_data",layer="buildings_tmk_dd") 
assessorsshpcpr <- st_read("F:/slr/kauai/Practice_data",layer="buildings_tmk_dd_CPR")
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

#join the dataframes. use the building-TMK-join method1 "centroid within" for non-CPR, method2 "intersect" for CPR units
assessors <- assessors[is.na(assessors$CPR_UNIT),]
assessorscpr <- assessorscpr[!is.na(assessorscpr$CPR_UNIT),]
assessorscpr <- subset(assessorscpr, select=-c(JOIN_FID))# remove column "JOIN_FID" that doesn't exist in assessors method1
assessors <- rbind(assessors,assessorscpr)

# Clean assessor's data, only keep important columns. no issue with duplicate TMK in kauai data
clean_assessors <- assessors[, c("PARID","COTMK","CPR_UNIT","TAXCLASS",
                                 "APRBLDGMKT","ASMTBLDG","APRLANDMKT","ASMTLAND",
                                 "APRTOTMKT","ASMTTOT","TOTEXEMPT","NETTAXABLE",
                                 "TARGET_FID","NEAR_2021",
                                 "NEAR_CE05","NEAR_CE11","NEAR_CE20","NEAR_CE32",
                                 "NEAR_PF05","NEAR_PF11","NEAR_PF20","NEAR_PF32",
                                 "NEAR_WF05","NEAR_WF11","NEAR_WF20","NEAR_WF32",
                                 "NEAR_XA05","NEAR_XA11","NEAR_XA20","NEAR_XA32")]

# rename columns
clean_assessors <- clean_assessors %>%
  rename(TMK = PARID,              #new name = old name
         NEAR_VEG = NEAR_2021,
         BuildingID = TARGET_FID)

# create column summing number of CPR units per building (need this later for dividing retreat costs per CPR'd building)
clean_assessors <- clean_assessors %>%
  group_by(BuildingID) %>%
  mutate(Number_CPRbldg = n())

# add apartment identification by CPR count. >15 CPR units per building is apartment building
clean_assessors$apartment <- ifelse(clean_assessors$Number_CPRbldg >15,1,0)

# buildings count per parcel 
clean_assessors <- clean_assessors %>%
  group_by(TMK) %>%
  mutate(buildings = n())

# if there are two buildings on a single non-CPR'd parcel, keep only the row with the most makai building
clean_assessors <- clean_assessors[order(clean_assessors$NEAR_VEG), ]
clean_assessors <- clean_assessors[!duplicated(clean_assessors$TMK), ] #pre: nrow = 1038, post: nrow = 856  



# Calculate current assessed tax revenue ($)

# Define the tax rates per $1000:
#residential <- 5.45 / 1000
#vacation rental <- 9.85 / 1000
#homestead <- 2.59 / 1000
#residential investor <- 9.40 / 1000
#commercialized home use <- 5.05 / 1000

#remove TMK's that are not residential 
residential <- c("1:RESIDENTIAL", "2:VACATION RENTAL","8:HOMESTEAD","9:Residential Investor","10:Commercialized Home Use") 
clean_assessors <- clean_assessors[clean_assessors$TAXCLASS %in% residential, ] #pre: nrow = 856, post: nrow = 581  

# Calculate the current tax revenue based on land class. residential tax rate = 0.0035. if below threshold 1,000,000, Residential A rate is 0.0045. 
# if above threshold, use high rate of 0.0105
clean_assessors$Current_Tax_Revenue <- case_when(
  clean_assessors$TAXCLASS == "1:RESIDENTIAL" ~ clean_assessors$NETTAXABLE * 0.00545,
  clean_assessors$TAXCLASS == "2:VACATION RENTAL" ~ clean_assessors$NETTAXABLE * 0.00985,
  clean_assessors$TAXCLASS == "8:HOMESTEAD" ~ clean_assessors$NETTAXABLE * 0.00259,
  clean_assessors$TAXCLASS == "9:Residential Investor" ~ clean_assessors$NETTAXABLE * 0.00940,
  clean_assessors$TAXCLASS == "10:Commercialized Home Use" ~ clean_assessors$NETTAXABLE * 0.00505)



## MERGE DATA


# import seawall data & parcel hazard area
seawallhazardshp <- st_read("F:/slr/kauai/tmk_XA_dd_Seawall") 
seawallhazarddf <- as.data.frame(seawallhazardshp)

#just keep the columns we need
seawallhazarddf <- seawallhazarddf[, c('PARID','area_og','SA_CE05','SA_CE11','SA_CE20','SA_CE32','SA_WF05','SA_WF11','SA_WF20','SA_WF32','Direct','Indirect','SS_FID','SS_Len_m')]

#Create baseline Shape_Area_2023_[hazard] with values of 0 (for easy integration in for loops). 
#All are assigned 0 as this is our baseline (no parcels have lost value yet, assessors value is that of current "original" parcel area)
seawallhazarddf$Shape_Area_2023_WF <- 0
seawallhazarddf$Shape_Area_2023_CE <- 0


# Convert SLR intervals to years
# Define the new column names
seawallhazarddf <- seawallhazarddf %>%
  rename(TMK = PARID,
         Shape_Area_2030_CE = SA_CE05,
         Shape_Area_2050_CE = SA_CE11,
         Shape_Area_2075_CE = SA_CE20,
         Shape_Area_2100_CE = SA_CE32,
         Shape_Area_2030_WF = SA_WF05,
         Shape_Area_2050_WF = SA_WF11,
         Shape_Area_2075_WF = SA_WF20,
         Shape_Area_2100_WF = SA_WF32,
         OG_PARCEL_AREA = area_og,
         Seawall_Direct = Direct,
         Seawall_Indirect = Indirect,
         SeawallID = SS_FID,
         Seawall_Len_M = SS_Len_m)

# Import OSDS counts data frame 
OSDS <- read.csv("tmk_OSDS.csv")

# Rename the 'POINT_COUN' column to 'OSDS'
OSDS <- OSDS %>% rename(OSDS = Join_Count)

# Join dataframes together
seawallhazarddf <- seawallhazarddf %>%
  inner_join(OSDS %>% select(TMK, OSDS), by = "tmk")


# Convert column names in Clean Parcel Hazard Area to uppercase
names(seawallhazarddf) <- toupper(names(seawallhazarddf))

# Join Building Footprint with assessor's and TMK with Parcel with hazard areas
clean_retreat_calcs <- left_join(clean_assessors, seawallhazarddf, by = c("TMK"))






# add regional/ahupuaa/moku filter if desired
#allisland <- clean_retreat_calcs
#makaha <- clean_retreat_calcs[clean_retreat_calcs$AHUPUAA == "Mākaha", ]



#overwrite clean_retreat_calcs to selected ahupuaa/qaqc
#clean_retreat_calcs <- makaha




