#naming standard for sub-scenarios: RE_s_tXA_lCE_bv1_chi (approach = AO, trigger = XA, land transfer = CE, building value = 1,cleanup cost = high, seawall = stay)
#approaches: AO, TB, RE
#seawall: s, none
#triggers: CE, WF, PF, XA
#land transfer: CE, WF
#building value: 1, 0
#cleanup cost: hi, lo


library(dplyr)
library(ggplot2)
library(ggpubr)
library(data.table)
setwd("F:/slr/kauai/kauai_retreat_code/")


# Disable scientific notation
options(scipen = 999)




### calculate retreat under each retreat type (RE,TB,AO) under each trigger (XA, WF, PF, CE)


#loop through each trigger (XA,WF,PF,CE) to calculate year retreat under each retreat type (RE,TB) 
triggers <- c("XA","WF","PF","CE")

for(trigger in triggers){
  col_AO_trigger <- paste0("year_AO_t",trigger)
  col_TB_trigger <- paste0("year_TB_t",trigger)
  col_RE_trigger <- paste0("year_RE_t",trigger)
  col_n32_trigger <- paste0("NEAR_",trigger,"32")
  col_n20_trigger <- paste0("NEAR_",trigger,"20")
  col_n11_trigger <- paste0("NEAR_",trigger,"11")
  col_n05_trigger <- paste0("NEAR_",trigger,"05")
  
  #calc year (2023) that parcel is all-at-once under the trigger
  clean_retreat_calcs[[col_AO_trigger]] <- ifelse(clean_retreat_calcs[[col_n32_trigger]] == 0, 2023,NA)
  
  #calc year that a parcel is threshold-based under the trigger
  clean_retreat_calcs[[col_TB_trigger]] <- ifelse(clean_retreat_calcs[[col_n32_trigger]] <= 6.1 & clean_retreat_calcs[[col_n32_trigger]] > 0,2100,NA)
  clean_retreat_calcs[[col_TB_trigger]] <- ifelse(clean_retreat_calcs[[col_n32_trigger]] == 0 & clean_retreat_calcs[[col_n20_trigger]] >= 6.1,2087,clean_retreat_calcs[[col_TB_trigger]])
  clean_retreat_calcs[[col_TB_trigger]] <- ifelse(clean_retreat_calcs[[col_n20_trigger]] <= 6.1 & clean_retreat_calcs[[col_n20_trigger]] > 0,2075,clean_retreat_calcs[[col_TB_trigger]])
  clean_retreat_calcs[[col_TB_trigger]] <- ifelse(clean_retreat_calcs[[col_n20_trigger]] == 0 & clean_retreat_calcs[[col_n11_trigger]] >= 6.1,2062,clean_retreat_calcs[[col_TB_trigger]])
  clean_retreat_calcs[[col_TB_trigger]] <- ifelse(clean_retreat_calcs[[col_n11_trigger]] <= 6.1 & clean_retreat_calcs[[col_n11_trigger]] > 0,2050,clean_retreat_calcs[[col_TB_trigger]])
  clean_retreat_calcs[[col_TB_trigger]] <- ifelse(clean_retreat_calcs[[col_n11_trigger]] == 0 & clean_retreat_calcs[[col_n05_trigger]] >= 6.1,2040,clean_retreat_calcs[[col_TB_trigger]])
  clean_retreat_calcs[[col_TB_trigger]] <- ifelse(clean_retreat_calcs[[col_n05_trigger]] <= 6.1 & clean_retreat_calcs[[col_n05_trigger]] > 0,2030,clean_retreat_calcs[[col_TB_trigger]])
  clean_retreat_calcs[[col_TB_trigger]] <- ifelse(clean_retreat_calcs[[col_n05_trigger]] == 0 & clean_retreat_calcs$NEAR_VEG >= 6.1,2026,clean_retreat_calcs[[col_TB_trigger]])
  clean_retreat_calcs[[col_TB_trigger]] <- ifelse(clean_retreat_calcs$NEAR_VEG <= 6.1 & clean_retreat_calcs$NEAR_VEG >= 0 & clean_retreat_calcs[[col_n05_trigger]] == 0,
                                                  2023,clean_retreat_calcs[[col_TB_trigger]])
  
  #calc year that a parcel is reactive under the trigger
  clean_retreat_calcs[[col_RE_trigger]] <- ifelse(clean_retreat_calcs[[col_n32_trigger]] == 0,2100,NA)
  clean_retreat_calcs[[col_RE_trigger]] <- ifelse(clean_retreat_calcs[[col_n20_trigger]] == 0,2075,clean_retreat_calcs[[col_RE_trigger]])
  clean_retreat_calcs[[col_RE_trigger]] <- ifelse(clean_retreat_calcs[[col_n11_trigger]] == 0,2050,clean_retreat_calcs[[col_RE_trigger]])
  clean_retreat_calcs[[col_RE_trigger]] <- ifelse(clean_retreat_calcs[[col_n05_trigger]] == 0,2030,clean_retreat_calcs[[col_RE_trigger]])
  clean_retreat_calcs[[col_RE_trigger]] <- ifelse(clean_retreat_calcs$NEAR_VEG == 0 & clean_retreat_calcs[[col_n05_trigger]] == 0,2023,clean_retreat_calcs[[col_RE_trigger]])
  
  
  #calculate retreat year under seawall scenario - if parcel has direct/indirect seawall, it does not retreat
  col_AO_trigger <- paste0("year_AO_t",trigger)
  col_TB_trigger <- paste0("year_TB_t",trigger)
  col_RE_trigger <- paste0("year_RE_t",trigger)
  
  col_AO_s_trigger <- paste0("year_AO_s_t",trigger)
  col_TB_s_trigger <- paste0("year_TB_s_t",trigger)
  col_RE_s_trigger <- paste0("year_RE_s_t",trigger)
  
  seawalld <- paste0("SEAWALL_DIRECT")
  seawalli <- paste0("SEAWALL_INDIRECT")
  
  clean_retreat_calcs[[col_AO_s_trigger]] <- ifelse(clean_retreat_calcs[[seawalld]]+clean_retreat_calcs[[seawalli]]>=1,NA,clean_retreat_calcs[[col_AO_trigger]])
  clean_retreat_calcs[[col_TB_s_trigger]] <- ifelse(clean_retreat_calcs[[seawalld]]+clean_retreat_calcs[[seawalli]]>=1,NA,clean_retreat_calcs[[col_TB_trigger]])
  clean_retreat_calcs[[col_RE_s_trigger]] <- ifelse(clean_retreat_calcs[[seawalld]]+clean_retreat_calcs[[seawalli]]>=1,NA,clean_retreat_calcs[[col_RE_trigger]])
}

#calculate land value & tax
years <- c("2023", "2030","2050","2075","2100")  
hazard_types <- c("CE","WF")

#calculate land value based on percent exposure to hazard
for (year in years) {
  for (hazard_type in hazard_types) {
    
    # percentage of land within hazard zones
    # reference and title columns containing percentage of parcel within hazard zones
    column_title <- paste0("Parcel_", year, "_l", hazard_type) 
    
    # reference correct Shape_Area columns
    shape_column_title <- paste0("SHAPE_AREA_", year, "_", hazard_type) 
    
    # create columns and fill with percentages of parcel within hazard zones (divide area in hazard zones by original parcel area)
    clean_retreat_calcs[[column_title]] <- clean_retreat_calcs[[shape_column_title]] / clean_retreat_calcs$OG_PARCEL_AREA 
    
    # remaining land value
    # create land value columns after transferring land using both hazards
    land_column_title <- paste0("Land_Value_", year, "_l", hazard_type) 
    #remaining land net taxable
    landtaxable_column_title <- paste0("Land_NetTaxable_", year, "_l", hazard_type) 
    
    # fill land value columns by multiplying percentage of parcel within hazard zones by original assessed land value. (rounding used so small negative land values don't occur)
    clean_retreat_calcs[[land_column_title]] <- (1- round(clean_retreat_calcs[[column_title]],digits=6)) * clean_retreat_calcs$Current_Land_Applied_Value
    clean_retreat_calcs[[landtaxable_column_title]] <- (1- round(clean_retreat_calcs[[column_title]],digits=6)) * clean_retreat_calcs$Current_Land_Net_taxable
    
  }
}

#for loop for TB averaged years
yearsTB <- c("2026","2040","2062","2087")
years <- c("2023", "2030","2050","2075","2100")

#calculate land value for middle years 
for (i in 1:length(yearsTB)) {
  for (hazard_type in hazard_types) {
    #get year from index number
    y <- yearsTB[i]
    
    #get the years referenced for averaging
    yearAve1 <- years[i]
    yearAve2 <- years[i+1]
    
    # percentage of land within hazard zones
    # reference and title columns containing percentage of parcel within hazard zones
    column_title <- paste0("Parcel_", y, "_l", hazard_type) 
    #reference columns for averaging
    column_title1 <- paste0("Parcel_", yearAve1, "_l", hazard_type) 
    column_title2 <- paste0("Parcel_", yearAve2, "_l", hazard_type)
    
    # create columns and fill with percentages of parcel within hazard zones (divide area in hazard zones by original parcel area)
    clean_retreat_calcs[[column_title]] <- (clean_retreat_calcs[[column_title1]] +  clean_retreat_calcs[[column_title2]])/2 
    
    # remaining land value
    # create land value columns after transferring land using both hazards
    land_column_title <- paste0("Land_Value_", y, "_l", hazard_type)
    #remaining land net taxable
    landtaxable_column_title <- paste0("Land_NetTaxable_", y, "_l", hazard_type) 
    
    # fill land value columns by multiplying percentage of parcel within hazard zones by original assessed land value. (rounding used so small negative land values don't occur)
    clean_retreat_calcs[[land_column_title]] <- clean_retreat_calcs$Current_Land_Applied_Value - clean_retreat_calcs[[column_title]] * clean_retreat_calcs$Current_Land_Applied_Value
    clean_retreat_calcs[[landtaxable_column_title]] <- clean_retreat_calcs$Current_Land_Net_taxable - clean_retreat_calcs[[column_title]] * clean_retreat_calcs$Current_Land_Net_taxable
    
  }
}


# Calculate tax revenue 

# Define the tax rates for residential and residential A
residential_rate <- 3.50 / 1000
residential_a_rate_low <- 4.50 / 1000
residential_a_rate_high <- 10.50 / 1000
residential_a_threshold <- 1000000

# Assume hazards used here are WF and CE, use same call for hazards as prior for loop
years <- c("2023", "2026", "2030","2040", "2050", "2062", "2075", "2087", "2100") 
bvals <- c(1,0) #indicate whether building value is full (1) or none (0)

for (year in years) {
  for (hazard_type in hazard_types) {
    
    # calculate remaining tax revenue after land transfer using only land value
    # title new columns for tax revenue after land transfer for each hazard type for land only
    landtax_column_title <- paste0("Land_TaxRev_", year, "_l", hazard_type)
    
    
    # reference correct net land taxable columns after land transfer
    landvalue_column_title <- paste0("Land_Value_", year, "_l", hazard_type) 
    #remaining land net taxable
    landtaxable_column_title <- paste0("Land_NetTaxable_", year, "_l", hazard_type) 
    
    # create columns and fill with new tax revenue using land only 
    clean_retreat_calcs[[landtax_column_title]] <- ifelse(clean_retreat_calcs$Land_Class == "RESIDENTIAL",
                                                          clean_retreat_calcs[[landtaxable_column_title]] * residential_rate,
                                                          ifelse(clean_retreat_calcs$Land_Class == "RESIDENTIALA",
                                                                 ifelse(clean_retreat_calcs[[landtaxable_column_title]] <= residential_a_threshold,
                                                                        clean_retreat_calcs[[landtaxable_column_title]] * residential_a_rate_low,
                                                                        (residential_a_threshold * residential_a_rate_low) +
                                                                          ((clean_retreat_calcs[[landtaxable_column_title]] - residential_a_threshold) * residential_a_rate_high)),
                                                                 0))     
    for(trigger in triggers){
      for(bval in bvals){
        # calculate total value 
        totalvalue_column_title <- paste0("Total_Value_", year, "_t", trigger, "_l", hazard_type, "_bv",bval) 
        landvalue_column_title <- paste0("Land_Value_", year, "_l", hazard_type) 
        
        clean_retreat_calcs[[totalvalue_column_title]] <- clean_retreat_calcs[[landvalue_column_title]] + bval * clean_retreat_calcs$Current_Building_Applied_Value
        
        #calculate total net taxable
        landtaxable_column_title <- paste0("Land_NetTaxable_", year, "_l", hazard_type)
        totaltaxable_column_title <- paste0("Total_NetTaxable_", year, "_t", trigger, "_l", hazard_type, "_bv",bval) 
        #if before reactive year, building has full/0 value (depending on bval scenario). if after year reactive, building value is 0
        col_RE_trigger <- paste0("year_RE_t",trigger)
        clean_retreat_calcs[[totaltaxable_column_title]] <- ifelse(clean_retreat_calcs[[col_RE_trigger]] > year,
                                                                   clean_retreat_calcs[[landtaxable_column_title]] + bval * clean_retreat_calcs$Current_Building_Net_taxable,
                                                                   clean_retreat_calcs[[landtaxable_column_title]])
        
        # calculate private property loss
        privproploss_column_title <- paste0("Priv_Prop_Loss_", year, "_t", trigger, "_l", hazard_type, "_bv",bval) 
        
        clean_retreat_calcs[[privproploss_column_title]] <- clean_retreat_calcs$Current_Total_Value - clean_retreat_calcs[[totalvalue_column_title]]
        
        # calculate remaining tax revenue after land transfer using total value
        # title new columns for tax revenue after land transfer for each hazard type
        totaltax_column_title <- paste0("Total_TaxRev_", year, "_t", trigger, "_l", hazard_type, "_bv",bval) 
        
        # create columns and fill with new tax revenue 
        clean_retreat_calcs[[totaltax_column_title]] <- ifelse(clean_retreat_calcs$Land_Class == "RESIDENTIAL",
                                                               clean_retreat_calcs[[totaltaxable_column_title]] * residential_rate,
                                                               ifelse(clean_retreat_calcs$Land_Class == "RESIDENTIALA",
                                                                      ifelse(clean_retreat_calcs[[totaltaxable_column_title]] <= residential_a_threshold,
                                                                             clean_retreat_calcs[[totaltaxable_column_title]] * residential_a_rate_low,
                                                                             (residential_a_threshold * residential_a_rate_low) +
                                                                               ((clean_retreat_calcs[[totaltaxable_column_title]] - residential_a_threshold) * residential_a_rate_high)),
                                                                      0))
      }
    }
  }
}


