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
setwd(workdir)


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
  
  seawalld <- paste0("SEAWALL_DIRECT") #if SS_direct: 1 = direct, 2 = indirect, 0 = no seawall

  clean_retreat_calcs[[col_AO_s_trigger]] <- ifelse(clean_retreat_calcs[[seawalld]]>=1,NA,clean_retreat_calcs[[col_AO_trigger]])
  clean_retreat_calcs[[col_TB_s_trigger]] <- ifelse(clean_retreat_calcs[[seawalld]]>=1,NA,clean_retreat_calcs[[col_TB_trigger]])
  clean_retreat_calcs[[col_RE_s_trigger]] <- ifelse(clean_retreat_calcs[[seawalld]]>=1,NA,clean_retreat_calcs[[col_RE_trigger]])
}

#merge year retreat columns with the buildings dataset
retreat_years <- clean_retreat_calcs %>%
  select(TMK, starts_with('year_AO_'), starts_with('year_TB_'), starts_with('year_RE_'))
clean_assessors_bldg <- clean_assessors_bldg %>%
  left_join(retreat_years, by = "TMK")
# Update columns to NA where CPR_UNIT == 0. this is so we don't double count any buildings
clean_assessors_bldg <- clean_assessors_bldg %>%
  mutate(across(starts_with('year_AO_') | starts_with('year_TB_') | starts_with('year_RE_'),
                ~ ifelse(!is.na(CPR_UNIT) & CPR_UNIT == "0", NA, .)))

#calculate land value & tax
years <- c("2023", "2030","2050","2075","2100")  
hazard_types <- c("CE","WF","PF","XA",'full','none')

#calculate land value based on percent exposure to hazard
for (year in years) {
  for (hazard_type in hazard_types) {
    
    # percentage of land within hazard zones
    # reference and title columns containing percentage of parcel within hazard zones
    column_title <- paste0("Parcel_", year, "_l", hazard_type) 
    
    # reference correct Shape_Area columns
    shape_column_title <- paste0("SA_", year, "_", hazard_type) 
    
    # remaining land value
    # create land value columns after transferring land using both hazards
    land_assess_col <- paste0("Land_Assess_", year, "_l", hazard_type) 
    land_apr_col <- paste0("Land_Appraise_", year, "_l", hazard_type) 
    
    if(hazard_type == 'CE' | hazard_type == 'WF'| hazard_type == 'XA' | hazard_type == 'PF'){
      # create columns and fill with percentages of parcel within hazard zones (divide area in hazard zones by original parcel area)
      clean_retreat_calcs[[column_title]] <- clean_retreat_calcs[[shape_column_title]] / clean_retreat_calcs$OG_PARCEL_AREA 
      
    } else if(hazard_type == 'full'){ # retain full land value (AO subscenario)
      
      clean_retreat_calcs[[column_title]] <- 0 # percentage of land in hazard zone = 0 bc retain full land value
    } else if(hazard_type == 'none'){ # retain no land value (RE subscenario)
      
      clean_retreat_calcs[[column_title]] <- 1 # percentage of land in hazard zone = 1 bc retain zero land value
    }
    
    # fill land value columns by multiplying percentage of parcel within hazard zones by original assessed land value. (rounding used so small negative land values don't occur)
    clean_retreat_calcs[[land_assess_col]] <- (1- round(clean_retreat_calcs[[column_title]],digits=6)) * clean_retreat_calcs$ASMTLAND 
    clean_retreat_calcs[[land_apr_col]] <- (1- round(clean_retreat_calcs[[column_title]],digits=6)) * clean_retreat_calcs$APRLANDMKT  

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
    
    if(hazard_type == 'CE' | hazard_type == 'WF'| hazard_type == 'XA' | hazard_type == 'PF'){
      # create columns and fill with percentages of parcel within hazard zones (divide area in hazard zones by original parcel area)
      clean_retreat_calcs[[column_title]] <- (clean_retreat_calcs[[column_title1]] +  clean_retreat_calcs[[column_title2]])/2
      
    } else if(hazard_type == 'full'){ # retain full land value (AO subscenario)
      
      clean_retreat_calcs[[column_title]] <- 0 # percentage of land in hazard zone = 0 bc retain full land value
    } else if(hazard_type == 'none'){ # retain no land value (RE subscenario)
      
      clean_retreat_calcs[[column_title]] <- 1 # percentage of land in hazard zone = 1 bc retain zero land value
    }
    
    
    # remaining land value
    # create land value columns after transferring land using both hazards
    land_assess_col <- paste0("Land_Assess_", y, "_l", hazard_type) 
    land_apr_col <- paste0("Land_Appraise_", y, "_l", hazard_type)
    
    # fill land value columns by multiplying percentage of parcel within hazard zones by original assessed land value. (rounding used so small negative land values don't occur)
    clean_retreat_calcs[[land_assess_col]] <- clean_retreat_calcs$ASMTLAND - clean_retreat_calcs[[column_title]] * clean_retreat_calcs$ASMTLAND
    clean_retreat_calcs[[land_apr_col]] <- clean_retreat_calcs$APRLANDMKT - clean_retreat_calcs[[column_title]] * clean_retreat_calcs$APRLANDMKT
  }
}


# Calculate tax revenue 

# Assume hazards used here are WF and CE, use same call for hazards as prior for loop
years <- c("2023", "2026", "2030","2040", "2050", "2062", "2075", "2087", "2100") 
bvals <- c(1,0) #indicate whether building value is full (1) or none (0)

for (year in years) {
  for (hazard_type in hazard_types) {
    for(trigger in triggers){
      for(bval in bvals){
        
        # reference land value columns after land transfer
        land_assess_col <- paste0("Land_Assess_", year, "_l", hazard_type) 
        land_apr_col <- paste0("Land_Appraise_", year, "_l", hazard_type)
        
        # calculate total value 
        totalvalue_column_title <- paste0("Total_Appraise_", year, "_t", trigger, "_l", hazard_type, "_bv",bval) 
        clean_retreat_calcs[[totalvalue_column_title]] <- clean_retreat_calcs[[land_apr_col]] + bval * clean_retreat_calcs$APRBLDGMKT
        
        # calculate private property loss
        privproploss_column_title <- paste0("Priv_Prop_Loss_", year, "_t", trigger, "_l", hazard_type, "_bv",bval) 
        clean_retreat_calcs[[privproploss_column_title]] <- clean_retreat_calcs$APRTOTMKT - clean_retreat_calcs[[totalvalue_column_title]]
        
        #calculate total net taxable
        totaltaxable_column_title <- paste0("Total_NetTaxable_", year, "_t", trigger, "_l", hazard_type, "_bv",bval) 
        #if before reactive year, building has full/0 value (depending on bval scenario). if after year reactive, building value is 0
        col_RE_trigger <- paste0("year_RE_t",trigger)
        clean_retreat_calcs[[totaltaxable_column_title]] <- ifelse(clean_retreat_calcs[[col_RE_trigger]] > year,
                                                                   clean_retreat_calcs[[land_assess_col]] + bval * clean_retreat_calcs$ASMTBLDG - clean_retreat_calcs$TOTEXEMPT,
                                                                   clean_retreat_calcs[[land_assess_col]] - clean_retreat_calcs$TOTEXEMPT) 
        
        # calculate remaining tax revenue after land transfer using total value
        # title new columns for tax revenue after land transfer for each hazard type
        totaltax_column_title <- paste0("Total_TaxRev_", year, "_t", trigger, "_l", hazard_type, "_bv",bval) 
        
        # create columns and fill with new tax revenue 
        #min property tax =75 to avoid negative taxes https://www.kauai.gov/Government/Departments-Agencies/Finance/Real-Property-Tax/Assessment/ExemptionTax-Relief-Information#section-9
        clean_retreat_calcs[[totaltax_column_title]] <- ifelse(clean_retreat_calcs[[totaltaxable_column_title]] < 75, 75, case_when( 
          clean_retreat_calcs$TAXCLASS == "1:RESIDENTIAL" ~ clean_retreat_calcs[[totaltaxable_column_title]] * 0.00545,
          clean_retreat_calcs$TAXCLASS == "2:VACATION RENTAL" ~ clean_retreat_calcs[[totaltaxable_column_title]] * 0.00985,
          clean_retreat_calcs$TAXCLASS == "3:COMMERCIAL" ~ clean_retreat_calcs[[totaltaxable_column_title]] * 0.00810,
          clean_retreat_calcs$TAXCLASS == "4:INDUSTRIAL" ~ clean_retreat_calcs[[totaltaxable_column_title]] * 0.00810,
          clean_retreat_calcs$TAXCLASS == "5:AGRICULTURAL" ~ clean_retreat_calcs[[totaltaxable_column_title]] * 0.00675,
          clean_retreat_calcs$TAXCLASS == "6:CONSERVATION" ~ clean_retreat_calcs[[totaltaxable_column_title]] * 0.00675,
          clean_retreat_calcs$TAXCLASS == "7:HOTEL AND RESORT" ~ clean_retreat_calcs[[totaltaxable_column_title]] * 0.01085,
          clean_retreat_calcs$TAXCLASS == "8:HOMESTEAD" ~ clean_retreat_calcs[[totaltaxable_column_title]] * 0.00259,
          clean_retreat_calcs$TAXCLASS == "9:Residential Investor" ~ clean_retreat_calcs[[totaltaxable_column_title]] * 0.00940,
          clean_retreat_calcs$TAXCLASS == "10:Commercialized Home Use" ~ clean_retreat_calcs[[totaltaxable_column_title]] * 0.00505))
      }
    }
  }
}


