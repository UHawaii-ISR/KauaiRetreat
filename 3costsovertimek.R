#naming standard for sub-scenarios: RE_s_tXA_lCE_bv1_chi (approach = AO, trigger = XA, land transfer = CE, building value = 1,cleanup cost = high, seawall = stay)

library(dplyr)
library(ggplot2)
library(ggpubr)
library(data.table)
setwd("F:/slr/kauai/kauai_retreat_code/")


# Disable scientific notation
options(scipen = 999)


# summarize costs over time
years <- c(2023, 2026, 2030, 2040, 2050, 2062, 2075, 2087, 2100)
Retreat_Analysis <- data.frame(
  Years = years)

#seawall scenarios: "_s_" = seawall stays and parcels&buildings directly/indirectly behind are protected. "_" = seawall is removed and retreat occurs
seawalls <- c("_","_s_")

#calculate values for each year
for (year in years) {
  for(seawall in seawalls){
    for(trigger in triggers){
      yearAO_col <- paste0("year_AO",seawall,"t",trigger)
      yearTB_col <- paste0("year_TB",seawall,"t",trigger)
      yearRE_col <- paste0("year_RE",seawall,"t",trigger)
      
      AO_matching_rows <- clean_retreat_calcs[[yearAO_col]] == year
      TB_matching_rows <- clean_retreat_calcs[[yearTB_col]] == year
      RE_matching_rows <- clean_retreat_calcs[[yearRE_col]] == year
      
      ### NUM OF BUILDINGS
      
      #Count the number of AO buildings (in 2023)
      buildings_col <- paste0("buildings_AO",seawall,"t",trigger)
      Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] <- 
        ifelse(year==2023,n_distinct(clean_retreat_calcs$BuildingID[AO_matching_rows], na.rm = TRUE),0)
      
      # Count the number of TB buildings in each year
      buildings_col <- paste0("buildings_TB",seawall,"t",trigger)
      Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_retreat_calcs$BuildingID[TB_matching_rows], na.rm = TRUE)
      
      # Count the number of RE buildings in each year
      buildings_col <- paste0("buildings_RE",seawall,"t",trigger)
      Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_retreat_calcs$BuildingID[RE_matching_rows], na.rm = TRUE)
      
      ### NUM OF APARTMENT BLDGs
      
      #Count the number of AO apartments (in 2023)
      apartments_col <- paste0("apartments_AO",seawall,"t",trigger)
      apt_rows <- clean_retreat_calcs[["apartment"]] ==1
      Retreat_Analysis[[apartments_col]][Retreat_Analysis$Years == year] <- 
        ifelse(year==2023,n_distinct(clean_retreat_calcs$BuildingID[AO_matching_rows][apt_rows], na.rm = TRUE),0)
      
      # Count the number of TB apartments in each year
      apartments_col <- paste0("apartments_TB",seawall,"t",trigger)
      Retreat_Analysis[[apartments_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_retreat_calcs$BuildingID[TB_matching_rows][apt_rows], na.rm = TRUE)
      
      # Count the number of RE apartments in each year
      apartments_col <- paste0("apartments_RE",seawall,"t",trigger)
      Retreat_Analysis[[apartments_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_retreat_calcs$BuildingID[RE_matching_rows][apt_rows], na.rm = TRUE)
      
      ### NUM OF SEAWALLS
      
      #Count the number of AO seawalls (in 2023)
      seawalls_col <- paste0("seawalls_AO",seawall,"t",trigger)
      Retreat_Analysis[[seawalls_col]][Retreat_Analysis$Years == year] <- 
        ifelse(year==2023,n_distinct(clean_retreat_calcs$SEAWALLID[AO_matching_rows], na.rm = TRUE),0)
      
      # Count the number of TB seawalls in each year
      seawalls_col <- paste0("seawalls_TB",seawall,"t",trigger)
      Retreat_Analysis[[seawalls_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_retreat_calcs$BuildingID[TB_matching_rows], na.rm = TRUE)
      
      # Count the number of RE seawalls in each year
      seawalls_col <- paste0("seawalls_RE",seawall,"t",trigger)
      Retreat_Analysis[[seawalls_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_retreat_calcs$BuildingID[RE_matching_rows], na.rm = TRUE)
      
      ### NUM OF PARCELS (based on TMK8 - COTMK column)
      
      #sum the AO parcels (in 2023)
      parcels_col <- paste0("parcels8_AO",seawall,"t",trigger)
      Retreat_Analysis[[parcels_col]][Retreat_Analysis$Years == year] <- 
        ifelse(year==2023,n_distinct(clean_retreat_calcs$COTMK[AO_matching_rows==T],na.rm=T),0)
      
      # Count the number of TB parcels in each year
      parcels_col <- paste0("parcels8_TB",seawall,"t",trigger)
      Retreat_Analysis[[parcels_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_retreat_calcs$COTMK[TB_matching_rows==T], na.rm = TRUE)
      
      # Count the number of RE parcels in each year
      parcels_col <- paste0("parcels8_RE",seawall,"t",trigger)
      Retreat_Analysis[[parcels_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_retreat_calcs$COTMK[RE_matching_rows==T], na.rm = TRUE)
      
      
      ### NUM OF PARCELS / CPR (based on TMK12)
      
      #sum the AO parcels (in 2023)
      parcels_col <- paste0("parcelcpr_AO",seawall,"t",trigger)
      Retreat_Analysis[[parcels_col]][Retreat_Analysis$Years == year] <- 
        ifelse(year==2023,n_distinct(clean_retreat_calcs$TMK[AO_matching_rows==T],na.rm=T),0)
      
      # Count the number of TB parcels in each year
      parcels_col <- paste0("parcelcpr_TB",seawall,"t",trigger)
      Retreat_Analysis[[parcels_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_retreat_calcs$TMK[TB_matching_rows==T], na.rm = TRUE)
      
      # Count the number of RE parcels in each year
      parcels_col <- paste0("parcelcpr_RE",seawall,"t",trigger)
      Retreat_Analysis[[parcels_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_retreat_calcs$TMK[RE_matching_rows==T], na.rm = TRUE)
      
      
      ### DEMOLITION & CLEANUP COSTS
      
      # Demolition Costs ($) for AO and TB
      demolition_house <- 8000  # Demolition cost per residential house
      demolition_apartment <- 250000 # apartment demolition cost **

      # sum demolition costs for AO
      demo_col <- paste0("demolition_AO",seawall,"t",trigger)
      buildings_col <- paste0("buildings_AO",seawall,"t",trigger)
      apartments_col <- paste0("apartments_AO",seawall,"t",trigger)
      aptcount <- Retreat_Analysis[[apartments_col]][Retreat_Analysis$Years == year]
      housecount <- Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] -  aptcount #num houses = #buildings - #apartments
      Retreat_Analysis[[demo_col]][Retreat_Analysis$Years == year] <- 
        ifelse(year==2023,aptcount*demolition_apartment + housecount*demolition_house,0) 
      
      # Sum demolition costs for TB 
      demo_col <- paste0("demolition_TB",seawall,"t",trigger)
      buildings_col <- paste0("buildings_TB",seawall,"t",trigger)
      apartments_col <- paste0("apartments_TB",seawall,"t",trigger)
      aptcount <- Retreat_Analysis[[apartments_col]][Retreat_Analysis$Years == year]
      housecount <- Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] - aptcount
      Retreat_Analysis[[demo_col]][Retreat_Analysis$Years == year] <- 
        aptcount*demolition_apartment + housecount*demolition_house 
      
      # Sum clean-up costs for parcels that are RE for each year

      cleanlo_house <- 10000 #Low-end ($10,000 per building)
      cleanhi_house <- 200000 # High-end ($200,000 per building)
      clean_apartment <- 250000 #clean-up costs for apartment building **** this is a dummy value
      
      cleanuplo_col <- paste0("cleanuplo_RE",seawall,"t",trigger)
      cleanuphi_col <- paste0("cleanuphi_RE",seawall,"t",trigger)
      buildings_col <- paste0("buildings_RE",seawall,"t",trigger)
      apartments_col <- paste0("apartments_RE",seawall,"t",trigger)
      aptcount <- Retreat_Analysis[[apartments_col]][Retreat_Analysis$Years == year]
      housecount <- Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] - aptcount
      
      Retreat_Analysis[[cleanuplo_col]][Retreat_Analysis$Years == year] <- 
        aptcount * clean_apartment + housecount * cleanlo_house 
      Retreat_Analysis[[cleanuphi_col]][Retreat_Analysis$Years == year] <- 
        aptcount * clean_apartment + housecount * cleanhi_house 
      
      ### AREA RETREATED
      
      # Total area retreated in AO (sq.m.)
      arearetreat_col <- paste0("arearetreat_AO",seawall,"t",trigger)
      Retreat_Analysis[[arearetreat_col]][Retreat_Analysis$Years == year] <- 
        ifelse(year==2023,sum(clean_retreat_calcs[["OG_PARCEL_AREA"]][AO_matching_rows], na.rm = TRUE),0) 
      
      # Total area retreated in TB (sq.m.)
      arearetreat_col <- paste0("arearetreat_TB",seawall,"t",trigger)
      Retreat_Analysis[[arearetreat_col]][Retreat_Analysis$Years == year] <- 
        sum(clean_retreat_calcs[["OG_PARCEL_AREA"]][TB_matching_rows], na.rm = TRUE)
      
      # Total area retreated in RE (sq.m.)
      arearetreat_col <- paste0("arearetreat_RE",seawall,"t",trigger)
      Retreat_Analysis[[arearetreat_col]][Retreat_Analysis$Years == year] <- 
        sum(clean_retreat_calcs[["OG_PARCEL_AREA"]][RE_matching_rows], na.rm = TRUE)
      
      ### OSDS REMOVAL
      
      #** need to add a cost for non-osds homes wastewater pipe removal
      
      # OSDS removal costs for AO ($)
      osds_col <- paste0("osdsremoval_AO",seawall,"t",trigger)
      Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year] <-
        ifelse(year==2023,sum(clean_retreat_calcs$OSDS[AO_matching_rows], na.rm = TRUE) * 2000,0)
      
      # Sum OSDS costs for TB ($)
      osds_col <- paste0("osdsremoval_TB",seawall,"t",trigger)
      Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year] <-
        sum(clean_retreat_calcs[["OSDS"]][TB_matching_rows], na.rm = TRUE) * 2000
      
      # Sum OSDS costs for RE ($)
      osds_col <- paste0("osdsremoval_RE",seawall,"t",trigger)
      Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year] <-
        sum(clean_retreat_calcs[["OSDS"]][RE_matching_rows], na.rm = TRUE) * 2000
      
      for(hazard_type in hazard_types){
        
        ### AREA UNDER HAZARD
        
        # Total area affected under each hazard (sq.m.)
        areahazard_col <- paste0("areahazard_l",hazard_type)
        parcelhazard <- paste0("Parcel_", year, "_l", hazard_type) 
        Retreat_Analysis[[areahazard_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_retreat_calcs[[parcelhazard]]*clean_retreat_calcs$OG_PARCEL_AREA, na.rm = TRUE)
        
        ### TOTAL VALUE, PROPERTY LOSS, TAX REV LOSS
        
        #total building + land value of parcels that are AO (full building value under AO)
        totalval_col <- paste0("Total_Value_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
        Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year] <- 
          ifelse(year==2023,sum(clean_retreat_calcs$APRTOTMKT[AO_matching_rows],na.rm=T),0)
        
        #priv prop loss under AO is 0
        
        # Sum the total building + land value of parcels that are RE (building value is none under RE)
        totalval_col <- paste0("Total_Value_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
        column_name <- paste0("Total_Appraise_", year, "_t",trigger,"_l",hazard_type,"_bv0")
        Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_retreat_calcs[[column_name]][RE_matching_rows], na.rm = TRUE)
        
        # Sum private property loss of parcels that are RE
        privproploss_col <- paste0("Priv_Prop_Loss_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
        column_name <- paste0("Priv_Prop_Loss_", year, "_t",trigger,"_l",hazard_type,"_bv0")
        Retreat_Analysis[[privproploss_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_retreat_calcs[[column_name]][RE_matching_rows], na.rm = TRUE)
        
        # Sum tax revenue loss for parcels that are RE (no building value)
        taxrev_col <- paste0("Total_TaxRev_RE",seawall,"t", trigger, "_l", hazard_type, "_bv0")
        column_name <- paste0("Total_TaxRev_", year, "_t",trigger,"_l",hazard_type,"_bv0")
        Retreat_Analysis[[taxrev_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_retreat_calcs[[column_name]][RE_matching_rows], na.rm = TRUE)
        
        for(bval in bvals){
          
          # Sum the total building + land value of parcels that are TB 
          totalval_col <- paste0("Total_Value_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
          column_name <- paste0("Total_Appraise_", year, "_t",trigger,"_l",hazard_type,"_bv",bval)
          Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year] <- 
            sum(clean_retreat_calcs[[column_name]][TB_matching_rows], na.rm = TRUE)
          
          # Sum private property loss of parcels that are TB 
          privproploss_col <- paste0("Priv_Prop_Loss_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
          column_name <- paste0("Priv_Prop_Loss_", year, "_t",trigger,"_l",hazard_type,"_bv",bval)
          Retreat_Analysis[[privproploss_col]][Retreat_Analysis$Years == year] <- 
            sum(clean_retreat_calcs[[column_name]][TB_matching_rows], na.rm = TRUE)
          
          # Sum tax revenue for parcels that are TB 
          taxrev_col <- paste0("Total_TaxRev_TB",seawall,"t", trigger, "_l", hazard_type, "_bv",bval)
          column_name <- paste0("Total_TaxRev_", year, "_t",trigger,"_l",hazard_type,"_bv",bval)
          Retreat_Analysis[[taxrev_col]][Retreat_Analysis$Years == year] <- 
            sum(clean_retreat_calcs[[column_name]][TB_matching_rows], na.rm = TRUE)
        }
      }
    }
  }
}



