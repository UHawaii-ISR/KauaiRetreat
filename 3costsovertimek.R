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
        ifelse(year==2023,sum(clean_retreat_calcs$buildings[AO_matching_rows], na.rm = TRUE),0)
      
      # Count the number of TB buildings in each year
      buildings_col <- paste0("buildings_TB",seawall,"t",trigger)
      Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] <- 
        sum(clean_retreat_calcs$buildings[TB_matching_rows], na.rm = TRUE)
      
      # Count the number of RE buildings in each year
      buildings_col <- paste0("buildings_RE",seawall,"t",trigger)
      Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] <- 
        sum(clean_retreat_calcs$buildings[RE_matching_rows], na.rm = TRUE)
      
      ### NUM OF PARCELS
      
      #sum the AO parcels (in 2023)
      parcel_col <- paste0("parcels_AO",seawall,"t",trigger)
      Retreat_Analysis[[parcel_col]][Retreat_Analysis$Years == year] <- 
        ifelse(year==2023,sum(AO_matching_rows[AO_matching_rows==T],na.rm=T),0)
      
      # Count the number of TB parcels in each year
      parcels_col <- paste0("parcels_TB",seawall,"t",trigger)
      Retreat_Analysis[[parcels_col]][Retreat_Analysis$Years == year] <- 
        sum(TB_matching_rows[TB_matching_rows==T], na.rm = TRUE)
      
      # Count the number of RE parcels in each year
      parcels_col <- paste0("parcels_RE",seawall,"t",trigger)
      Retreat_Analysis[[parcels_col]][Retreat_Analysis$Years == year] <- 
        sum(RE_matching_rows[RE_matching_rows==T], na.rm = TRUE)
      
      ####  ** DIVIDE BUILDING COSTS BY NUMBER OF CPR IN BUILDING
      
      #### ** use building ID to make sure that building demo won't be duplicated
      
      ### DEMOLITION & CLEANUP COSTS
      
      #** Demolition Costs ($) for AO and TB
      demolition_house <- 8000  # Demolition cost per residential house
      demolition_apartment <- 250000 # apartment demolition cost **
      demolition_seawall <- ifelse(seawall == "_",4000,0) # conditional seawall demolition depending on scenario. $4000/ft for inaccessible seawall e.g. residential area)
      
      apartments_col <- paste0("apartment")
      seawalls_col <- paste0("SEAWALLFEET")
      
      # sum demolition costs for AO
      demo_col <- paste0("demolition_AO",seawall,"t",trigger)
      buildings_col <- paste0("buildings_AO",seawall,"t",trigger)
      aptcount <- sum(clean_retreat_calcs[[apartments_col]][AO_matching_rows], na.rm = TRUE)
      housecount <- Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] - aptcount
      seawallft <- sum(clean_retreat_calcs[[seawalls_col]][AO_matching_rows], na.rm = TRUE)
      Retreat_Analysis[[demo_col]][Retreat_Analysis$Years == year] <- 
        ifelse(year==2023,seawallft*demolition_seawall + aptcount*demolition_apartment + housecount*demolition_house,0) 
      
      # Sum demolition costs for TB 
      demo_col <- paste0("demolition_TB",seawall,"t",trigger)
      buildings_col <- paste0("buildings_TB",seawall,"t",trigger)
      aptcount <- sum(clean_retreat_calcs[[apartments_col]][TB_matching_rows], na.rm = TRUE)
      housecount <- Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] - aptcount
      seawallft <- sum(clean_retreat_calcs[[seawalls_col]][TB_matching_rows], na.rm = TRUE)
      Retreat_Analysis[[demo_col]][Retreat_Analysis$Years == year] <- 
        seawallft*demolition_seawall + aptcount*demolition_apartment + housecount*demolition_house 
      
      # Sum clean-up costs for parcels that are RE for each year
      # Low-end ($10,000 per building) **add costs for larger apartment buildings
      cleanuplo_col <- paste0("cleanuplo_RE",seawall,"t",trigger)
      cleanuphi_col <- paste0("cleanuphi_RE",seawall,"t",trigger)
      seawallft <- sum(clean_retreat_calcs[[seawalls_col]][RE_matching_rows], na.rm = TRUE)
      Retreat_Analysis[[cleanuplo_col]][Retreat_Analysis$Years == year] <- 
        Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] * 10000 + seawallft*demolition_seawall
      # High-end ($200,000 per building)
      Retreat_Analysis[[cleanuphi_col]][Retreat_Analysis$Years == year] <- 
        Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] * 200000 + seawallft*demolition_seawall
      
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
          ifelse(year==2023,sum(clean_retreat_calcs$Current_Total_Value[AO_matching_rows],na.rm=T),0)
        
        #priv prop loss under AO is 0
        
        # Sum the total building + land value of parcels that are RE (building value is none under RE)
        totalval_col <- paste0("Total_Value_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
        column_name <- paste0("Total_Value_", year, "_t",trigger,"_l",hazard_type,"_bv0")
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
          column_name <- paste0("Total_Value_", year, "_t",trigger,"_l",hazard_type,"_bv",bval)
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


# Infrastructure Retreat Cost, brought in from GIS calcs
#**add in infrastructure costs new data
infra_cost <- read.csv("Infra_cost.csv")
Retreat_Analysis$infrastructure_AO_s_ <- c(infra_cost$Sum[1],rep(0,nrow(Retreat_Analysis)-1))
Retreat_Analysis$infrastructure_TB_s_ <- c(0,0,infra_cost$X2030[2],0,infra_cost$X2050[2],0,infra_cost$X2075[2],0,infra_cost$X2100[2])
Retreat_Analysis$infrastructure_RE_s_ <- c(0,0,infra_cost$X2030[3],0,infra_cost$X2050[3],0,infra_cost$X2075[3],0,infra_cost$X2100[3])

#seawall infrastructure removal (seawalls in front of parks)
seawallparcels <- seawalldf[seawalldf$seawall == 1,]
seawallinfra <- seawallparcels[seawallparcels$tmklandclass_csv_Land_Class != "RESIDENTIAL",]
seawallinfra <- seawallinfra[seawallinfra$tmklandclass_csv_Land_Class != "RESIDENTIAL A",]
#$1500/ft for removal of accessible seawall ** what should be the timing of this?
Retreat_Analysis$infrastructure_AO_ <- Retreat_Analysis$infrastructure_AO_s_ + c(sum(seawallinfra$sum_lengthfeet)*1500,rep(0,nrow(Retreat_Analysis)-1))
Retreat_Analysis$infrastructure_TB_ <- Retreat_Analysis$infrastructure_TB_s_ + c(sum(seawallinfra$sum_lengthfeet)*1500,rep(0,nrow(Retreat_Analysis)-1))
Retreat_Analysis$infrastructure_RE_ <- Retreat_Analysis$infrastructure_RE_s_ + c(sum(seawallinfra$sum_lengthfeet)*1500,rep(0,nrow(Retreat_Analysis)-1))
