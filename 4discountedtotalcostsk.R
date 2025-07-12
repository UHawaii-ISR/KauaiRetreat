#naming standard for sub-scenarios: RE_s_tXA_lCE_bv1_chi (approach = AO, trigger = XA, land transfer = CE, building value = 1,cleanup cost = high, seawall = stay)

library(dplyr)
library(ggplot2)
library(ggpubr)
library(data.table)
setwd(workdir)


# Disable scientific notation
options(scipen = 999)



Retreat_Analysis_Total <- data.frame(init = c(NA))


# Calculate the number of buildings and parcels impacted under each scenario and each trigger
#within 3.2 ft SLR-XA/WF (AO-WF) and reactive by 2100 (AO-CE, TB, RE)
scenarios <- c("AO","TB","RE") 
for(scenario in scenarios){
  for(hazard_type in hazard_types){
    # Total area affected under each hazard (sq.m.)
    areahazard_col <- paste0("areahazard_l",hazard_type)
    
    Retreat_Analysis_Total[[areahazard_col]] <- sum(Retreat_Analysis[areahazard_col],na.rm=T)
    
  }
  for(seawall in seawalls){
    for(trigger in triggers){
      apartments_col <- paste0("apartments_",scenario,seawall,"t",trigger)
      buildings_col <- paste0("buildings_",scenario,seawall,"t",trigger)
      buildings_res_col <- paste0("buildings_res_",scenario,seawall,"t",trigger)
      buildings_hotel_col <- paste0("buildings_hotel_",scenario,seawall,"t",trigger)
      buildings_comm_col <- paste0("buildings_comm_",scenario,seawall,"t",trigger)
      buildings_ag_col <- paste0("buildings_ag_",scenario,seawall,"t",trigger)
      buildings_cons_col <- paste0("buildings_cons_",scenario,seawall,"t",trigger)
      CPRunits_col <- paste0("CPRunits_",scenario,seawall,"t",trigger)
      parcel_col <- paste0("parcels8_",scenario,seawall,"t",trigger)
      homes_col <- paste0("homes_",scenario,seawall,"t",trigger)
      homes_cpr_col <- paste0("homes_cpr_",scenario,seawall,"t",trigger)
      
      Retreat_Analysis_Total[[apartments_col]] <- sum(Retreat_Analysis[apartments_col],na.rm=T)
      Retreat_Analysis_Total[[buildings_col]] <- sum(Retreat_Analysis[buildings_col],na.rm=T)
      Retreat_Analysis_Total[[buildings_res_col]] <- sum(Retreat_Analysis[buildings_res_col],na.rm=T)
      Retreat_Analysis_Total[[buildings_hotel_col]] <- sum(Retreat_Analysis[buildings_hotel_col],na.rm=T)
      Retreat_Analysis_Total[[buildings_comm_col]] <- sum(Retreat_Analysis[buildings_comm_col],na.rm=T)
      Retreat_Analysis_Total[[buildings_ag_col]] <- sum(Retreat_Analysis[buildings_ag_col],na.rm=T)
      Retreat_Analysis_Total[[buildings_cons_col]] <- sum(Retreat_Analysis[buildings_cons_col],na.rm=T)
      Retreat_Analysis_Total[[CPRunits_col]] <- sum(Retreat_Analysis[CPRunits_col],na.rm=T)
      Retreat_Analysis_Total[[parcel_col]] <- sum(Retreat_Analysis[parcel_col],na.rm=T)
      Retreat_Analysis_Total[[homes_col]] <- sum(Retreat_Analysis[homes_col],na.rm=T)
      Retreat_Analysis_Total[[homes_cpr_col]] <- sum(Retreat_Analysis[homes_cpr_col],na.rm=T)
      for(rdr in rdret){
        hwylength_col <- paste0("hwylength",scenario,seawall,trigger,"_rdr",rdr)
        hwyripraplen_col <- paste0("hwyripraplen",scenario,seawall,trigger,"_rdr",rdr) #total highway riprap length
        rdremovelen_col <- paste0("rdremovelen",scenario,seawall,trigger,"_rdr",rdr) #total highway riprap length
        b_reloclen_col <- paste0("bridgerelocatelen",scenario,seawall,trigger,"_rdr",rdr)
        b_retrofitlen_col <- paste0("bridgeretrofitlen",scenario,seawall,trigger,"_rdr",rdr)
        #maintainlen_col <- paste0("maintainlen",scenario,seawall,trigger,"_rdr",rdr)
        total_affected_col <- paste0("affectedlen",scenario,seawall,trigger,"_rdr",rdr)
        hwy_affected_col <- paste0("hwy_affectedlen",scenario,seawall,trigger,"_rdr",rdr)
        nonhwy_affected_col <- paste0("nonhwy_affectedlen",scenario,seawall,trigger,"_rdr",rdr)
        
        Retreat_Analysis_Total[[hwylength_col]] <- sum(Retreat_Analysis[hwylength_col],na.rm=T)
        Retreat_Analysis_Total[[hwyripraplen_col]] <- sum(Retreat_Analysis[hwyripraplen_col],na.rm=T)
        Retreat_Analysis_Total[[rdremovelen_col]] <- sum(Retreat_Analysis[rdremovelen_col],na.rm=T)
        Retreat_Analysis_Total[[b_reloclen_col]] <- sum(Retreat_Analysis[b_reloclen_col],na.rm=T)
        Retreat_Analysis_Total[[b_retrofitlen_col]] <- sum(Retreat_Analysis[b_retrofitlen_col],na.rm=T)
        #Retreat_Analysis_Total[[maintainlen_col]] <- Retreat_Analysis[[maintainlen_col]][Retreat_Analysis$Years == 2100] #capture the total length that is maintained at 2100
        if(scenario == 'AO'){
          Retreat_Analysis_Total[[total_affected_col]] <- Retreat_Analysis[[total_affected_col]][Retreat_Analysis$Years == 2025]
          Retreat_Analysis_Total[[hwy_affected_col]] <- Retreat_Analysis[[hwy_affected_col]][Retreat_Analysis$Years == 2025]
          Retreat_Analysis_Total[[nonhwy_affected_col]] <- Retreat_Analysis[[nonhwy_affected_col]][Retreat_Analysis$Years == 2025]
          
        }else{
          Retreat_Analysis_Total[[total_affected_col]] <- Retreat_Analysis[[total_affected_col]][Retreat_Analysis$Years == 2100]
          Retreat_Analysis_Total[[hwy_affected_col]] <- Retreat_Analysis[[hwy_affected_col]][Retreat_Analysis$Years == 2100]
          Retreat_Analysis_Total[[nonhwy_affected_col]] <- Retreat_Analysis[[nonhwy_affected_col]][Retreat_Analysis$Years == 2100]
          
        }
         
      }
    }
  }
}



# Create Discount Rate Tables
years <- c(2025,2027,2028,2029,2030,2035,2040,2045,2050,2056,2062,2068,2075,2081,2087,2093,2100)
present_year <- 2025  # Set the present year for the discount rate calculation

#consider using this function for npv instead
# npv <- function(year){
#   1/((1+0.03)**(year-2025))
# }

discount_rate <- 1.03 #adjust for sensitivity analysis 

# 3% discount rate (for tax revenue and road NPV)
discount_rates_30 <- discount_rate ^ (years - present_year)
DiscountRate30 <- data.frame(year = years, `Discount_Rates_30` = discount_rates_30)

# Find the corresponding discount rate for each year
discount_rate_30 <- function(year) {
  row <- DiscountRate30[DiscountRate30$year == year, ]
  if (nrow(row) > 0) {
    return(row$Discount_Rates_30)
  } else {
    return(0)  # Default discount rate if year is not found in DiscountRate30
  }
}

discount_sum <- function(column) {
  years <- c("2025", "2028", "2030", "2040", "2050", "2062", "2075", "2087", "2100")
  originalvalues <- numeric(length(years))
  discounted_values <- numeric(length(years))
  
  for (i in 1:length(years)) {
    year <- years[i]
    
    # Retrieve value
    index <- which(Retreat_Analysis$Years == year)
    value <- Retreat_Analysis[[column]][index]
    
    # Retrieve Discount_Rates_30 value and calculate discounted value
    discount_rate <- DiscountRate30$Discount_Rates_30[DiscountRate30$year == year]
    discounted_value <- value / discount_rate
    
    # Store values in vectors
    originalvalues[i] <- value
    discounted_values[i] <- discounted_value
  }
  sum(discounted_values, na.rm = TRUE)
}

# Bring total (building+land) values and private property loss into NPV ($2025) using 3% discount rate
# Calculate the sum of discounted values
for(trigger in triggers){
  for(seawall in seawalls){
    for(hazard_type in hazard_types){
      #AO
      totalval_col <- paste0("Total_Value_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
      Retreat_Analysis_Total[[totalval_col]] <- sum(Retreat_Analysis[[totalval_col]]) # no need to discount because only has values in present (2025)
      
      privproploss_col <- paste0("Priv_Prop_Loss_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
      Retreat_Analysis_Total[[privproploss_col]] <- 0 # no priv prop loss for AO approaches since properties are assumed to have full assessed value 
      
      totalval_col <- paste0("Total_Value_res_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
      Retreat_Analysis_Total[[totalval_col]] <- sum(Retreat_Analysis[[totalval_col]])
      privproploss_col <- paste0("Priv_Prop_Loss_res_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
      Retreat_Analysis_Total[[privproploss_col]] <- 0 
      
      totalval_col <- paste0("Total_Value_hotel_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
      Retreat_Analysis_Total[[totalval_col]] <- sum(Retreat_Analysis[[totalval_col]])
      privproploss_col <- paste0("Priv_Prop_Loss_hotel_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
      Retreat_Analysis_Total[[privproploss_col]] <- 0 
      
      totalval_col <- paste0("Total_Value_ag_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
      Retreat_Analysis_Total[[totalval_col]] <- sum(Retreat_Analysis[[totalval_col]])
      privproploss_col <- paste0("Priv_Prop_Loss_ag_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
      Retreat_Analysis_Total[[privproploss_col]] <- 0 
      
      totalval_col <- paste0("Total_Value_comm_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
      Retreat_Analysis_Total[[totalval_col]] <- sum(Retreat_Analysis[[totalval_col]])
      privproploss_col <- paste0("Priv_Prop_Loss_comm_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
      Retreat_Analysis_Total[[privproploss_col]] <- 0 
      
      totalval_col <- paste0("Total_Value_cons_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
      Retreat_Analysis_Total[[totalval_col]] <- sum(Retreat_Analysis[[totalval_col]])
      privproploss_col <- paste0("Priv_Prop_Loss_cons_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
      Retreat_Analysis_Total[[privproploss_col]] <- 0 
      
      #RE
      totalval_col <- paste0("Total_Value_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
      Retreat_Analysis_Total[[totalval_col]] <- discount_sum(totalval_col)
      
      privproploss_col <- paste0("Priv_Prop_Loss_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
      Retreat_Analysis_Total[[privproploss_col]] <- discount_sum(privproploss_col)
      
      totalval_col <- paste0("Total_Value_res_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
      Retreat_Analysis_Total[[totalval_col]] <- discount_sum(totalval_col)
      privproploss_col <- paste0("Priv_Prop_Loss_res_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
      Retreat_Analysis_Total[[privproploss_col]] <- discount_sum(privproploss_col)
      
      totalval_col <- paste0("Total_Value_hotel_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
      Retreat_Analysis_Total[[totalval_col]] <- discount_sum(totalval_col)
      privproploss_col <- paste0("Priv_Prop_Loss_hotel_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
      Retreat_Analysis_Total[[privproploss_col]] <- discount_sum(privproploss_col)
      
      totalval_col <- paste0("Total_Value_ag_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
      Retreat_Analysis_Total[[totalval_col]] <- discount_sum(totalval_col)
      privproploss_col <- paste0("Priv_Prop_Loss_ag_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
      Retreat_Analysis_Total[[privproploss_col]] <- discount_sum(privproploss_col)
      
      totalval_col <- paste0("Total_Value_comm_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
      Retreat_Analysis_Total[[totalval_col]] <- discount_sum(totalval_col)
      privproploss_col <- paste0("Priv_Prop_Loss_comm_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
      Retreat_Analysis_Total[[privproploss_col]] <- discount_sum(privproploss_col)
      
      totalval_col <- paste0("Total_Value_cons_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
      Retreat_Analysis_Total[[totalval_col]] <- discount_sum(totalval_col)
      privproploss_col <- paste0("Priv_Prop_Loss_cons_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
      Retreat_Analysis_Total[[privproploss_col]] <- discount_sum(privproploss_col)
      
      for(bval in bvals){
        #TB
        totalval_col <- paste0("Total_Value_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
        Retreat_Analysis_Total[[totalval_col]] <- discount_sum(totalval_col)
        
        privproploss_col <- paste0("Priv_Prop_Loss_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
        Retreat_Analysis_Total[[privproploss_col]] <- discount_sum(privproploss_col)
        
        totalval_col <- paste0("Total_Value_res_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
        Retreat_Analysis_Total[[totalval_col]] <- discount_sum(totalval_col)
        privproploss_col <- paste0("Priv_Prop_Loss_res_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
        Retreat_Analysis_Total[[privproploss_col]] <- discount_sum(privproploss_col)
        
        totalval_col <- paste0("Total_Value_hotel_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
        Retreat_Analysis_Total[[totalval_col]] <- discount_sum(totalval_col)
        privproploss_col <- paste0("Priv_Prop_Loss_hotel_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
        Retreat_Analysis_Total[[privproploss_col]] <- discount_sum(privproploss_col)
        
        totalval_col <- paste0("Total_Value_ag_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
        Retreat_Analysis_Total[[totalval_col]] <- discount_sum(totalval_col)
        privproploss_col <- paste0("Priv_Prop_Loss_ag_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
        Retreat_Analysis_Total[[privproploss_col]] <- discount_sum(privproploss_col)
        
        totalval_col <- paste0("Total_Value_comm_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
        Retreat_Analysis_Total[[totalval_col]] <- discount_sum(totalval_col)
        privproploss_col <- paste0("Priv_Prop_Loss_comm_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
        Retreat_Analysis_Total[[privproploss_col]] <- discount_sum(privproploss_col)
        
        totalval_col <- paste0("Total_Value_cons_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
        Retreat_Analysis_Total[[totalval_col]] <- discount_sum(totalval_col)
        privproploss_col <- paste0("Priv_Prop_Loss_cons_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
        Retreat_Analysis_Total[[privproploss_col]] <- discount_sum(privproploss_col)
      }
    }
  }
}


# Define taxclass-filtered datasets and corresponding suffixes
taxclass_list <- list(
  "all" = clean_retreat_calcs,  # No filter version
  "res" = clean_retreat_calcs_home,
  "hotel" = clean_retreat_calcs_hotel,
  "comm" = clean_retreat_calcs_comm,
  "ag" = clean_retreat_calcs_ag,
  "cons" = clean_retreat_calcs_cons
)

for(taxclass in names(taxclass_list)) {
  current_data <- taxclass_list[[taxclass]]
  
  # Determine suffix for column names
  suffix <- ifelse(taxclass == "all", "", paste0("_", taxclass))
  
  # Calculate Lost Tax Revenue ($) for AO
  for (hazard_type in hazard_types) {
    for(trigger in triggers){
      for(seawall in seawalls){
        AO_taxrevloss_col <- paste0("Total_TaxRev_Loss",suffix,"_AO", seawall, "t", trigger, "_l", hazard_type, "_bv1")
        yearAO_col <- paste0("year_AO", seawall, "t", trigger)
        totaltax_col_2030 <- paste0("Total_TaxRev_2030_t", trigger, "_l", hazard_type, "_bv1") 
        totaltax_col_2050 <- paste0("Total_TaxRev_2050_t", trigger, "_l", hazard_type, "_bv1") 
        totaltax_col_2075 <- paste0("Total_TaxRev_2075_t", trigger, "_l", hazard_type, "_bv1") 
        totaltax_col_2100 <- paste0("Total_TaxRev_2100_t", trigger, "_l", hazard_type, "_bv1") 
        
        current_data[[AO_taxrevloss_col]] <-
          ifelse(current_data[[yearAO_col]] == 2025, 
                 (current_data$Current_Tax_Revenue * (2030 - present_year) / discount_rate_30(2027)) +
                   (current_data[[totaltax_col_2030]] * (2050 - 2030) / discount_rate_30(2040)) +
                   (current_data[[totaltax_col_2050]] * (2075 - 2050) / discount_rate_30(2062)) +
                   (current_data[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
                   (current_data[[totaltax_col_2100]] / discount_rate_30(2100)),
                 NA ) 
        
        totaltaxrevloss_col <- paste0("Total_TaxRev_Loss", suffix,"_AO", seawall, "t", trigger, "_l", hazard_type, "_bv1")
        Retreat_Analysis_Total[[totaltaxrevloss_col]] <- sum(current_data[[AO_taxrevloss_col]], na.rm=T)
      }
    }
  }
  
  # Calculate Lost Tax Revenue for TB
  scenarios <- c("TB")  
  
  for(scenario in scenarios){
    for(hazard_type in hazard_types){
      for(trigger in triggers){
        for(bval in bvals){
          for(seawall in seawalls){
            taxrevloss_col <- paste0("Total_TaxRev_Loss", suffix,"_", scenario, seawall, "t", trigger, "_l", hazard_type, "_bv", bval)
            yearRetreat_col <- paste0("year_", scenario, seawall, "t", trigger)
            totaltax_col_2025 <- paste0("Total_TaxRev_2025_t", trigger, "_l", hazard_type, "_bv", bval) 
            totaltax_col_2028 <- paste0("Total_TaxRev_2028_t", trigger, "_l", hazard_type, "_bv", bval) 
            totaltax_col_2030 <- paste0("Total_TaxRev_2030_t", trigger, "_l", hazard_type, "_bv", bval) 
            totaltax_col_2040 <- paste0("Total_TaxRev_2040_t", trigger, "_l", hazard_type, "_bv", bval) 
            totaltax_col_2050 <- paste0("Total_TaxRev_2050_t", trigger, "_l", hazard_type, "_bv", bval) 
            totaltax_col_2062 <- paste0("Total_TaxRev_2062_t", trigger, "_l", hazard_type, "_bv", bval) 
            totaltax_col_2075 <- paste0("Total_TaxRev_2075_t", trigger, "_l", hazard_type, "_bv", bval) 
            totaltax_col_2087 <- paste0("Total_TaxRev_2087_t", trigger, "_l", hazard_type, "_bv", bval) 
            totaltax_col_2100 <- paste0("Total_TaxRev_2100_t", trigger, "_l", hazard_type, "_bv", bval) 
            
            current_data[[taxrevloss_col]] <- case_when(
              current_data[[yearRetreat_col]] == 2025 ~
                (current_data[[totaltax_col_2025]] * (2030 - 2025) / discount_rate_30(2027)) +
                (current_data[[totaltax_col_2030]] * (2050 - 2030) / discount_rate_30(2040)) +
                (current_data[[totaltax_col_2050]] * (2075 - 2050) / discount_rate_30(2062)) +
                (current_data[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
                (current_data[[totaltax_col_2100]] / discount_rate_30(2100)),
              current_data[[yearRetreat_col]] == 2028 ~ 
                (current_data[[totaltax_col_2028]] * (2030 - 2028) / discount_rate_30(2029)) +
                (current_data[[totaltax_col_2030]] * (2050 - 2030) / discount_rate_30(2040)) +
                (current_data[[totaltax_col_2050]] * (2075 - 2050) / discount_rate_30(2062)) +
                (current_data[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
                (current_data[[totaltax_col_2100]] / discount_rate_30(2100)),
              current_data[[yearRetreat_col]] == 2030 ~ 
                (current_data[[totaltax_col_2030]] * (2050 - 2030) / discount_rate_30(2040)) +
                (current_data[[totaltax_col_2050]] * (2075 - 2050) / discount_rate_30(2062)) +
                (current_data[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
                (current_data[[totaltax_col_2100]] / discount_rate_30(2100)),
              current_data[[yearRetreat_col]] == 2040 ~ 
                (current_data[[totaltax_col_2040]] * (2050 - 2040) / discount_rate_30(2045)) +
                (current_data[[totaltax_col_2050]] * (2075 - 2050) / discount_rate_30(2062)) +
                (current_data[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
                (current_data[[totaltax_col_2100]] / discount_rate_30(2100)),
              current_data[[yearRetreat_col]] == 2050 ~ 
                (current_data[[totaltax_col_2050]] * (2075 - 2050) / discount_rate_30(2062)) +
                (current_data[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
                (current_data[[totaltax_col_2100]] / discount_rate_30(2100)),
              current_data[[yearRetreat_col]] == 2062 ~ 
                (current_data[[totaltax_col_2062]] * (2075 - 2062) / discount_rate_30(2068)) +
                (current_data[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
                (current_data[[totaltax_col_2100]] / discount_rate_30(2100)),
              current_data[[yearRetreat_col]] == 2075 ~ 
                (current_data[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
                (current_data[[totaltax_col_2100]] / discount_rate_30(2100)),
              current_data[[yearRetreat_col]] == 2087 ~ 
                (current_data[[totaltax_col_2087]] * (2100 - 2087) / discount_rate_30(2093)) +
                (current_data[[totaltax_col_2100]] / discount_rate_30(2100)),
              current_data[[yearRetreat_col]] == 2100 ~ 
                (current_data[[totaltax_col_2100]] / discount_rate_30(2100)),
              TRUE ~ NA
            )
            
            totaltaxrevloss_col <- paste0("Total_TaxRev_Loss", suffix,"_", scenario, seawall, "t", trigger, "_l", hazard_type, "_bv", bval)
            Retreat_Analysis_Total[[totaltaxrevloss_col]] <- sum(current_data[[taxrevloss_col]], na.rm=T)
          }
        }
      }
    }
  }
  
  # Calculate Lost Tax Revenue for RE
  scenarios <- c("RE")  
  
  for(scenario in scenarios){
    for(hazard_type in hazard_types){
      for(trigger in triggers){
        for(seawall in seawalls){
          taxrevloss_col <- paste0("Total_TaxRev_Loss", suffix,"_", scenario, seawall, "t", trigger, "_l", hazard_type, "_bv0")
          yearRetreat_col <- paste0("year_", scenario, seawall, "t", trigger)
          totaltax_col_2025 <- paste0("Total_TaxRev_2025_t", trigger, "_l", hazard_type, "_bv0") 
          totaltax_col_2028 <- paste0("Total_TaxRev_2028_t", trigger, "_l", hazard_type, "_bv0") 
          totaltax_col_2030 <- paste0("Total_TaxRev_2030_t", trigger, "_l", hazard_type, "_bv0") 
          totaltax_col_2040 <- paste0("Total_TaxRev_2040_t", trigger, "_l", hazard_type, "_bv0") 
          totaltax_col_2050 <- paste0("Total_TaxRev_2050_t", trigger, "_l", hazard_type, "_bv0") 
          totaltax_col_2062 <- paste0("Total_TaxRev_2062_t", trigger, "_l", hazard_type, "_bv0") 
          totaltax_col_2075 <- paste0("Total_TaxRev_2075_t", trigger, "_l", hazard_type, "_bv0") 
          totaltax_col_2087 <- paste0("Total_TaxRev_2087_t", trigger, "_l", hazard_type, "_bv0") 
          totaltax_col_2100 <- paste0("Total_TaxRev_2100_t", trigger, "_l", hazard_type, "_bv0") 
          
          current_data[[taxrevloss_col]] <- case_when(
            current_data[[yearRetreat_col]] == 2025 ~
              (current_data[[totaltax_col_2025]] * (2030 - 2025) / discount_rate_30(2027)) +
              (current_data[[totaltax_col_2030]] * (2050 - 2030) / discount_rate_30(2040)) +
              (current_data[[totaltax_col_2050]] * (2075 - 2050) / discount_rate_30(2062)) +
              (current_data[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
              (current_data[[totaltax_col_2100]] / discount_rate_30(2100)),
            current_data[[yearRetreat_col]] == 2028 ~ 
              (current_data[[totaltax_col_2028]] * (2030 - 2028) / discount_rate_30(2029)) +
              (current_data[[totaltax_col_2030]] * (2050 - 2030) / discount_rate_30(2040)) +
              (current_data[[totaltax_col_2050]] * (2075 - 2050) / discount_rate_30(2062)) +
              (current_data[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
              (current_data[[totaltax_col_2100]] / discount_rate_30(2100)),
            current_data[[yearRetreat_col]] == 2030 ~ 
              (current_data[[totaltax_col_2030]] * (2050 - 2030) / discount_rate_30(2040)) +
              (current_data[[totaltax_col_2050]] * (2075 - 2050) / discount_rate_30(2062)) +
              (current_data[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
              (current_data[[totaltax_col_2100]] / discount_rate_30(2100)),
            current_data[[yearRetreat_col]] == 2040 ~ 
              (current_data[[totaltax_col_2040]] * (2050 - 2040) / discount_rate_30(2045)) +
              (current_data[[totaltax_col_2050]] * (2075 - 2050) / discount_rate_30(2062)) +
              (current_data[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
              (current_data[[totaltax_col_2100]] / discount_rate_30(2100)),
            current_data[[yearRetreat_col]] == 2050 ~ 
              (current_data[[totaltax_col_2050]] * (2075 - 2050) / discount_rate_30(2062)) +
              (current_data[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
              (current_data[[totaltax_col_2100]] / discount_rate_30(2100)),
            current_data[[yearRetreat_col]] == 2062 ~ 
              (current_data[[totaltax_col_2062]] * (2075 - 2062) / discount_rate_30(2068)) +
              (current_data[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
              (current_data[[totaltax_col_2100]] / discount_rate_30(2100)),
            current_data[[yearRetreat_col]] == 2075 ~ 
              (current_data[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
              (current_data[[totaltax_col_2100]] / discount_rate_30(2100)),
            current_data[[yearRetreat_col]] == 2087 ~ 
              (current_data[[totaltax_col_2087]] * (2100 - 2087) / discount_rate_30(2093)) +
              (current_data[[totaltax_col_2100]] / discount_rate_30(2100)),
            current_data[[yearRetreat_col]] == 2100 ~ 
              (current_data[[totaltax_col_2100]] / discount_rate_30(2100)),
            TRUE ~ NA
          )
          
          totaltaxrevloss_col <- paste0("Total_TaxRev_Loss", suffix,"_", scenario, seawall, "t", trigger, "_l", hazard_type, "_bv0")
          Retreat_Analysis_Total[[totaltaxrevloss_col]] <- sum(current_data[[taxrevloss_col]], na.rm=T)
        }
      }
    }
  }
}





# Clean-up cost calculation (RE only) ($)

years <- unique(Retreat_Analysis$Years)  # Get unique years from Retreat_Analysis data frame

for(trigger in triggers){
  for(seawall in seawalls){
    cleanuplo_col <- paste0("cleanuplo_RE",seawall,"t",trigger)
    cleanuphi_col <- paste0("cleanuphi_RE",seawall,"t",trigger)
    # Calculate the discounted clean-up costs for each year
    discounted_cleanups <- sapply(years, function(year) {
      # Retrieve the discount rate for the current year from DiscountRate30 data frame
      discount_rate <- DiscountRate30$Discount_Rates_30[DiscountRate30$year == year]
      
      # Calculate the low end and high end of the range of discounted clean-up costs
      low_end <- Retreat_Analysis[[cleanuplo_col]][Retreat_Analysis$Years == year] / discount_rate
      high_end <- Retreat_Analysis[[cleanuphi_col]][Retreat_Analysis$Years == year] / discount_rate
      
      # Return the calculated values
      c(low_end, high_end)
    })
    
    # Add up all the low ends and high ends of the range of all discounted clean-up costs
    total_low_end <- sum(discounted_cleanups[1, ])  # Sum of all low ends
    total_high_end <- sum(discounted_cleanups[2, ])  # Sum of all high ends
    
    # add to dataframe
    Retreat_Analysis_Total[[cleanuplo_col]] <- total_low_end
    Retreat_Analysis_Total[[cleanuphi_col]] <- total_high_end
  }
}


taxclasses <- c("", "res", "hotel", "ag", "comm", "cons")


# Demolition Costs 

# Demolition Costs - AO
for(trigger in triggers){
  for(seawall in seawalls){
    for(taxclass in taxclasses){
      if(taxclass == ""){
        demo_col <- paste0("demolition_AO",seawall,"t",trigger)
      } else {
        demo_col <- paste0("demolition_",taxclass,"_AO",seawall,"t",trigger)
      }
      
      Retreat_Analysis_Total[[demo_col]] <- sum(Retreat_Analysis[[demo_col]])
    }
  }
}

# Demolition Costs - TB (with discounting)
for(trigger in triggers){
  for(seawall in seawalls){
    for(taxclass in taxclasses){
      if(taxclass == ""){
        demo_col <- paste0("demolition_TB",seawall,"t",trigger)
      } else {
        demo_col <- paste0("demolition_",taxclass,"_TB",seawall,"t",trigger)
      }
      
      discounted_demolition_costs <- sapply(years, function(year) {
        discount_rate <- DiscountRate30$Discount_Rates_30[DiscountRate30$year == year]
        demo <- Retreat_Analysis[[demo_col]][Retreat_Analysis$Years == year] / discount_rate
      })
      
      Retreat_Analysis_Total[[demo_col]] <- sum(discounted_demolition_costs)
    }
  }
}


# OSDS & Seawall Removal Costs ($)

#AO
for(trigger in triggers){
  for(seawall in seawalls){
    for(taxclass in taxclasses){
      if(taxclass == ""){
        osds_col <- paste0("osdsremoval_AO",seawall,"t",trigger)
        wastewater_col <- paste0("wastewaterremoval_AO",seawall,"t",trigger)
        seawall_col <- paste0("seawall_AO",seawall,trigger)
      } else {
        osds_col <- paste0("osdsremoval_",taxclass,"_AO",seawall,"t",trigger)
        wastewater_col <- paste0("wastewaterremoval_",taxclass,"_AO",seawall,"t",trigger)
        seawall_col <- paste0("seawall_",taxclass,"_AO",seawall,trigger)
      }
      
      Retreat_Analysis_Total[[osds_col]] <- sum(Retreat_Analysis[[osds_col]])
      Retreat_Analysis_Total[[wastewater_col]] <- sum(Retreat_Analysis[[wastewater_col]])
      Retreat_Analysis_Total[[seawall_col]] <- sum(Retreat_Analysis[[seawall_col]])
    }
  }
}



#TB&RE
scenarios <- c("TB","RE")
for(scenario in scenarios){
  for(trigger in triggers){
    for(seawall in seawalls){
      for(taxclass in taxclasses){
        
        # Create column names based on tax class
        if(taxclass == ""){
          # Original column names (no tax class)
          osds_col <- paste0("osdsremoval_",scenario,seawall,"t",trigger)
          wastewater_col <- paste0("wastewaterremoval_",scenario,seawall,"t",trigger)
          seawall_col <- paste0("seawall_",scenario,seawall,trigger)
        } else {
          # Tax class-specific column names
          osds_col <- paste0("osdsremoval_",taxclass,"_",scenario,seawall,"t",trigger)
          wastewater_col <- paste0("wastewaterremoval_",taxclass,"_",scenario,seawall,"t",trigger)
          seawall_col <- paste0("seawall_",taxclass,"_",scenario,seawall,trigger)
        }

        # Calculate the discounted clean-up costs for each year
        discounted_osds_costs <- sapply(years, function(year) {
          # Retrieve the discount rate for the current year from DiscountRate30 data frame
          discount_rate <- DiscountRate30$Discount_Rates_30[DiscountRate30$year == year]
          
          # Calculate the low end and high end of the range of discounted clean-up costs
          osds <- Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year] / discount_rate
        })
        discounted_water_costs <- sapply(years, function(year) {
          # Retrieve the discount rate for the current year from DiscountRate30 data frame
          discount_rate <- DiscountRate30$Discount_Rates_30[DiscountRate30$year == year]
          
          # Calculate the low end and high end of the range of discounted clean-up costs
          water <- Retreat_Analysis[[wastewater_col]][Retreat_Analysis$Years == year] / discount_rate
        })
        discounted_seawall_costs <- sapply(years, function(year) {
          # Retrieve the discount rate for the current year from DiscountRate30 data frame
          discount_rate <- DiscountRate30$Discount_Rates_30[DiscountRate30$year == year]
          
          # Calculate the low end and high end of the range of discounted clean-up costs
          seawall <- Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] / discount_rate
        })
        
        # Sum up all discounted demolition costs
        total_discounted_osds_costs <- sum(discounted_osds_costs)  
        total_discounted_water_costs <- sum(discounted_water_costs)  
        total_discounted_seawall_costs <- sum(discounted_seawall_costs)
        
        # add to dataframe
        Retreat_Analysis_Total[[osds_col]] <- total_discounted_osds_costs
        Retreat_Analysis_Total[[wastewater_col]] <- total_discounted_water_costs
        Retreat_Analysis_Total[[seawall_col]] <- total_discounted_seawall_costs
      }
    }
  }
}


# Infrastructure  Retreat Cost 

#AO
for(trigger in triggers){
  for(seawall in seawalls){
    for(rdr in rdret){
      infrastructure_col <- paste0("infrastructure_AO",seawall,trigger,"_rdr",rdr)
      b_reloc_col <- paste0("bridgerelocateAO",seawall,trigger,"_rdr",rdr)
      b_retrofit_col <- paste0("bridgeretrofitAO",seawall,trigger,"_rdr",rdr)
      hwy_reloc_col <- paste0("hwyrelocateAO",seawall,trigger,"_rdr",rdr)
      water_reloc_col <- paste0("waterrelocateAO",seawall,trigger,"_rdr",rdr)
      emdom_col <- paste0("emdomAO",seawall,trigger,"_rdr",rdr)
      hwy_riprap_col <- paste0("hwyriprapAO",seawall,trigger,"_rdr",rdr)
      rd_remove_col <- paste0("rdremoveAO",seawall,trigger,"_rdr",rdr)
      riprap_remove_col <- paste0("riprapremoveAO",seawall,trigger,"_rdr",rdr)
      #maintain_col <- paste0("maintainAO",seawall,trigger,"_rdr",rdr)
      
      Retreat_Analysis_Total[[infrastructure_col]] <- sum(Retreat_Analysis[infrastructure_col])
      Retreat_Analysis_Total[[b_reloc_col]] <- sum(Retreat_Analysis[b_reloc_col])
      Retreat_Analysis_Total[[b_retrofit_col]] <- sum(Retreat_Analysis[b_retrofit_col])
      Retreat_Analysis_Total[[hwy_reloc_col]] <- sum(Retreat_Analysis[hwy_reloc_col])
      Retreat_Analysis_Total[[water_reloc_col]] <- sum(Retreat_Analysis[water_reloc_col])
      Retreat_Analysis_Total[[emdom_col]] <- sum(Retreat_Analysis[emdom_col])
      Retreat_Analysis_Total[[hwy_riprap_col]] <- sum(Retreat_Analysis[hwy_riprap_col])
      Retreat_Analysis_Total[[rd_remove_col]] <- sum(Retreat_Analysis[rd_remove_col])
      Retreat_Analysis_Total[[riprap_remove_col]] <- sum(Retreat_Analysis[riprap_remove_col])
      #Retreat_Analysis_Total[[maintain_col]] <- sum(Retreat_Analysis[maintain_col])
    }
  }
}

#TB&RE
scenarios <- c("TB","RE")
for(scenario in scenarios){
  for(trigger in triggers){
    for(seawall in seawalls){
      for(rdr in rdret){
        infrastructure_col <- paste0("infrastructure_",scenario,seawall,trigger,"_rdr",rdr)
        b_reloc_col <- paste0("bridgerelocate",scenario,seawall,trigger,"_rdr",rdr)
        b_retrofit_col <- paste0("bridgeretrofit",scenario,seawall,trigger,"_rdr",rdr)
        hwy_reloc_col <- paste0("hwyrelocate",scenario,seawall,trigger,"_rdr",rdr)
        water_reloc_col <- paste0("waterrelocate",scenario,seawall,trigger,"_rdr",rdr)
        emdom_col <- paste0("emdom",scenario,seawall,trigger,"_rdr",rdr)
        hwy_riprap_col <- paste0("hwyriprap",scenario,seawall,trigger,"_rdr",rdr)
        rd_remove_col <- paste0("rdremove",scenario,seawall,trigger,"_rdr",rdr)
        riprap_remove_col <- paste0("riprapremove",scenario,seawall,trigger,"_rdr",rdr)
        #maintain_col <- paste0("maintain",scenario,seawall,trigger,"_rdr",rdr)
        
        columns <- c(infrastructure_col,b_reloc_col,b_retrofit_col,hwy_reloc_col,water_reloc_col,
                     emdom_col,hwy_riprap_col,rd_remove_col,riprap_remove_col) #,maintain_col)
        
        for(column in columns){
          # Calculate the discounted costs for each year
          discounted_costs <- sapply(years, function(year) {
            # Retrieve the discount rate for the current year from DiscountRate30 data frame
            discount_rate <- DiscountRate30$Discount_Rates_30[DiscountRate30$year == year]
            
            # Calculate the sum discounted costs
            discountsum <- Retreat_Analysis[[column]][Retreat_Analysis$Years == year] / discount_rate
          })
          
          # Sum up all discounted demolition costs
          total_discounted_costs <- sum(discounted_costs)
          
          # add to dataframe
          Retreat_Analysis_Total[[column]] <- total_discounted_costs
        }
      }
    }
  }
}







