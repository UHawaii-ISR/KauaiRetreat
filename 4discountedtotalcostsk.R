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
      buildings_col <- paste0("buildings_",scenario,seawall,"t",trigger)
      parcel_col <- paste0("parcels8_",scenario,seawall,"t",trigger)
      
      Retreat_Analysis_Total[[buildings_col]] <- sum(Retreat_Analysis[buildings_col],na.rm=T)
      Retreat_Analysis_Total[[parcel_col]] <- sum(Retreat_Analysis[parcel_col],na.rm=T)
    }
  }
}



# Create Discount Rate Tables
years <- c(2023,2025,2026,2028,2030,2035,2040,2045,2050,2056,2062,2068,2075,2081,2087,2093,2100)
present_year <- 2023  # Set the present year for the discount rate calculation

#consider using this function for npv instead
# npv <- function(year){
#   1/((1+0.03)**(year-2023))
# }

# 2.6% discount rate (for dwelling and land value, demolition, and clean-up NPV)
discount_rates_26 <- 1.026 ^ (years - present_year)
DiscountRate26 <- data.frame(year = years, `Discount_Rates_26` = discount_rates_26)

# 3% discount rate (for tax revenue and road NPV)
discount_rates_30 <- 1.03 ^ (years - present_year)
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
  years <- c("2023", "2026", "2030", "2040", "2050", "2062", "2075", "2087", "2100")
  originalvalues <- numeric(length(years))
  discounted_values <- numeric(length(years))
  
  for (i in 1:length(years)) {
    year <- years[i]
    
    # Retrieve value
    index <- which(Retreat_Analysis$Years == year)
    value <- Retreat_Analysis[[column]][index]
    
    # Retrieve Discount_Rates_26 value and calculate discounted value
    discount_rate <- DiscountRate26$Discount_Rates_26[DiscountRate26$year == year]
    discounted_value <- value / discount_rate
    
    # Store values in vectors
    originalvalues[i] <- value
    discounted_values[i] <- discounted_value
  }
  sum(discounted_values, na.rm = TRUE)
}

# Bring total (building+land) values and private property loss into NPV ($2023) using 2.6% discount rate
# Calculate the sum of discounted values
for(trigger in triggers){
  for(seawall in seawalls){
    for(hazard_type in hazard_types){
      totalval_col <- paste0("Total_Value_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
      Retreat_Analysis_Total[[totalval_col]] <- sum(Retreat_Analysis[[totalval_col]]) # no need to discount because only has values in present (2023)
      
      privproploss_col <- paste0("Priv_Prop_Loss_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
      Retreat_Analysis_Total[[privproploss_col]] <- 0 # no priv prop loss for AO approaches since properties are assumed to have full assessed value 
      
      totalval_col <- paste0("Total_Value_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
      Retreat_Analysis_Total[[totalval_col]] <- discount_sum(totalval_col)
      
      privproploss_col <- paste0("Priv_Prop_Loss_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
      Retreat_Analysis_Total[[privproploss_col]] <- discount_sum(privproploss_col)
      
      for(bval in bvals){
        totalval_col <- paste0("Total_Value_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
        Retreat_Analysis_Total[[totalval_col]] <- discount_sum(totalval_col)
        
        privproploss_col <- paste0("Priv_Prop_Loss_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
        Retreat_Analysis_Total[[privproploss_col]] <- discount_sum(privproploss_col)
      }
    }
  }
}



# Calculate Lost Tax Revenue ($) for AO - tax revenue is all based on current tax revenue - building and land don't lose value
for (hazard_type in hazard_types) {
  for(trigger in triggers){
    for(seawall in seawalls){
      AO_taxrevloss_col <- paste0("Total_TaxRev_Loss_AO",seawall,"t", trigger, "_l", hazard_type,"_bv1")
      yearAO_col <- paste0("year_AO",seawall,"t",trigger)
      totaltax_col_2030 <- paste0("Total_TaxRev_2030_t", trigger, "_l", hazard_type,"_bv1") 
      totaltax_col_2050 <- paste0("Total_TaxRev_2050_t", trigger, "_l", hazard_type,"_bv1") 
      totaltax_col_2075 <- paste0("Total_TaxRev_2075_t", trigger, "_l", hazard_type,"_bv1") 
      totaltax_col_2100 <- paste0("Total_TaxRev_2100_t", trigger, "_l", hazard_type,"_bv1") 
      
      clean_retreat_calcs[[AO_taxrevloss_col]] <-
        ifelse(clean_retreat_calcs[[yearAO_col]] == 2023, 
               (clean_retreat_calcs$Current_Tax_Revenue * (2030 - present_year) / discount_rate_30(2025)) +
                 (clean_retreat_calcs[[totaltax_col_2030]] * (2050 - 2030) / discount_rate_30(2040)) +
                 (clean_retreat_calcs[[totaltax_col_2050]] * (2075 - 2050) / discount_rate_30(2062)) +
                 (clean_retreat_calcs[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
                 (clean_retreat_calcs[[totaltax_col_2100]] / discount_rate_30(2100)),
               NA ) 
      
      totaltaxrevloss_col <- paste0("Total_TaxRev_Loss_AO",seawall,"t", trigger, "_l", hazard_type,"_bv1")
      Retreat_Analysis_Total[[totaltaxrevloss_col]] <- sum(clean_retreat_calcs[AO_taxrevloss_col],na.rm=T)
    }
    
  }
}

#Calculate Lost Tax Revenue for TB
#* could consider using *apply() functions rather than nested for loops 
scenarios <- c("TB")  

for(scenario in scenarios){
  for(hazard_type in hazard_types){
    for(trigger in triggers){
      for(bval in bvals){
        for(seawall in seawalls){
          taxrevloss_col <- paste0("Total_TaxRev_Loss_",scenario,seawall,"t", trigger, "_l", hazard_type, "_bv",bval)
          yearRetreat_col <- paste0("year_",scenario,seawall,"t",trigger)
          totaltax_col_2023 <- paste0("Total_TaxRev_2023_t", trigger, "_l", hazard_type, "_bv",bval) 
          totaltax_col_2026 <- paste0("Total_TaxRev_2026_t", trigger, "_l", hazard_type, "_bv",bval) 
          totaltax_col_2030 <- paste0("Total_TaxRev_2030_t", trigger, "_l", hazard_type, "_bv",bval) 
          totaltax_col_2040 <- paste0("Total_TaxRev_2040_t", trigger, "_l", hazard_type, "_bv",bval) 
          totaltax_col_2050 <- paste0("Total_TaxRev_2050_t", trigger, "_l", hazard_type, "_bv",bval) 
          totaltax_col_2062 <- paste0("Total_TaxRev_2062_t", trigger, "_l", hazard_type, "_bv",bval) 
          totaltax_col_2075 <- paste0("Total_TaxRev_2075_t", trigger, "_l", hazard_type, "_bv",bval) 
          totaltax_col_2087 <- paste0("Total_TaxRev_2087_t", trigger, "_l", hazard_type, "_bv",bval) 
          totaltax_col_2100 <- paste0("Total_TaxRev_2100_t", trigger, "_l", hazard_type, "_bv",bval) 
          
          
          
          clean_retreat_calcs[[taxrevloss_col]] <- case_when(
            clean_retreat_calcs[[yearRetreat_col]] == 2023 ~
              (clean_retreat_calcs[[totaltax_col_2023]] * (2030 - 2023) / discount_rate_30(2025)) +
              (clean_retreat_calcs[[totaltax_col_2030]] * (2050 - 2030) / discount_rate_30(2040)) +
              (clean_retreat_calcs[[totaltax_col_2050]] * (2075 - 2050) / discount_rate_30(2062)) +
              (clean_retreat_calcs[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
              (clean_retreat_calcs[[totaltax_col_2100]] / discount_rate_30(2100)),
            clean_retreat_calcs[[yearRetreat_col]] == 2026 ~ 
              (clean_retreat_calcs[[totaltax_col_2026]] * (2030 - 2026) / discount_rate_30(2028)) +
              (clean_retreat_calcs[[totaltax_col_2030]] * (2050 - 2030) / discount_rate_30(2040)) +
              (clean_retreat_calcs[[totaltax_col_2050]] * (2075 - 2050) / discount_rate_30(2062)) +
              (clean_retreat_calcs[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
              (clean_retreat_calcs[[totaltax_col_2100]] / discount_rate_30(2100)),
            clean_retreat_calcs[[yearRetreat_col]] == 2030 ~ 
              (clean_retreat_calcs[[totaltax_col_2030]] * (2050 - 2030) / discount_rate_30(2040)) +
              (clean_retreat_calcs[[totaltax_col_2050]] * (2075 - 2050) / discount_rate_30(2062)) +
              (clean_retreat_calcs[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
              (clean_retreat_calcs[[totaltax_col_2100]] / discount_rate_30(2100)),
            clean_retreat_calcs[[yearRetreat_col]] == 2040 ~ 
              (clean_retreat_calcs[[totaltax_col_2040]] * (2050 - 2040) / discount_rate_30(2045)) +
              (clean_retreat_calcs[[totaltax_col_2050]] * (2075 - 2050) / discount_rate_30(2062)) +
              (clean_retreat_calcs[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
              (clean_retreat_calcs[[totaltax_col_2100]] / discount_rate_30(2100)),
            clean_retreat_calcs[[yearRetreat_col]] == 2050 ~ 
              (clean_retreat_calcs[[totaltax_col_2050]] * (2075 - 2050) / discount_rate_30(2062)) +
              (clean_retreat_calcs[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
              (clean_retreat_calcs[[totaltax_col_2100]] / discount_rate_30(2100)),
            clean_retreat_calcs[[yearRetreat_col]] == 2062 ~ 
              (clean_retreat_calcs[[totaltax_col_2062]] * (2075 - 2062) / discount_rate_30(2068)) +
              (clean_retreat_calcs[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
              (clean_retreat_calcs[[totaltax_col_2100]] / discount_rate_30(2100)),
            clean_retreat_calcs[[yearRetreat_col]] == 2075 ~ 
              (clean_retreat_calcs[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
              (clean_retreat_calcs[[totaltax_col_2100]] / discount_rate_30(2100)),
            clean_retreat_calcs[[yearRetreat_col]] == 2087 ~ 
              (clean_retreat_calcs[[totaltax_col_2087]] * (2100 - 2087) / discount_rate_30(2093)) +
              (clean_retreat_calcs[[totaltax_col_2100]] / discount_rate_30(2100)),
            clean_retreat_calcs[[yearRetreat_col]] == 2100 ~ 
              (clean_retreat_calcs[[totaltax_col_2100]] / discount_rate_30(2100)),
            TRUE ~ NA
          )
          
          totaltaxrevloss_col <- paste0("Total_TaxRev_Loss_",scenario,seawall,"t", trigger, "_l", hazard_type,"_bv",bval)
          Retreat_Analysis_Total[[totaltaxrevloss_col]] <- sum(clean_retreat_calcs[taxrevloss_col],na.rm=T)
        }
        
      }
    }
  }
}

#Calculate Lost Tax Revenue for RE
#use the taxrev columns with building value=0
#* could consider using *apply() functions rather than nested for loops 
scenarios <- c("RE")  

for(scenario in scenarios){
  for(hazard_type in hazard_types){
    for(trigger in triggers){
      for(seawall in seawalls){
        taxrevloss_col <- paste0("Total_TaxRev_Loss_",scenario,seawall,"t", trigger, "_l", hazard_type, "_bv0")
        yearRetreat_col <- paste0("year_",scenario,seawall,"t",trigger)
        totaltax_col_2023 <- paste0("Total_TaxRev_2023_t", trigger, "_l", hazard_type, "_bv0") 
        totaltax_col_2026 <- paste0("Total_TaxRev_2026_t", trigger, "_l", hazard_type, "_bv0") 
        totaltax_col_2030 <- paste0("Total_TaxRev_2030_t", trigger, "_l", hazard_type, "_bv0") 
        totaltax_col_2040 <- paste0("Total_TaxRev_2040_t", trigger, "_l", hazard_type, "_bv0") 
        totaltax_col_2050 <- paste0("Total_TaxRev_2050_t", trigger, "_l", hazard_type, "_bv0") 
        totaltax_col_2062 <- paste0("Total_TaxRev_2062_t", trigger, "_l", hazard_type, "_bv0") 
        totaltax_col_2075 <- paste0("Total_TaxRev_2075_t", trigger, "_l", hazard_type, "_bv0") 
        totaltax_col_2087 <- paste0("Total_TaxRev_2087_t", trigger, "_l", hazard_type, "_bv0") 
        totaltax_col_2100 <- paste0("Total_TaxRev_2100_t", trigger, "_l", hazard_type, "_bv0") 
        
        
        clean_retreat_calcs[[taxrevloss_col]] <- case_when(
          clean_retreat_calcs[[yearRetreat_col]] == 2023 ~
            (clean_retreat_calcs[[totaltax_col_2023]] * (2030 - 2023) / discount_rate_30(2025)) +
            (clean_retreat_calcs[[totaltax_col_2030]] * (2050 - 2030) / discount_rate_30(2040)) +
            (clean_retreat_calcs[[totaltax_col_2050]] * (2075 - 2050) / discount_rate_30(2062)) +
            (clean_retreat_calcs[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
            (clean_retreat_calcs[[totaltax_col_2100]] / discount_rate_30(2100)),
          clean_retreat_calcs[[yearRetreat_col]] == 2026 ~ 
            (clean_retreat_calcs[[totaltax_col_2026]] * (2030 - 2026) / discount_rate_30(2028)) +
            (clean_retreat_calcs[[totaltax_col_2030]] * (2050 - 2030) / discount_rate_30(2040)) +
            (clean_retreat_calcs[[totaltax_col_2050]] * (2075 - 2050) / discount_rate_30(2062)) +
            (clean_retreat_calcs[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
            (clean_retreat_calcs[[totaltax_col_2100]] / discount_rate_30(2100)),
          clean_retreat_calcs[[yearRetreat_col]] == 2030 ~ 
            (clean_retreat_calcs[[totaltax_col_2030]] * (2050 - 2030) / discount_rate_30(2040)) +
            (clean_retreat_calcs[[totaltax_col_2050]] * (2075 - 2050) / discount_rate_30(2062)) +
            (clean_retreat_calcs[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
            (clean_retreat_calcs[[totaltax_col_2100]] / discount_rate_30(2100)),
          clean_retreat_calcs[[yearRetreat_col]] == 2040 ~ 
            (clean_retreat_calcs[[totaltax_col_2040]] * (2050 - 2040) / discount_rate_30(2045)) +
            (clean_retreat_calcs[[totaltax_col_2050]] * (2075 - 2050) / discount_rate_30(2062)) +
            (clean_retreat_calcs[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
            (clean_retreat_calcs[[totaltax_col_2100]] / discount_rate_30(2100)),
          clean_retreat_calcs[[yearRetreat_col]] == 2050 ~ 
            (clean_retreat_calcs[[totaltax_col_2050]] * (2075 - 2050) / discount_rate_30(2062)) +
            (clean_retreat_calcs[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
            (clean_retreat_calcs[[totaltax_col_2100]] / discount_rate_30(2100)),
          clean_retreat_calcs[[yearRetreat_col]] == 2062 ~ 
            (clean_retreat_calcs[[totaltax_col_2062]] * (2075 - 2062) / discount_rate_30(2068)) +
            (clean_retreat_calcs[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
            (clean_retreat_calcs[[totaltax_col_2100]] / discount_rate_30(2100)),
          clean_retreat_calcs[[yearRetreat_col]] == 2075 ~ 
            (clean_retreat_calcs[[totaltax_col_2075]] * (2100 - 2075) / discount_rate_30(2087)) +
            (clean_retreat_calcs[[totaltax_col_2100]] / discount_rate_30(2100)),
          clean_retreat_calcs[[yearRetreat_col]] == 2087 ~ 
            (clean_retreat_calcs[[totaltax_col_2087]] * (2100 - 2087) / discount_rate_30(2093)) +
            (clean_retreat_calcs[[totaltax_col_2100]] / discount_rate_30(2100)),
          clean_retreat_calcs[[yearRetreat_col]] == 2100 ~ 
            (clean_retreat_calcs[[totaltax_col_2100]] / discount_rate_30(2100)),
          TRUE ~ NA
        )
        
        totaltaxrevloss_col <- paste0("Total_TaxRev_Loss_",scenario,seawall,"t", trigger, "_l", hazard_type,"_bv0")
        Retreat_Analysis_Total[[totaltaxrevloss_col]] <- sum(clean_retreat_calcs[taxrevloss_col],na.rm=T)
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
      # Retrieve the discount rate for the current year from DiscountRate26 data frame
      discount_rate <- DiscountRate26$Discount_Rates_26[DiscountRate26$year == year]
      
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


# Demolition Costs 

for(trigger in triggers){
  for(seawall in seawalls){
    #Calculate AO demolition costs
    demo_col <- paste0("demolition_AO",seawall,"t",trigger)
    
    Retreat_Analysis_Total[[demo_col]] <- sum(Retreat_Analysis[[demo_col]])
    
    #calculate TB demolition costs
    demo_col <- paste0("demolition_TB",seawall,"t",trigger)
    
    # Calculate the discounted clean-up costs for each year
    discounted_demolition_costs <- sapply(years, function(year) {
      # Retrieve the discount rate for the current year from DiscountRate26 data frame
      discount_rate <- DiscountRate26$Discount_Rates_26[DiscountRate26$year == year]
      
      # Calculate the discounted demo costs
      demo <- Retreat_Analysis[[demo_col]][Retreat_Analysis$Years == year] / discount_rate
      
    })
    
    # Sum up all discounted demolition costs
    total_discounted_demolition_costs <- sum(discounted_demolition_costs)  
    
    # add to dataframe
    Retreat_Analysis_Total[[demo_col]] <- total_discounted_demolition_costs
  }
}


# OSDS Removal Costs ($)

#AO
for(trigger in triggers){
  for(seawall in seawalls){
    osds_col <- paste0("osdsremoval_AO",seawall,"t",trigger)
    
    Retreat_Analysis_Total[[osds_col]] <- sum(Retreat_Analysis[osds_col])
  }
}

#TB&RE
scenarios <- c("TB","RE")
for(scenario in scenarios){
  for(trigger in triggers){
    for(seawall in seawalls){
      osds_col <- paste0("osdsremoval_",scenario,seawall,"t",trigger)
      
      # Calculate the discounted clean-up costs for each year
      discounted_osds_costs <- sapply(years, function(year) {
        # Retrieve the discount rate for the current year from DiscountRate26 data frame
        discount_rate <- DiscountRate26$Discount_Rates_26[DiscountRate26$year == year]
        
        # Calculate the low end and high end of the range of discounted clean-up costs
        osds <- Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year] / discount_rate
      })
      
      # Sum up all discounted demolition costs
      total_discounted_osds_costs <- sum(discounted_osds_costs)  
      
      # add to dataframe
      Retreat_Analysis_Total[[osds_col]] <- total_discounted_osds_costs
    }
  }
}


# Infrastructure & Seawall Retreat Cost 
infrastructure_total <- data.frame(init = c(NA))

#AO
for(trigger in triggers){
  for(seawall in seawalls){
    for(rdr in rdret){
      seawall_col <- paste0("seawall_AO",seawall,trigger)
      infrastructure_col <- paste0("infrastructure_AO",seawall,trigger,"_rdr",rdr)
      
      Retreat_Analysis_Total[[seawall_col]] <- sum(Retreat_Analysis[seawall_col])
      Retreat_Analysis_Total[[infrastructure_col]] <- sum(Retreat_Analysis[infrastructure_col])
    }
  }
}

#TB&RE
scenarios <- c("TB","RE")
for(scenario in scenarios){
  for(trigger in triggers){
    for(seawall in seawalls){
      for(rdr in rdret){
        seawall_col <- paste0("seawall_",scenario,seawall,trigger)
        infrastructure_col <- paste0("infrastructure_",scenario,seawall,trigger,"_rdr",rdr)
        
        # Calculate the discounted costs for each year
        discounted_seawall_costs <- sapply(years, function(year) {
          # Retrieve the discount rate for the current year from DiscountRate26 data frame
          discount_rate <- DiscountRate26$Discount_Rates_26[DiscountRate26$year == year]
          
          # Calculate the sum discounted costs
          seawall <- Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] / discount_rate
        })
        discounted_infra_costs <- sapply(years, function(year) {
          # Retrieve the discount rate for the current year from DiscountRate26 data frame
          discount_rate <- DiscountRate26$Discount_Rates_26[DiscountRate26$year == year]
          
          # Calculate the sum discounted costs
          infra <- Retreat_Analysis[[infrastructure_col]][Retreat_Analysis$Years == year] / discount_rate
        })
  
        # Sum up all discounted demolition costs
        total_discounted_seawall_costs <- sum(discounted_seawall_costs)  
        total_discounted_infra_costs <- sum(discounted_infra_costs) 
        
        # add to dataframe
        Retreat_Analysis_Total[[seawall_col]] <- total_discounted_seawall_costs
        Retreat_Analysis_Total[[infrastructure_col]] <- total_discounted_infra_costs
      
      }
    }
  }
}




###
# export csv's with all relevant calculations

#just tmk + year retreat csv
# Clean Parcel Hazard Area data, only keep important columns
#forkammie <- clean_retreat_calcs[, c("TMK","NEAR_XA32","Land_Class","CPR")]
# write.csv(retreat_time,"retreat_timing.csv")
# write.csv(clean_retreat_calcs, "clean_retreat_calcs.csv", row.names=F)
# write.csv(Retreat_Analysis, "Retreat_Analysis.csv",row.names=F)
# write.csv(Retreat_Analysis_Total, "Retreat_Analysis_Total.csv",row.names=F)
#write.csv(cost_summary, "cost_summary.csv",row.names=F)
#write.csv(Retreat_Analysis, "Retreat_Analysis.csv",row.names=F)



