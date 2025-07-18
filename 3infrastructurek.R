#naming standard for sub-scenarios: RE_s_tXA_lCE_bv1_chi (approach = AO, trigger = XA, land transfer = CE, building value = 1,cleanup cost = high, seawall = stay)

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(data.table)
library(sf)
setwd(workdir)


# Disable scientific notation
options(scipen = 999)




#calculate cost for eminent domain

#open assessors data for parcels adjacent (just outside) slr-xa
slrxaadjshp <- st_read(emdomfile)  
slrxaadjdf <- as.data.frame(slrxaadjshp)

slrxaadjdf <- slrxaadjdf %>%
  filter(PARID %% 1000 == 0 )

#calculate cost per area
#APRTOTMKT = Appraised Total Market Value (use for total value)
#Shape__Are = sq meters. assume 10 meter width is necessary for a road realignment
slrxaadjdf$costarea <- slrxaadjdf$APRTOTMKT / slrxaadjdf$Shape__Are
aveappraisedkauai <- mean(slrxaadjdf$costarea,na.rm=T) #this is cost per sq meter
emdom_hwy <- aveappraisedkauai*10 #this is cost per 1 meter length of road, assuming road is 10 meters wide




#add infrastructure demo calcs to Retreat_Analysis df and breakdown in infra_costtime

years <- c(2025, 2028, 2030, 2040, 2050, 2062, 2075, 2087, 2100)
scenarios <- c('AO','TB','RE')

infra_costtime <- data.frame(
  Years = years)

for (year in years) {
  for(seawall in seawalls){
    for(trigger in triggers){
      for(rdr in rdret){

      #highways, bridges, roads
      bridge_reloc <- 388691  #per meter retreat    
      bridge_riprap <- 82697  #per meter retrofit
      highway_reloc <- 298412  #per meter realignment
      water_reloc <- 6539  #per meter removal replacement water mains
      highway_riprap <- 42629  #per meter hardening
      road_remove <- 37  #per meter
      emdom_hwy #per meter length of road
      riprap_remove <- 16979  #per meter 
      #maintain <- 13586  #per meter
      
      for(scenario in scenarios){
        subdf <- subset(infra_retreat, Scenario == scenario & Year == year & Trigger == trigger & rdret == rdr)
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
        hwylength_col <- paste0("hwylength",scenario,seawall,trigger,"_rdr",rdr) #total highway length affected 
        hwyripraplen_col <- paste0("hwyripraplen",scenario,seawall,trigger,"_rdr",rdr) #total highway riprap length
        rdremovelen_col <- paste0("rdremovelen",scenario,seawall,trigger,"_rdr",rdr) #total highway riprap length
        b_reloclen_col <- paste0("bridgerelocatelen",scenario,seawall,trigger,"_rdr",rdr)
        b_retrofitlen_col <- paste0("bridgeretrofitlen",scenario,seawall,trigger,"_rdr",rdr)
        #maintainlen_col <- paste0("maintainlen",scenario,seawall,trigger,"_rdr",rdr)
        total_affected_col <- paste0("affectedlen",scenario,seawall,trigger,"_rdr",rdr)
        hwy_affected_col <- paste0("hwy_affectedlen",scenario,seawall,trigger,"_rdr",rdr)
        nonhwy_affected_col <- paste0("nonhwy_affectedlen",scenario,seawall,trigger,"_rdr",rdr)
        
        
        if(nrow(subdf)==0){
          infra_costtime[[b_reloc_col]][infra_costtime$Years == year] <- 0
          infra_costtime[[b_retrofit_col]][infra_costtime$Years == year] <- 0
          infra_costtime[[hwy_reloc_col]][infra_costtime$Years == year] <- 0
          infra_costtime[[water_reloc_col]][infra_costtime$Years == year] <- 0
          infra_costtime[[emdom_col]][infra_costtime$Years == year] <- 0
          infra_costtime[[hwy_riprap_col]][infra_costtime$Years == year] <- 0
          infra_costtime[[rd_remove_col]][infra_costtime$Years == year] <- 0
          infra_costtime[[riprap_remove_col]][infra_costtime$Years == year] <- 0
          #infra_costtime[[maintain_col]][infra_costtime$Years == year] <- 0
          infra_costtime[[hwylength_col]][infra_costtime$Years == year] <- 0
          infra_costtime[[hwyripraplen_col]][infra_costtime$Years == year] <- 0
          infra_costtime[[rdremovelen_col]][infra_costtime$Years == year] <- 0
          infra_costtime[[b_reloclen_col]][infra_costtime$Years == year] <- 0
          infra_costtime[[b_retrofitlen_col]][infra_costtime$Years == year] <- 0
          #infra_costtime[[maintainlen_col]][infra_costtime$Years == year] <- 0
          infra_costtime[[total_affected_col]][infra_costtime$Years == year] <- 0
          infra_costtime[[hwy_affected_col]][infra_costtime$Years == year] <- 0
          infra_costtime[[nonhwy_affected_col]][infra_costtime$Years == year] <- 0
          
          
        } else if(seawall == "_s_"){ #seawall-stay scenario
          subdf <- subdf %>%
            mutate(across(maintain_hwy:removeriprap_rd, ~ if_else(swall_s > 0, 0, .)))
          subdf <- subdf %>%
            mutate(across(maintain_hwy:removeriprap_hwy, ~ if_else(swall_s > 0, 0, .)))#set 0's for relocating columns since these rows will stay behind the seawall
          
          infra_costtime[[b_reloc_col]][infra_costtime$Years == year] <- sum(subdf$relocate_b,na.rm=T)*bridge_reloc
          infra_costtime[[b_retrofit_col]][infra_costtime$Years == year] <- (sum(subdf$riprap_b,na.rm=T)+sum(subdf$riprap_b_s,na.rm=T))*bridge_riprap
          infra_costtime[[hwy_reloc_col]][infra_costtime$Years == year] <- sum(subdf$relocate_hwy,na.rm=T)*highway_reloc
          infra_costtime[[water_reloc_col]][infra_costtime$Years == year] <- sum(subdf$relocate_hwy,na.rm=T)*water_reloc
          infra_costtime[[emdom_col]][infra_costtime$Years == year] <- sum(subdf$relocate_hwy,na.rm=T)*emdom_hwy
          infra_costtime[[hwy_riprap_col]][infra_costtime$Years == year] <- (sum(subdf$riprap_hwy,na.rm=T)+sum(subdf$riprap_hwy_s,na.rm=T)+sum(subdf$riprap_rd_s,na.rm=T))*highway_riprap
          infra_costtime[[rd_remove_col]][infra_costtime$Years == year] <- sum(subdf$remove_rd,na.rm=T)*road_remove
          infra_costtime[[riprap_remove_col]][infra_costtime$Years == year] <- sum(subdf$removeriprap_rd,subdf$removeriprap_hwy,na.rm=T)*riprap_remove
          #infra_costtime[[maintain_col]][infra_costtime$Years == year] <- (sum(subdf$maintain_hwy,na.rm=T)+sum(subdf$maintain_s,na.rm=T))*maintain
          infra_costtime[[hwylength_col]][infra_costtime$Years == year] <- sum(subdf$relocate_hwy,na.rm=T) 
          infra_costtime[[hwyripraplen_col]][infra_costtime$Years == year] <- sum(subdf$riprap_hwy,na.rm=T) 
          infra_costtime[[rdremovelen_col]][infra_costtime$Years == year] <- sum(subdf$remove_rd,na.rm=T) 
          infra_costtime[[b_reloclen_col]][infra_costtime$Years == year] <- sum(subdf$relocate_b,na.rm=T)
          infra_costtime[[b_retrofitlen_col]][infra_costtime$Years == year] <- (sum(subdf$riprap_b,na.rm=T)+sum(subdf$riprap_b_s,na.rm=T))
          #infra_costtime[[maintainlen_col]][infra_costtime$Years == year] <- (sum(subdf$maintain_hwy,na.rm=T)+sum(subdf$maintain_s,na.rm=T))
          infra_costtime[[total_affected_col]][infra_costtime$Years == year] <- (sum(subdf$total_affected,na.rm=T))
          infra_costtime[[hwy_affected_col]][infra_costtime$Years == year] <- (sum(subdf$hwy_affected ,na.rm=T))
          infra_costtime[[nonhwy_affected_col]][infra_costtime$Years == year] <- (sum(subdf$nonhwy_affected ,na.rm=T))
          
        } else{ 
          infra_costtime[[b_reloc_col]][infra_costtime$Years == year] <- sum(subdf$relocate_b,na.rm=T)*bridge_reloc
          infra_costtime[[b_retrofit_col]][infra_costtime$Years == year] <- sum(subdf$riprap_b,na.rm=T)*bridge_riprap
          infra_costtime[[hwy_reloc_col]][infra_costtime$Years == year] <- sum(subdf$relocate_hwy,na.rm=T)*highway_reloc
          infra_costtime[[water_reloc_col]][infra_costtime$Years == year] <- sum(subdf$relocate_hwy,na.rm=T)*water_reloc
          infra_costtime[[emdom_col]][infra_costtime$Years == year] <- sum(subdf$relocate_hwy,na.rm=T)*emdom_hwy
          infra_costtime[[hwy_riprap_col]][infra_costtime$Years == year] <- sum(subdf$riprap_hwy,na.rm=T)*highway_riprap
          infra_costtime[[rd_remove_col]][infra_costtime$Years == year] <- sum(subdf$remove_rd,na.rm=T)*road_remove
          infra_costtime[[riprap_remove_col]][infra_costtime$Years == year] <- sum(subdf$removeriprap_rd,subdf$removeriprap_hwy,na.rm=T)*riprap_remove
          #infra_costtime[[maintain_col]][infra_costtime$Years == year] <- sum(subdf$maintain_hwy,na.rm=T)*maintain
          infra_costtime[[hwylength_col]][infra_costtime$Years == year] <- sum(subdf$relocate_hwy,na.rm=T) 
          infra_costtime[[hwyripraplen_col]][infra_costtime$Years == year] <- sum(subdf$riprap_hwy,na.rm=T) 
          infra_costtime[[rdremovelen_col]][infra_costtime$Years == year] <- sum(subdf$remove_rd,na.rm=T) 
          infra_costtime[[b_reloclen_col]][infra_costtime$Years == year] <- sum(subdf$relocate_b,na.rm=T)
          infra_costtime[[b_retrofitlen_col]][infra_costtime$Years == year] <- sum(subdf$riprap_b,na.rm=T)
          #infra_costtime[[maintainlen_col]][infra_costtime$Years == year] <- sum(subdf$maintain_hwy,na.rm=T)
          infra_costtime[[total_affected_col]][infra_costtime$Years == year] <- (sum(subdf$total_affected,na.rm=T))
          infra_costtime[[hwy_affected_col]][infra_costtime$Years == year] <- (sum(subdf$hwy_affected,na.rm=T))
          infra_costtime[[nonhwy_affected_col]][infra_costtime$Years == year] <- (sum(subdf$nonhwy_affected,na.rm=T))
          
        }
        
        Retreat_Analysis[[infrastructure_col]][Retreat_Analysis$Years == year] <- 
          sum(infra_costtime[[b_reloc_col]][infra_costtime$Years == year],
              infra_costtime[[b_retrofit_col]][infra_costtime$Years == year],
              infra_costtime[[hwy_reloc_col]][infra_costtime$Years == year],
              infra_costtime[[water_reloc_col]][infra_costtime$Years == year],
              infra_costtime[[emdom_col]][infra_costtime$Years == year],
              infra_costtime[[hwy_riprap_col]][infra_costtime$Years == year],
              infra_costtime[[rd_remove_col]][infra_costtime$Years == year],
              infra_costtime[[riprap_remove_col]][infra_costtime$Years == year]) #,
              #infra_costtime[[maintain_col]][infra_costtime$Years == year], na.rm=T)
        
        Retreat_Analysis[[b_reloc_col]][Retreat_Analysis$Years == year] <- infra_costtime[[b_reloc_col]][infra_costtime$Years == year]
        Retreat_Analysis[[b_retrofit_col]][Retreat_Analysis$Years == year] <- infra_costtime[[b_retrofit_col]][infra_costtime$Years == year]
        Retreat_Analysis[[hwy_reloc_col]][Retreat_Analysis$Years == year] <- infra_costtime[[hwy_reloc_col]][infra_costtime$Years == year]
        Retreat_Analysis[[water_reloc_col]][Retreat_Analysis$Years == year] <- infra_costtime[[water_reloc_col]][infra_costtime$Years == year]
        Retreat_Analysis[[emdom_col]][Retreat_Analysis$Years == year] <- infra_costtime[[emdom_col]][infra_costtime$Years == year]
        Retreat_Analysis[[hwy_riprap_col]][Retreat_Analysis$Years == year] <- infra_costtime[[hwy_riprap_col]][infra_costtime$Years == year]
        Retreat_Analysis[[rd_remove_col]][Retreat_Analysis$Years == year] <- infra_costtime[[rd_remove_col]][infra_costtime$Years == year]
        Retreat_Analysis[[riprap_remove_col]][Retreat_Analysis$Years == year] <- infra_costtime[[riprap_remove_col]][infra_costtime$Years == year]
        #Retreat_Analysis[[maintain_col]][Retreat_Analysis$Years == year] <- infra_costtime[[maintain_col]][infra_costtime$Years == year]
        Retreat_Analysis[[hwylength_col]][Retreat_Analysis$Years == year] <- infra_costtime[[hwylength_col]][infra_costtime$Years == year]
        Retreat_Analysis[[hwyripraplen_col]][Retreat_Analysis$Years == year] <- infra_costtime[[hwyripraplen_col]][infra_costtime$Years == year]
        Retreat_Analysis[[rdremovelen_col]][Retreat_Analysis$Years == year] <- infra_costtime[[rdremovelen_col]][infra_costtime$Years == year]
        Retreat_Analysis[[b_reloclen_col]][Retreat_Analysis$Years == year] <- infra_costtime[[b_reloclen_col]][infra_costtime$Years == year]
        Retreat_Analysis[[b_retrofitlen_col]][Retreat_Analysis$Years == year] <- infra_costtime[[b_retrofitlen_col]][infra_costtime$Years == year]
        #Retreat_Analysis[[maintainlen_col]][Retreat_Analysis$Years == year] <- infra_costtime[[maintainlen_col]][infra_costtime$Years == year]
        Retreat_Analysis[[total_affected_col]][Retreat_Analysis$Years == year] <- infra_costtime[[total_affected_col]][infra_costtime$Years == year]
        Retreat_Analysis[[hwy_affected_col]][Retreat_Analysis$Years == year] <- infra_costtime[[hwy_affected_col]][infra_costtime$Years == year]
        Retreat_Analysis[[nonhwy_affected_col]][Retreat_Analysis$Years == year] <- infra_costtime[[nonhwy_affected_col]][infra_costtime$Years == year]
        }
      }
    }
  }
}
      



