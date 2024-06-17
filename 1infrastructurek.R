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


# Infrastructure Retreat Cost, brought in from GIS calcs
#highway, non-highway (road), bridge

infrademo <- data.frame(matrix(ncol=1,nrow=0))
colnames(infrademo)<- c('ID')
infrademo$ID <- as.character(infrademo$ID)

levels <- c("05","11","20","32")
buffer <- c("","b")
triggers <- c("PF","CE","WF","XA")

for(buff in buffer){
  #import veg files
  infrashp <- st_read(infrastructurefolder,layer=paste0("VEG23",buff))
  infrasdf <- as.data.frame(infrashp)
  
  infrasdf$id <- ifelse(infrasdf$Has_HWY == 0,paste("r", infrasdf$id, sep=""),infrasdf$id)
  infrasdf$id <- ifelse(infrasdf$Has_HWY == 4,paste("rr", infrasdf$id, sep=""),infrasdf$id)
  infrasdf$id <- ifelse(infrasdf$Has_HWY == 5,paste("rb", infrasdf$id, sep=""),infrasdf$id)
  
  infradf <- infrasdf[c('Has_HWY','SS_FID','VEG','Ln_m','id')]
  
  year <- 2023
  
  #reformat and summarize data
  for(trigger in triggers){
    dfsum <- infradf %>%
      group_by(id) %>% 
      summarise(
        #highway
        !!sym(paste0("hwy_",trigger,"_", year, "_", buff)) := sum(Ln_m[Has_HWY == 1]),
        #bridge
        !!sym(paste0("b_",trigger,"_", year, "_", buff)) := sum(Ln_m[Has_HWY == 2]),
        #nonhwy
        !!sym(paste0("rd_",trigger,"_", year, "_", buff)) := sum(Ln_m[Has_HWY == 0]),
        #nonhwy may retreat
        !!sym(paste0("rdretreat_",trigger,"_", year, "_", buff)) := sum(Ln_m[Has_HWY == 4]), #add to either hwy or rd depending on scenario
        #nonhwy bridge may retreat
        !!sym(paste0("rdbretreat_",trigger,"_", year, "_", buff)) := sum(Ln_m[Has_HWY == 5]), #there is one bridge. add to either bridge or demolish
        #seawall / riprap
        !!sym(paste0("seawall_",trigger,"_", year, "_", buff)) := sum(case_when(SS_FID > 0 ~ Ln_m, TRUE ~ 0))
      )
    infrademo <- full_join(infrademo, dfsum, by =c('ID' = 'id')) 
  }
  
  for(level in levels){
    for(trigger in triggers){
      #read in all shapefiles
      infrashp <- st_read(infrastructurefolder,layer=paste0("Road_",trigger,level,buff))
      infrasdf <- as.data.frame(infrashp)
      
      #dataset metadata: 
      #Has_HWY: 0=nonHWY, 1=HWY, 2=Bridge, 4=nonhwy that may retreat, 5=bridge in nonhwy that may retreat
      #SS_FID: 0=does not have shoreline hardening. # = FID corresponding to seawall ID
      #VEG: if SS_FID > 0 and veg=1 then retreat RE. if SS_FID > 0 and veg=2 then retreat TB
      #Ln_m: length of segment in meters
      #id: corresponds to segment. note: it is not unique (need to combine with Has-hwy ID to make it unique)
      
      infrasdf$id <- ifelse(infrasdf$Has_HWY == 0,paste("r", infrasdf$id, sep=""),infrasdf$id)
      infrasdf$id <- ifelse(infrasdf$Has_HWY == 4,paste("rr", infrasdf$id, sep=""),infrasdf$id)
      infrasdf$id <- ifelse(infrasdf$Has_HWY == 5,paste("rb", infrasdf$id, sep=""),infrasdf$id)
      
      infradf <- infrasdf[c('Has_HWY','SS_FID','VEG','Ln_m','id')] 
      
      
      year <- ifelse(level=="05",2030,ifelse(level=='11',2050,ifelse(level=='20',2075,ifelse(level=='32',2100,NA))))
      
      #reformat and summarize data
      dfsum <- infradf %>%
        group_by(id) %>% 
        summarise(
          #highway
          !!sym(paste0("hwy_",trigger,"_", year, "_", buff)) := sum(Ln_m[Has_HWY == 1]),
          #bridge
          !!sym(paste0("b_",trigger,"_", year, "_", buff)) := sum(Ln_m[Has_HWY == 2]),
          #nonhwy
          !!sym(paste0("rd_",trigger,"_", year, "_", buff)) := sum(Ln_m[Has_HWY == 0]),
          #nonhwy may retreat
          !!sym(paste0("rdretreat_",trigger,"_", year, "_", buff)) := sum(Ln_m[Has_HWY == 4]), #add to either hwy or rd depending on scenario
          #nonhwy bridge may retreat
          !!sym(paste0("rdbretreat_",trigger,"_", year, "_", buff)) := sum(Ln_m[Has_HWY == 5]), #there is one bridge. add to either bridge or demolish
          #seawall / riprap
          !!sym(paste0("seawall_",trigger,"_", year, "_", buff)) := sum(case_when(SS_FID > 0 ~ Ln_m, TRUE ~ 0))
        )
      
      infrademo <- full_join(infrademo, dfsum, by =c('ID' = 'id')) 
      
    }
  }
}

#import the community columns from slrxa32b and veg23b
infrashp <- st_read(infrastructurefolder,layer=paste0("Road_XA32b"))
infrasdf <- as.data.frame(infrashp)
infrasdf$id <- ifelse(infrasdf$Has_HWY == 0,paste("r", infrasdf$id, sep=""),infrasdf$id)
infrasdf$id <- ifelse(infrasdf$Has_HWY == 4,paste("rr", infrasdf$id, sep=""),infrasdf$id)
infrasdf$id <- ifelse(infrasdf$Has_HWY == 5,paste("rb", infrasdf$id, sep=""),infrasdf$id)
infracomm <- infrasdf[c('id','ahupuaa','moku',"devplan_","devplan_id","district",'LittrlCell','NewB','Community',"dp","ballottype")] 
infracomm <- infracomm[!duplicated(infracomm[,c('id')]),] #warning: if there are multiple communities for a given ID, this ignores that and picks the first row
infrademo <- left_join(infrademo,infracomm,by=c('ID' = 'id'))

infrashp <- st_read(infrastructurefolder,layer=paste0("VEG23b"))
infrasdf <- as.data.frame(infrashp)
infrasdf$id <- ifelse(infrasdf$Has_HWY == 0,paste("r", infrasdf$id, sep=""),infrasdf$id)
infrasdf$id <- ifelse(infrasdf$Has_HWY == 4,paste("rr", infrasdf$id, sep=""),infrasdf$id)
infrasdf$id <- ifelse(infrasdf$Has_HWY == 5,paste("rb", infrasdf$id, sep=""),infrasdf$id)
infracomm <- infrasdf[c('id','ahupuaa','moku',"devplan_","devplan_id","district",'LittrlCell','NewB','Community',"dp","ballottype")] 
infracomm <- infracomm[!duplicated(infracomm[,c('id')]),] #warning: if there are multiple communities for a given ID, this ignores that and picks the first row
names(infracomm)[1] <- 'ID'
infrademo <- rows_update(infrademo,infracomm,by='ID')




#calculate infrastructure affected over time
infra_retreat <- infrademo %>%
  select(ID,Community,LittrlCell,NewB,district, 
         starts_with("hwy_"), starts_with("b_"), starts_with("rd_"),starts_with("rdretreat_"),
         starts_with("rdbretreat_"),starts_with("seawall")) %>%
  pivot_longer(cols = -c(ID,Community,LittrlCell,NewB,district), names_to = c(".value", "Trigger","Year", "Scenario"), names_sep = "_") %>%
  mutate(Scenario = ifelse(grepl("b$", Scenario), "TB", "RE"),
         Year = as.numeric(gsub("hwy_|b_|rd_", "", Year))
  ) %>%
  arrange(Community,LittrlCell,NewB,district,ID, Scenario, Year) %>%
  group_by(Community,LittrlCell,NewB,district,ID, Scenario) 

#add veg values to trigger values
infra_retreat <- infra_retreat %>%
  group_by(ID, Community,LittrlCell,NewB,district, Trigger, Scenario) %>%
  mutate(
    hwy = ifelse(Year == 2023, hwy, sum(hwy[Year == 2023], na.rm = TRUE) + ifelse(is.na(hwy), 0, hwy)),
    b = ifelse(Year == 2023, b, sum(b[Year == 2023], na.rm = TRUE) + ifelse(is.na(b), 0, b)),
    rd = ifelse(Year == 2023, rd, sum(rd[Year == 2023], na.rm = TRUE) + ifelse(is.na(rd), 0, rd)),
    rdretreat = ifelse(Year == 2023, rdretreat, sum(rdretreat[Year == 2023], na.rm = TRUE) + ifelse(is.na(rdretreat), 0, rdretreat))
  ) %>%
  ungroup()

#add scenario where we treat non-highways as a highway and realign/retreat the road 
rdret <- c(0,1) 
infra_rdretreat <- infra_retreat
infra_retreat$rdret <- 0
infra_rdretreat$rdret <- 1
infra_retreat <- rbind(infra_retreat,infra_rdretreat)

infraIDs <- unique(na.omit(infra_retreat$ID))
scenarios <- unique(na.omit(infra_retreat$Scenario))
triggers <- unique(na.omit(infra_retreat$Trigger))
years <- unique(na.omit(infra_retreat$Year))

infra_retreat[ ,c("new_hwy","total_hwy","new_b","total_b","new_swallhwy","total_swallhwy","total_length","retreatyr",
                  "maintain_hwy","relocate_hwy","relocate_b","riprap_hwy","riprap_b","removeriprap_hwy","remove_rd","removeriprap_rd",
                  "swall_s","riprap_hwy_s","riprap_b_s","riprap_rd_s","maintain_s")] <- NA

#this takes 15 minutes to run :(
for(id in infraIDs){
  for(scenario in scenarios){
    for(trigger in triggers){
      for(rdr in rdret){
        #set up subset dataframe and running totals
        subdf <- subset(infra_retreat, Scenario == scenario & ID == id & Trigger == trigger & rdret == rdr)
        total_hwy <- 0
        total_b <- 0
        total_swallhwy <- 0
        total_length <- 0
        
        #set up road retreat scenario: if retreating residential road, transfer values in $rdretreat to $hwy and $rdbretreat to $b. if not retreating, transfer to $rd
        if(rdr == 1){ #if retreating residential road, transfer values in $rdretreat to $hwy and $rdbretreat to $b
          subdf$hwy <- subdf$hwy + subdf$rdretreat
          subdf$b <- subdf$b + subdf$rdbretreat
        }
        if(rdr == 0){ #if not retreating roads, transfer to $rd
          subdf$rd <- subdf$rd + subdf$rdretreat
        }
        
        for(i in 1:length(years)){
          
          year <- years[i]
          prevyear <- years[i-1]
          
          #calculate new highway and bridge lengths under hazard
          #highway+bridge. retreat occurs when length of highway+bridge under hazard exceeds 1000ft (304.8m)
          hwy <- subdf$hwy[subdf$Year == year]
          prev_hwy <- ifelse(year > 2023,subdf$hwy[subdf$Year == prevyear],0)
          new_hwyy <- ifelse(!is.na(prev_hwy), hwy - prev_hwy, hwy) 
          new_hwy <- pmax(new_hwyy,0,na.rm=T) #here pmax is removing any negative numbers
          b <- subdf$b[subdf$Year == year]
          prev_b <- ifelse(year > 2023,subdf$b[subdf$Year == prevyear],0)
          new_b <- ifelse(!is.na(prev_b), b - prev_b, b)
          prev_swallhwy <- ifelse(year == 2023, subdf$seawall[subdf$Year == year], ifelse(year > 2023,subdf$total_swallhwy[subdf$Year == prevyear],0))
          new_swall_hwy <- ifelse(year==2023 & new_hwy>0, new_hwy-prev_swallhwy,new_hwy) #amount of highway that needs to be riprapped
          
          total_hwy <- sum(new_hwy,total_hwy,na.rm=T)
          total_b <- sum(new_b,total_b, na.rm=T)
          total_swallhwy <- ifelse(year==2023, sum(prev_swallhwy, new_swall_hwy, na.rm=T),sum(new_swall_hwy,total_swallhwy,na.rm=T))
          total_swallhwy_s <-  ifelse(year==2023, sum(prev_swallhwy, new_swall_hwy, na.rm=T),sum(new_swall_hwy,total_swallhwy_s,na.rm=T))
          total_length <- sum(total_length, new_hwy, new_b, na.rm=T)
          
          
          infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & infra_retreat$Trigger == trigger & infra_retreat$rdret == rdr, 
                        'new_hwy'] <- new_hwy
          infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & infra_retreat$Trigger == trigger & infra_retreat$rdret == rdr, 
                        'total_hwy'] <- total_hwy
          infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & infra_retreat$Trigger == trigger & infra_retreat$rdret == rdr, 
                        'new_b'] <- new_b
          infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & infra_retreat$Trigger == trigger & infra_retreat$rdret == rdr, 
                        'total_b'] <- total_b
          infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & infra_retreat$Trigger == trigger & infra_retreat$rdret == rdr, 
                        'new_swallhwy'] <- new_swall_hwy
          infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & infra_retreat$Trigger == trigger & infra_retreat$rdret == rdr, 
                        'total_swallhwy'] <- total_swallhwy
          infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & infra_retreat$Trigger == trigger & infra_retreat$rdret == rdr, 
                        'total_length'] <- total_length
          
          
          
          #if total length is >304.8, then retreat occurs and any subsequent sections of road also immediately retreat
          #relocate_hwy and relocate_b are equal to total_hwy and total_b. seawall/riprap removed is total_swallhwy
          #total_length, total_hwy, total_swallhwy, total_b are reset
          retreatyr <- ifelse(total_length > 304.8, year, NA)
          infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & infra_retreat$Trigger == trigger & infra_retreat$rdret == rdr, 
                        'retreatyr'] <- retreatyr
          
          if(!is.na(retreatyr)){
            infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & infra_retreat$Trigger == trigger & infra_retreat$rdret == rdr, 
                          'relocate_hwy'] <- total_hwy
            infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & infra_retreat$Trigger == trigger & infra_retreat$rdret == rdr, 
                          'relocate_b'] <- total_b
            infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & infra_retreat$Trigger == trigger & infra_retreat$rdret == rdr, 
                          'removeriprap_hwy'] <- total_swallhwy-new_swall_hwy
            
            #don't reset total length. it just continue to sum and any future affected infrastructure immediately relocated
            #total_length <- 0
            total_hwy <- 0
            total_b <- 0
            total_swallhwy <- 0
          }
          
          #if total length is <304.8 (retreat is NA), then riprap occurs. riprap_hwy and riprap_b are equal to new_hwy and new_b.
          # these lengths are also added to the running total_swallhwy to know how much riprap must later be removed
          if(is.na(retreatyr)){
            infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & infra_retreat$Trigger == trigger & infra_retreat$rdret == rdr, 
                          'riprap_hwy'] <- new_swall_hwy #new_hwy
            infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & infra_retreat$Trigger == trigger & infra_retreat$rdret == rdr, 
                          'riprap_b'] <- new_b
            infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & infra_retreat$Trigger == trigger & infra_retreat$rdret == rdr, 
                          'maintain_hwy'] <- total_swallhwy-new_swall_hwy #prev_swallhwy 
            
            #total_swallhwy <- total_swallhwy + new_hwy + new_b
          }
          
          #calculate road removed (all non-hwy road is removed as it becomes under hazard)
          rd <- subdf$rd[subdf$Year == year]
          prev_rd <- ifelse(year > 2023,subdf$rd[subdf$Year == prevyear],0)
          new_rd <- ifelse(!is.na(prev_rd), rd - prev_rd, rd)
          prev_swallrd <- ifelse(year == 2023 & grepl('r',id) & rdr<1, subdf$seawall[subdf$Year == year], 0) #amount of highway that has riprap that needs to be removed
          new_swall_rd <- ifelse(year==2023 & new_rd>0, new_rd-prev_swallrd,new_rd) #amount of highway that needs to be riprapped
          
          infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & infra_retreat$Trigger == trigger & infra_retreat$rdret == rdr, 
                        'remove_rd'] <- new_rd       
          infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & infra_retreat$Trigger == trigger & infra_retreat$rdret == rdr, 
                        'removeriprap_rd'] <- prev_swallrd 
          
          #seawall-stay scenario: if there are any pre-existing seawalls on given highway ID, then there is no retreat or removal of riprap. just riprap any new affected areas
          if(sum(subdf$seawall,na.rm=T)>0){
            #set up seawall-stay indicator
            infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & infra_retreat$Trigger == trigger & infra_retreat$rdret == rdr, 
                          'swall_s'] <- 1
            infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & infra_retreat$Trigger == trigger & infra_retreat$rdret == rdr, 
                          'riprap_hwy_s'] <- new_swall_hwy
            infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & infra_retreat$Trigger == trigger & infra_retreat$rdret == rdr, 
                          'riprap_b_s'] <- new_b
            infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & infra_retreat$Trigger == trigger & infra_retreat$rdret == rdr, 
                          'riprap_rd_s'] <- new_swall_rd
            infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & infra_retreat$Trigger == trigger & infra_retreat$rdret == rdr, 
                          'maintain_s'] <- prev_hwy+prev_rd #total_swallhwy_s-new_swall_hwy
          }
          
          
          
        }
        if(scenario == "RE"){
          #add row for AO
          #get current riprap hwy/rd lengths
          ao_removeriprap_hwy <- subdf$seawall[subdf$Year == 2023]
          
          ao_hwy <- subdf$hwy[subdf$Year == 2100]
          ao_b <- subdf$b[subdf$Year == 2100]
          ao_rd <- subdf$rd[subdf$Year == 2100]
          infracommunity <- subdf$Community[1]
          infralittrlcell <- subdf$LittrlCell[1]
          infradistrict <- subdf$district[1]
          infrabeach <- subdf$NewB[1]
          
          ao_relocate_hwy <- ao_hwy 
          ao_relocate_b <- ao_b
          ao_remove_rd <- ao_rd 
          
          infra_retreat <- infra_retreat %>%
            ungroup() %>%
            add_row(ID=id,Community=infracommunity,LittrlCell=infralittrlcell,district=infradistrict,NewB=infrabeach,
                    Trigger = trigger,Year=2023,Scenario='AO',rdret = rdr,retreatyr=2023,relocate_hwy=ao_relocate_hwy,
                    relocate_b=ao_relocate_b,remove_rd=ao_remove_rd,
                    removeriprap_hwy=ao_removeriprap_hwy,removeriprap_rd=NA) %>% 
            group_by(ID, Scenario) 
        }
      }
    }
  }
}
