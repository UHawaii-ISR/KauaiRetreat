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
  infracomm <- infrasdf[c('id',"devplan_","devplan_id","district",'LittrlCell','Community',"dp","ballottype")] #'ahupuaa','moku',
  infracomm <- infracomm[!duplicated(infracomm[,c('id')]),] #warning: if there are multiple communities for a given ID, this ignores that and picks the first row
  
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
      infracomm <- infrasdf[c('id',"devplan_","devplan_id","district",'LittrlCell','Community',"dp","ballottype")] #'ahupuaa','moku',
      infracomm <- infracomm[!duplicated(infracomm[,c('id')]),] #warning: if there are multiple communities for a given ID, this ignores that and picks the first row
      
      
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

#import the community columns from slrxa and veg23b
infrashp <- st_read(infrastructurefolder,layer=paste0("Road_XA32b"))
infrasdf <- as.data.frame(infrashp)
infrasdf$id <- ifelse(infrasdf$Has_HWY == 0,paste("r", infrasdf$id, sep=""),infrasdf$id)
infrasdf$id <- ifelse(infrasdf$Has_HWY == 4,paste("rr", infrasdf$id, sep=""),infrasdf$id)
infrasdf$id <- ifelse(infrasdf$Has_HWY == 5,paste("rb", infrasdf$id, sep=""),infrasdf$id)
infracomm <- infrasdf[c('id',"devplan_","devplan_id","district",'LittrlCell','Community',"dp","ballottype")] #'ahupuaa','moku',
infracomm <- infracomm[!duplicated(infracomm[,c('id')]),] #warning: if there are multiple communities for a given ID, this ignores that and picks the first row
infrademo <- left_join(infrademo,infracomm,by=c('ID' = 'id'))

infrashp <- st_read(infrastructurefolder,layer=paste0("VEG23b"))
infrasdf <- as.data.frame(infrashp)
infrasdf$id <- ifelse(infrasdf$Has_HWY == 0,paste("r", infrasdf$id, sep=""),infrasdf$id)
infrasdf$id <- ifelse(infrasdf$Has_HWY == 4,paste("rr", infrasdf$id, sep=""),infrasdf$id)
infrasdf$id <- ifelse(infrasdf$Has_HWY == 5,paste("rb", infrasdf$id, sep=""),infrasdf$id)
infracomm <- infrasdf[c('id',"devplan_","devplan_id","district",'LittrlCell','Community',"dp","ballottype")] #'ahupuaa','moku',
infracomm <- infracomm[!duplicated(infracomm[,c('id')]),] #warning: if there are multiple communities for a given ID, this ignores that and picks the first row
names(infracomm)[1] <- 'ID'
infrademo <- rows_update(infrademo,infracomm,by='ID')




#calculate infrastructure affected over time
infra_retreat <- infrademo %>%
  select(ID,Community, starts_with("hwy_"), starts_with("b_"), starts_with("rd_"),starts_with("rdretreat_"),starts_with("rdbretreat_"),starts_with("seawall")) %>%
  pivot_longer(cols = -c(ID,Community), names_to = c(".value", "Trigger","Year", "Scenario"), names_sep = "_") %>%
  mutate(Scenario = ifelse(grepl("b$", Scenario), "TB", "RE"),
         Year = as.numeric(gsub("hwy_|b_|rd_", "", Year))
  ) %>%
  arrange(Community,ID, Scenario, Year) %>%
  group_by(Community,ID, Scenario) 

#add veg values to trigger values
infra_retreat <- infra_retreat %>%
  group_by(ID, Community, Trigger, Scenario) %>%
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
                  "riprap_hwy_s","riprap_b_s","riprap_rd_s","maintain_s")] <- NA

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
          subdf$hwy <- subdf$rdretreat
          subdf$b <- subdf$rdbretreat
        }
        if(rdr == 0){ #if not retreating roads, transfer to $rd
          subdf$rd <- subdf$rdretreat
        }
    
      for(i in 1:length(years)){
        
        year <- years[i]
        prevyear <- years[i-1]
        
        #calculate new highway and bridge lengths under hazard
        #highway+bridge. retreat occurs when length of highway+bridge under hazard exceeds 1000ft (304.8m)
        hwy <- subdf$hwy[subdf$Year == year]
        prev_hwy <- ifelse(year > 2023,subdf$hwy[subdf$Year == prevyear],0)
        new_hwy <- ifelse(!is.na(prev_hwy), hwy - prev_hwy, hwy)
        b <- subdf$b[subdf$Year == year]
        prev_b <- ifelse(year > 2023,subdf$b[subdf$Year == prevyear],0)
        new_b <- ifelse(!is.na(prev_b), b - prev_b, b)
        prev_swallhwy <- ifelse(year == 2023, subdf$seawall[subdf$Year == year], ifelse(year > 2023,subdf$total_swallhwy[subdf$Year == prevyear],0))
        new_swall_hwy <- ifelse(year==2023, new_hwy-prev_swallhwy,new_hwy) #amount of highway that needs to be riprapped
        
        total_hwy <- sum(new_hwy,total_hwy,na.rm=T)
        total_b <- sum(new_b,total_b, na.rm=T)
        total_swallhwy <- ifelse(year==2023, sum(prev_swallhwy, new_swallhwy, na.rm=T),sum(new_swall_hwy,total_swallhwy,na.rm=T))
        total_swallhwy_s <-  ifelse(year==2023, sum(prev_swallhwy, new_swallhwy, na.rm=T),sum(new_swall_hwy,total_swallhwy_s,na.rm=T))
        total_length <- sum(total_length, new_hwy, new_b, na.rm=T)
        
        
        infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & Trigger == trigger & rdret == rdr, 'new_hwy'] <- new_hwy
        infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & Trigger == trigger & rdret == rdr, 'total_hwy'] <- total_hwy
        infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & Trigger == trigger & rdret == rdr, 'new_b'] <- new_b
        infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & Trigger == trigger & rdret == rdr, 'total_b'] <- total_b
        infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & Trigger == trigger & rdret == rdr, 'new_swallhwy'] <- new_swall_hwy
        infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & Trigger == trigger & rdret == rdr, 'total_swallhwy'] <- total_swallhwy
        infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & Trigger == trigger & rdret == rdr, 'total_length'] <- total_length
        
        #seawall-stay scenario: if there are any pre-existing seawalls on given highway ID, then there is no retreat or removal of riprap. just riprap any new affected areas
        if(sum(subdf$seawall,na.rm=T)>0){
          infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & Trigger == trigger & rdret == rdr, 'riprap_hwy_s'] <- new_swall_hwy
          infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & Trigger == trigger & rdret == rdr, 'riprap_b_s'] <- new_b
          infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & Trigger == trigger & rdret == rdr, 'maintain_s'] <- total_swallhwy_s-new_swall_hwy
        }
        
        #if total length is >304.8, then retreat occurs and any subsequent sections of road also immediately retreat
        #relocate_hwy and relocate_b are equal to total_hwy and total_b. seawall/riprap removed is total_swallhwy
        #total_length, total_hwy, total_swallhwy, total_b are reset
        retreatyr <- ifelse(total_length > 304.8, year, NA)
        infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & Trigger == trigger & rdret == rdr, 'retreatyr'] <- retreatyr
        
        if(!is.na(retreatyr)){
          infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & Trigger == trigger & rdret == rdr, 'relocate_hwy'] <- total_hwy
          infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & Trigger == trigger & rdret == rdr, 'relocate_b'] <- total_b
          infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & Trigger == trigger & rdret == rdr, 'removeriprap_hwy'] <- total_swallhwy
          
          #don't reset total length. it just continue to sum and any future affected infrastructure immediately relocated
          #total_length <- 0
          total_hwy <- 0
          total_b <- 0
          total_swallhwy <- 0
        }
        
        #if total length is <304.8 (retreat is NA), then riprap occurs. riprap_hwy and riprap_b are equal to new_riprap_hwy and new_b.
        # these lengths are also added to the running total_swallhwy to know how much riprap must later be removed
        if(is.na(retreatyr)){
          infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & Trigger == trigger & rdret == rdr, 'riprap_hwy'] <- new_riprap_hwy
          infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & Trigger == trigger & rdret == rdr, 'riprap_b'] <- new_b
          infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & Trigger == trigger & rdret == rdr, 'maintain_hwy'] <- prev_swallhwy
          
          total_swallhwy <- total_swallhwy + new_hwy + new_b
        }
        
        #calculate road removed (all non-hwy road is removed as it becomes under hazard)
        rd <- subdf$rd[subdf$Year == year]
        prev_rd <- ifelse(year > 2023,subdf$rd[subdf$Year == prevyear],0)
        new_rd <- ifelse(!is.na(prev_rd), rd - prev_rd, rd)
        prev_swallrd <- ifelse(year == 2023, subdf$seawall[subdf$Year == year], 0) #amount of highway that has riprap that needs to be removed
        
        infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & Trigger == trigger & rdret == rdr, 'remove_rd'] <- new_rd       
        infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & Trigger == trigger & rdret == rdr, 'removeriprap_rd'] <- prev_swallrd 
        
        #seawall-stay scenario: if there are any pre-existing seawalls on given highway ID, then there is no retreat or removal of riprap. just riprap any new affected areas
        if(sum(subdf$seawall,na.rm=T)>0){
          infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & Trigger == trigger & rdret == rdr, 'riprap_rd_s'] <- new_rd
          infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id & Trigger == trigger & rdret == rdr, 'maintain_s'] <- prev_rd
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
        
        ao_relocate_hwy <- ao_hwy 
        ao_relocate_b <- ao_b
        ao_remove_rd <- ao_rd 
        
        infra_retreat <- infra_retreat %>%
          ungroup() %>%
          add_row(ID=id,Year=2023,Community=infracommunity,Scenario='AO',retreatyr=2023,relocate_hwy=ao_relocate_hwy,relocate_b=ao_relocate_b,remove_rd=ao_remove_rd,
                  removeriprap_hwy=ao_removeriprap_hwy,removeriprap_rd=NA) %>% 
          group_by(ID, Scenario) 
      }
      }
    }
  }
}

#filter to just community of interest
if(!is.na(communityfilter)){
  infra_retreat <- infradf[infradf[[communitytype]]==communityfilter,] 
  }




#calculate cost for eminent domain

#open assessors data for full island
kauaiassessorsshp <- st_read(assessorsfile) 
kauaiassessorsdf <- as.data.frame(kauaiassessorsshp)

#calculate cost per area
#APRTOTMKT = Appraised Total Market Value (use for total value)
#Shape__Are = sq meters. assume 10 meter width is necessary for a road realignment
kauaiassessorsdf$costarea <- kauaiassessorsdf$APRTOTMKT / kauaiassessorsdf$Shape__Are
aveappraisedkauai <- mean(kauaiassessorsdf$costarea,na.rm=T) #this is cost per sq meter
emdom_hwy <- aveappraisedkauai*10 #this is cost per 1 meter length of road, assuming road is 10 meters wide






#add infrastructure demo calcs to Retreat_Analysis df and breakdown in infra_costtime

years <- c(2023, 2026, 2030, 2040, 2050, 2062, 2075, 2087, 2100)
scenarios <- c('AO','TB','RE')

infra_costtime <- data.frame(
  Years = years)

for (year in years) {
  for(seawall in seawalls){
    for(trigger in triggers){
      ### ADD ANOTHER FOR LOOP FOR RDRET
      
      #### FOR SEAWALL SCENARIO: FIRST SUM VALUES & COSTS IN SEAWALL COLUMNS, THEN FOR ROWS WHERE THERE IS NO VALUE IN SEAWALL COLUMN, CONTINUE CALCULATION AS BELOW
      
      #highways, bridges, roads
      bridge_reloc <- 337992 #per meter retreat    
      bridge_riprap <- 71910 #per meter retrofit
      highway_reloc <- 259489 #per meter realignment
      water_reloc <- 5686 #per meter removal replacement water mains
      highway_riprap <- 37069 #per meter hardening
      road_remove <- 34 #per meter
      emdom_hwy #per meter length of road
      riprap_remove <- 14764 #per meter 
      maintain <- 11814 #per meter
      
      for(scenario in scenarios){
        subdf <- subset(infra_retreat, Scenario == scenario & Year == year & Trigger == trigger & rdret == rdr)
        infrastructure_col <- paste0("infrastructure_",scenario,seawall,trigger)
        b_reloc_col <- paste0("bridgerelocate",scenario,seawall,trigger)
        b_retrofit_col <- paste0("bridgeretrofit",scenario,seawall,trigger)
        hwy_reloc_col <- paste0("hwyrelocate",scenario,seawall,trigger)
        water_reloc_col <- paste0("waterrelocate",scenario,seawall,trigger)
        emdom_col <- paste0("emdom",scenario,seawall,trigger)
        hwy_riprap_col <- paste0("hwyriprap",scenario,seawall,trigger)
        rd_remove_col <- paste0("rdremove",scenario,seawall,trigger)
        riprap_remove_col <- paste0("riprapremove",scenario,seawall,trigger)
        #wastewater_remove_col <- paste0("waterremove",scenario,seawall,trigger)
        maintain_col <- paste0("maintain",scenario,seawall,trigger)
        
        if(nrow(subdf)==0){
          infra_costtime[[b_reloc_col]][infra_costtime$Years == year] <- 0
          infra_costtime[[b_retrofit_col]][infra_costtime$Years == year] <- 0
          infra_costtime[[hwy_reloc_col]][infra_costtime$Years == year] <- 0
          infra_costtime[[water_reloc_col]][infra_costtime$Years == year] <- 0
          infra_costtime[[emdom_col]][infra_costtime$Years == year] <- 0
          infra_costtime[[hwy_riprap_col]][infra_costtime$Years == year] <- 0
          infra_costtime[[rd_remove_col]][infra_costtime$Years == year] <- 0
          infra_costtime[[riprap_remove_col]][infra_costtime$Years == year] <- 0
          #infra_costtime[[wastewater_remove_col]][infra_costtime$Years == year] <- 0
          infra_costtime[[maintain_col]][infra_costtime$Years == year] <- 0
          
          
        } else{
          infra_costtime[[b_reloc_col]][infra_costtime$Years == year] <- sum(subdf$relocate_b,na.rm=T)*bridge_reloc
          infra_costtime[[b_retrofit_col]][infra_costtime$Years == year] <- sum(subdf$riprap_b,na.rm=T)*bridge_riprap
          infra_costtime[[hwy_reloc_col]][infra_costtime$Years == year] <- sum(subdf$relocate_hwy,na.rm=T)*highway_reloc
          infra_costtime[[water_reloc_col]][infra_costtime$Years == year] <- sum(subdf$relocate_hwy,na.rm=T)*water_reloc
          infra_costtime[[emdom_col]][infra_costtime$Years == year] <- sum(subdf$relocate_hwy,na.rm=T)*emdom_hwy
          infra_costtime[[hwy_riprap_col]][infra_costtime$Years == year] <- sum(subdf$riprap_hwy,na.rm=T)*highway_riprap
          infra_costtime[[rd_remove_col]][infra_costtime$Years == year] <- sum(subdf$remove_rd,na.rm=T)*road_remove
          infra_costtime[[riprap_remove_col]][infra_costtime$Years == year] <- sum(subdf$removeriprap_rd,na.rm=T)*riprap_remove
          #infra_costtime[[wastewater_remove_col]][infra_costtime$Years == year] <- sum(subdf$removewater,na.rm=T)*wastewater_remove
          infra_costtime[[maintain_col]][infra_costtime$Years == year] <- sum(subdf$maintain_hwy,na.rm=T)*maintain
          
        }
        
        Retreat_Analysis[[infrastructure_col]][Retreat_Analysis$Years == year] <- 
          sum(infra_costtime[[b_reloc_col]][infra_costtime$Years == year],
              infra_costtime[[b_retrofit_col]][infra_costtime$Years == year],
              infra_costtime[[hwy_reloc_col]][infra_costtime$Years == year],
              infra_costtime[[water_reloc_col]][infra_costtime$Years == year],
              infra_costtime[[emdom_col]][infra_costtime$Years == year],
              infra_costtime[[hwy_riprap_col]][infra_costtime$Years == year],
              infra_costtime[[rd_remove_col]][infra_costtime$Years == year],
              infra_costtime[[riprap_remove_col]][infra_costtime$Years == year],
              #infra_costtime[[wastewater_remove_col]][infra_costtime$Years == year],
              infra_costtime[[maintain_col]][infra_costtime$Years == year], na.rm=T)
        
      }
    }
  }
}
      



## QAQC infra

#infra_totalaffected <- aggregate(infra_retreat$riprap_hwy, by=list(infra_retreat$Year,infra_retreat$Scenario),FUN=sum,na.rm=T)

#aggregate(infra_retreat$riprap_hwy, by=list(infra_retreat$Year,infra_retreat$Scenario),FUN=sum,na.rm=T)


#infra_retreat$relocate_hwy,infra_retreat$relocate_b,infra_retreat$riprap_hwy,infra_retreat$riprap_b,infra_retreat$removeriprap_hwy,
#infra_retreat$remove_rd,infra_retreat$removeriprap_rd

