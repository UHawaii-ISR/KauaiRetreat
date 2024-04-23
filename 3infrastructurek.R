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
infrademo$Community <- as.character(infrademo$Community)

levels <- c("05","11","20","32")
buffer <- c("","b")
triggers <- c("XA","WF","PF","CE")

for(level in levels){
  for(buff in buffer){
    for(trigger in triggers){
      #read in all shapefiles
      infrashp <- st_read(infrastructurefolder,layer=paste0(trigger,level,buff))
      infradf <- as.data.frame(infrashp)
      infradf <- infradf[c('Has_HWY','SS_FID','VEG','Ln_m','id')]
      
      #dataset metadata: 
      #Has_HWY: 0=nonHWY, 1=HWY, 2=Bridge, 4=nonhwy that may retreat, 5=bridge in nonhwy that may retreat
      #SS_FID: 0=does not have shoreline hardening. # = FID corresponding to seawall ID
      #VEG: if SS_FID > 0 and veg=1 then retreat RE. if SS_FID > 0 and veg=2 then retreat TB
      #Ln_m: length of segment in meters
      #id: corresponds to segment. note: it is not unique (need to combine with Has-hwy ID to make it unique)
      
      infradf$id <- ifelse(infradf$Has_HWY == 0,paste("r", infradf$id, sep=""),infradf$Has_HWY)
      infradf$id <- ifelse(infradf$Has_HWY == 4,paste("r", infradf$id, sep=""),infradf$Has_HWY)
      infradf$id <- ifelse(infradf$Has_HWY == 5,paste("r", infradf$id, sep=""),infradf$Has_HWY)
      
      year <- ifelse(level=="05",2030,ifelse(level=='11',2050,ifelse(level=='20',2075,ifelse(level=='32',2100,NA))))
      
      #highway
      dfsum <- hwydf %>%
        group_by(Community,id) %>%
        summarise(
          !!sym(paste0("hwy_", year, "_", buff)) := sum(!!sym(paste0('Ln_m_CE', level, buff))),
          !!sym(paste0("seawallhwy_", year, "_", buff)) := sum(case_when(SS_FID_1 > 0 ~ !!sym(paste0('Ln_m_CE', level, buff)), TRUE ~ 0))
        )
      
      infrademo <- full_join(infrademo, dfsum, by =c('ID' = 'id','Community' = 'Community'))
      
      #bridge
      dfsum <- bdf %>%
        group_by(Community,id) %>%
        summarise(!!sym(paste0("b_",year,"_",buff)) := sum(!!sym(paste0('Ln_m_CE',level,buff))))
      
      infrademo <- full_join(infrademo, dfsum, by =c('ID' = 'id','Community' = 'Community'))
      
      #road (non-highway)
      #column for nonHWY is "realign" 0= ignore 1= realign 2= remove
      dfsum <- rddf %>%
        group_by(Community,id) %>%
        summarise(
          !!sym(paste0("rd_", year, "_", buff)) := sum(!!sym(paste0('Ln_m_CE', level, buff))),
          !!sym(paste0("seawallrd_", year, "_", buff)) := sum(case_when(SS_FID_1 > 0 ~ !!sym(paste0('Ln_m_CE', level, buff)), TRUE ~ 0))
        )
      
      infrademo <- full_join(infrademo, dfsum, by =c('ID' = 'id','Community' = 'Community'))
      
      # sum the seawall columns together (from road seawalls and highway seawalls)
      infrademo[[paste0("seawall_", year, "_", buff)]] <- rowSums(infrademo[, c(paste0("seawallrd_", year, "_", buff), 
                                                                                paste0("seawallhwy_", year, "_", buff))], na.rm = TRUE)
    }
  }
}


#highway+bridge. retreat occurs when length of highway+bridge under CE exceeds 1000ft (304.8m)
infra_retreat <- infrademo %>%
  select(ID,Community, starts_with("hwy_"), starts_with("b_"), starts_with("rd_"),starts_with("seawall")) %>%
  pivot_longer(cols = -c(ID,Community), names_to = c(".value", "Year", "Scenario"), names_sep = "_") %>%
  mutate(Scenario = ifelse(grepl("b$", Scenario), "TB", "RE"),
         Year = as.numeric(gsub("hwy_|b_|rd_", "", Year))
  ) %>%
  arrange(Community,ID, Scenario, Year) %>%
  group_by(Community,ID, Scenario) 

infraIDs <- unique(na.omit(infra_retreat$ID))
scenarios <- unique(na.omit(infra_retreat$Scenario))
years <- unique(na.omit(infra_retreat$Year))

infra_retreat[ ,c("new_hwy","total_hwy","new_b","total_b","new_swallhwy","total_swallhwy","total_length","retreatyr",
                  "maintain_hwy","relocate_hwy","relocate_b","riprap_hwy","riprap_b","removeriprap_hwy","remove_rd","removeriprap_rd")] <- NA

for(id in infraIDs){
  for(scenario in scenarios){
    
    #set up subset dataframe and running totals
    subdf <- subset(infra_retreat, Scenario == scenario & ID == id)
    total_hwy <- 0
    total_b <- 0
    total_swallhwy <- 0
    total_length <- 0
    
    for(i in 1:length(years)){
      year <- years[i]
      prevyear <- years[i-1]
      
      #calculate new highway and bridge lengths under hazard
      hwy <- subdf$hwy[subdf$Year == year]
      prev_hwy <- ifelse(year > 2030,subdf$hwy[subdf$Year == prevyear],0)
      new_hwy <- ifelse(!is.na(prev_hwy), hwy - prev_hwy, hwy)
      b <- subdf$b[subdf$Year == year]
      prev_b <- ifelse(year > 2030,subdf$b[subdf$Year == prevyear],0)
      new_b <- ifelse(!is.na(prev_b), b - prev_b, b)
      swallhwy <- subdf$seawallhwy[subdf$Year == year]
      prev_swallhwy <- ifelse(year > 2030,subdf$seawallhwy[subdf$Year == prevyear],0)
      new_swallhwy <- ifelse(!is.na(prev_swallhwy), swallhwy - prev_swallhwy, swallhwy) #the amount of highway already riprap
      new_riprap_hwy <- ifelse(!is.na(new_swallhwy),new_hwy - new_swallhwy,new_hwy) #amount of highway that needs to be riprapped
      
      total_hwy <- sum(new_hwy,total_hwy,na.rm=T)
      total_b <- sum(new_b,total_b, na.rm=T)
      total_swallhwy <- sum(new_swallhwy,total_swallhwy,na.rm=T)
      total_length <- sum(total_length, new_hwy, new_b, na.rm=T)
      
      
      infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'new_hwy'] <- new_hwy
      infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'total_hwy'] <- total_hwy
      infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'new_b'] <- new_b
      infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'total_b'] <- total_b
      infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'new_swallhwy'] <- new_swallhwy
      infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'total_swallhwy'] <- total_swallhwy
      infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'total_length'] <- total_length
      
      #if total length is >304.8, then retreat occurs and any subsequent sections of road also immediately retreat
      #relocate_hwy and relocate_b are equal to total_hwy and total_b. seawall/riprap removed is total_swallhwy
      #total_length, total_hwy, total_swallhwy, total_b are reset
      retreatyr <- ifelse(total_length > 304.8, year, NA)
      infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'retreatyr'] <- retreatyr
      
      if(!is.na(retreatyr)){
        infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'relocate_hwy'] <- total_hwy
        infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'relocate_b'] <- total_b
        infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'removeriprap_hwy'] <- total_swallhwy
        
        #don't reset total length. it just continue to sum and any future affected infrastructure immediately relocated
        #total_length <- 0
        total_hwy <- 0
        total_b <- 0
        total_swallhwy <- 0
      }
      
      #if total length is <304.8 (retreat is NA), then riprap occurs. riprap_hwy and riprap_b are equal to new_riprap_hwy and new_b.
      # these lengths are also added to the running total_swallhwy to know how much riprap must later be removed
      if(is.na(retreatyr)){
        infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'riprap_hwy'] <- new_riprap_hwy
        infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'riprap_b'] <- new_b
        infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'maintain_hwy'] <- new_swallhwy
        
        total_swallhwy <- total_swallhwy + new_hwy + new_b
      }
      
      #calculate road removed (all non-hwy road is removed as it becomes under hazard)
      rd <- subdf$rd[subdf$Year == year]
      prev_rd <- ifelse(year > 2030,subdf$rd[subdf$Year == prevyear],0)
      new_rd <- ifelse(!is.na(prev_rd), rd - prev_rd, rd)
      swallrd <- subdf$seawallrd[subdf$Year == year]
      prev_swallrd <- ifelse(year > 2030,subdf$seawallrd[subdf$Year == prevyear],0)
      new_swallrd <- ifelse(!is.na(prev_swallrd), swallrd - prev_swallrd, swallrd)
      
      infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'remove_rd'] <- new_rd       
      infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'removeriprap_rd'] <- new_swallrd 
    }
    if(scenario == "RE"){
      #add row for AO
      #get current riprap hwy/rd lengths
      ao_removeriprap_hwy <- subdf$seawallhwy[subdf$Year == 2100]
      ao_removeriprap_rd <- subdf$seawallrd[subdf$Year == 2100]
      
      ao_hwy <- subdf$hwy[subdf$Year == 2100]
      ao_b <- subdf$b[subdf$Year == 2100]
      ao_rd <- subdf$rd[subdf$Year == 2100]
      infracommunity <- subdf$Community[1]
      
      ao_relocate_hwy <- ao_hwy #ifelse(!is.na(ao_removeriprap_hwy), ao_hwy - ao_removeriprap_hwy, ao_hwy) # this is for keeping seawall scenario
      ao_relocate_b <- ao_b
      ao_remove_rd <- ao_rd #ifelse(!is.na(ao_removeriprap_rd), ao_rd - ao_removeriprap_rd, ao_rd)
      
      infra_retreat <- infra_retreat %>%
        ungroup() %>%
        add_row(ID=id,Year=2023,Community=infracommunity,Scenario='AO',retreatyr=2023,relocate_hwy=ao_relocate_hwy,relocate_b=ao_relocate_b,remove_rd=ao_remove_rd,
                removeriprap_hwy=ao_removeriprap_hwy,removeriprap_rd=ao_removeriprap_rd) %>% 
        group_by(ID, Scenario) 
    }
  }
}

#filter to just community of interest
if(!is.na(communityfilter)){
  infra_retreat <- subset(infra_retreat, Community==communityfilter)
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




#Seawall demolition

#seawall infrastructure removal (seawalls in front of parks)**** are there any cases of this in kauai where I'm removing those rows?
#If “direct” seawall for parcel, when first parcel retreats, entire seawall length is demo’d
#If “indirect” seawall, seawall demo when road retreat/remove

#seawall dataframe with just relevant columns
seawalldemodf <- clean_retreat_calcs[,c('TMK','SEAWALL_DIRECT','SEAWALL_INDIRECT','SEAWALLID','SEAWALL_LEN_M',
                                      'year_AO_tCE','year_AO_tXA','year_AO_tWF','year_AO_tPF',
                                      'year_TB_tCE','year_TB_tXA','year_TB_tWF','year_TB_tPF',
                                      'year_RE_tCE','year_RE_tXA','year_RE_tWF','year_RE_tPF')]

seawallIDs <- unique(na.omit(seawalldemodf$SEAWALLID))

seawalldemo <- data.frame(matrix(ncol=14,nrow=0))
colnames(seawalldemo) <- c('SEAWALLID','SEAWALL_LEN_M',
              'year_AO_tCE','year_AO_tXA','year_AO_tWF','year_AO_tPF',
              'year_TB_tCE','year_TB_tXA','year_TB_tWF','year_TB_tPF',
              'year_RE_tCE','year_RE_tXA','year_RE_tWF','year_RE_tPF')

for (seawallid in seawallIDs) {
  
  #subset to single seawallID
  seawallIDdf <- filter(seawalldemodf, SEAWALLID == seawallid)
  
  #get the full length
  seawalllength <- sum(unique(seawallIDdf$SEAWALL_LEN_M),na.rm=T)
  
  #subset to just those that are direct seawall parcels
  seawallIDdir <- filter(seawallIDdf, SEAWALL_DIRECT == 1)
  
  #if there are no direct seawall parcels, seawall is not demo'd ** should be when road is demo'd. need this data
  if(nrow(seawallIDdir) == 0){
    seawalldemo <- seawalldemo %>%
      add_row(SEAWALLID = seawallid,SEAWALL_LEN_M = seawalllength,
              year_AO_tCE = NA, year_AO_tXA = NA, year_AO_tWF = NA, year_AO_tPF = NA,
              year_TB_tCE = NA, year_TB_tXA = NA, year_TB_tWF = NA, year_TB_tPF = NA,
              year_RE_tCE = NA, year_RE_tXA = NA, year_RE_tWF = NA, year_RE_tPF = NA)
  } else{
    seawalldemo <- seawalldemo %>%
      add_row(SEAWALLID = seawallid,SEAWALL_LEN_M = seawalllength,
              year_AO_tCE = NA, year_AO_tXA = NA, year_AO_tWF = NA, year_AO_tPF = NA,
              year_TB_tCE = NA, year_TB_tXA = NA, year_TB_tWF = NA, year_TB_tPF = NA,
              year_RE_tCE = NA, year_RE_tXA = NA, year_RE_tWF = NA, year_RE_tPF = NA)
    for(trigger in triggers){
      #else find earliest direct parcel retreat under each trigger. this is when entire seawall is demo'd
      yearAO_col <- paste0("year_AO_t",trigger)
      yearTB_col <- paste0("year_TB_t",trigger)
      yearRE_col <- paste0("year_RE_t",trigger)
      
      AO_matching_rows <- seawallIDdir[[yearAO_col]]
      TB_matching_rows <- seawallIDdir[[yearTB_col]] 
      RE_matching_rows <- seawallIDdir[[yearRE_col]] 
      
      ifelse(is.na(AO_matching_rows) == T, seawalldemo[[yearAO_col]][seawalldemo$SEAWALLID == seawallid] <- NA,
             seawalldemo[[yearAO_col]][seawalldemo$SEAWALLID == seawallid] <- min(AO_matching_rows, na.rm = TRUE))
      ifelse(is.na(TB_matching_rows) == T, seawalldemo[[yearTB_col]][seawalldemo$SEAWALLID == seawallid] <- NA,
             seawalldemo[[yearTB_col]][seawalldemo$SEAWALLID == seawallid] <- min(TB_matching_rows, na.rm = TRUE))
      ifelse(is.na(RE_matching_rows) == T, seawalldemo[[yearRE_col]][seawalldemo$SEAWALLID == seawallid] <- NA,
             seawalldemo[[yearRE_col]][seawalldemo$SEAWALLID == seawallid] <- min(RE_matching_rows, na.rm = TRUE))
    }
  }
}



#add infrastructure demo calcs to Retreat_Analysis df and breakdown in infra_costtime

years <- c(2023, 2026, 2030, 2040, 2050, 2062, 2075, 2087, 2100)
scenarios <- c('AO','TB','RE')

infra_costtime <- data.frame(
  Years = years)

for (year in years) {
  for(seawall in seawalls){
    for(trigger in triggers){
      
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
        subdf <- subset(infra_retreat, Scenario == scenario & Year == year)
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
      
      #seawalls
      demolition_seawall <- ifelse(seawall == "_",13123,0) # conditional seawall demolition depending on scenario. $4000/ft ($13123/m) for inaccessible seawall e.g. residential area)
      
      if(seawall == '_'){
        yearAO_col <- paste0("year_AO",seawall,"t",trigger)
        yearTB_col <- paste0("year_TB",seawall,"t",trigger)
        yearRE_col <- paste0("year_RE",seawall,"t",trigger)
        
        AO_matching_rows <- seawalldemo[[yearAO_col]] == year
        TB_matching_rows <- seawalldemo[[yearTB_col]] == year
        RE_matching_rows <- seawalldemo[[yearRE_col]] == year

        column_name <- paste0("SEAWALL_LEN_M")
        
        #AO
        seawall_col <- paste0("seawall_AO",seawall,trigger)
        seawallm <- sum(seawalldemo[[column_name]][AO_matching_rows], na.rm = TRUE)
        Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] <- 
          ifelse(year==2023,seawallm*demolition_seawall,0)
        
        #TB
        seawall_col <- paste0("seawall_TB",seawall,trigger)
        seawallm <- sum(seawalldemo[[column_name]][TB_matching_rows], na.rm = TRUE)
        Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] <- 
          seawallm*demolition_seawall
        
        #RE
        seawall_col <- paste0("seawall_RE",seawall,trigger)
        seawallm <- sum(seawalldemo[[column_name]][RE_matching_rows], na.rm = TRUE)
        Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] <- 
          seawallm*demolition_seawall
        
      }
      
      if(seawall == '_s_'){
        seawall_col <- paste0("seawall_AO",seawall,trigger)
        Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] <- 0
        
        seawall_col <- paste0("seawall_TB",seawall,trigger)
        Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] <- 0
        
        seawall_col <- paste0("seawall_RE",seawall,trigger)
        Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] <- 0
      }
    }
  }
}
      



## QAQC infra

infra_totalaffected <- aggregate(infra_retreat$riprap_hwy, by=list(infra_retreat$Year,infra_retreat$Scenario),FUN=sum,na.rm=T)

aggregate(infra_retreat$riprap_hwy, by=list(infra_retreat$Year,infra_retreat$Scenario),FUN=sum,na.rm=T)


#infra_retreat$relocate_hwy,infra_retreat$relocate_b,infra_retreat$riprap_hwy,infra_retreat$riprap_b,infra_retreat$removeriprap_hwy,
#infra_retreat$remove_rd,infra_retreat$removeriprap_rd

