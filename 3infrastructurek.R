#naming standard for sub-scenarios: RE_s_tXA_lCE_bv1_chi (approach = AO, trigger = XA, land transfer = CE, building value = 1,cleanup cost = high, seawall = stay)

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(data.table)
setwd("F:/slr/kauai/kauai_retreat_code/")


# Disable scientific notation
options(scipen = 999)


# Infrastructure Retreat Cost, brought in from GIS calcs
#highway, non-highway (road), bridge

infrademo <- data.frame(matrix(ncol=1,nrow=0))
colnames(infrademo)<- c('ID')
infrademo$ID <- as.character(infrademo$ID)

levels <- c("05","11","20","32")
buffer <- c("","b")

for(level in levels){
  for(buff in buffer){
    #read in all shapefiles
    hwyshp <- st_read("F:/slr/kauai/HWY/HWY",layer=paste0("HWY_CE",level,buff))
    hwydf <- as.data.frame(hwyshp)
    bshp<- st_read("F:/slr/kauai/Bridge/Bridge",layer=paste0("Bri_CE",level,buff))
    bdf <- as.data.frame(bshp)
    rdshp <- st_read("F:/slr/kauai/nonHWY/nonHWY",layer=paste0("nonHWY_CE",level,buff))
    rddf <- as.data.frame(rdshp)
    rddf$id_12 <- paste("r", rddf$id, sep="")
    
    year <- ifelse(level=="05",2030,ifelse(level=='11',2050,ifelse(level=='20',2075,ifelse(level=='32',2100,NA))))
    
    ce <- ifelse(buff=="b","E","CE")
    
    #highway
    dfsum <- hwydf %>%
      group_by(id_12) %>%
      summarise(!!sym(paste0("hwy_",year,"_",buff)) := sum(!!sym(paste0('Ln_m_',ce,level,buff))))
    
    infrademo <- full_join(infrademo, dfsum, by =c('ID' = 'id_12'))
    
    #bridge
    dfsum <- bdf %>%
      group_by(id_12) %>%
      summarise(!!sym(paste0("b_",year,"_",buff)) := sum(!!sym(paste0('Ln_m_',ce,level,buff))))
    
    infrademo <- full_join(infrademo, dfsum, by =c('ID' = 'id_12'))
    
    #road (non-highway)
    dfsum <- rddf %>%
      group_by(id_12) %>%
      summarise(!!sym(paste0("rd_",year,"_",buff)) := sum(!!sym(paste0('Ln_m_',ce,level,buff))))
    
    infrademo <- full_join(infrademo, dfsum, by =c('ID' = 'id_12'))
  }
}


#highway+bridge. retreat occurs when length of highway+bridge under CE exceeds 1000ft (304.8m)
infra_retreat <- infrademo %>%
  select(ID, starts_with("hwy_"), starts_with("b_"), starts_with("rd_")) %>%
  pivot_longer(cols = -ID, names_to = c(".value", "Year", "Scenario"), names_sep = "_") %>%
  mutate(Scenario = ifelse(grepl("b$", Scenario), "TB", "RE"),
         Year = as.numeric(gsub("hwy_|b_|rd_", "", Year))
  ) %>%
  arrange(ID, Scenario, Year) %>%
  group_by(ID, Scenario) 

infraIDs <- unique(na.omit(infra_retreat$ID))
scenarios <- unique(na.omit(infra_retreat$Scenario))
years <- unique(na.omit(infra_retreat$Year))

infra_retreat[ ,c("new_hwy","total_hwy","new_b","total_b","total_length","retreatyr",
                  "relocate_hwy","relocate_b","riprap_hwy","riprap_b","remove_rd")] <- NA

for(id in infraIDs){
  for(scenario in scenarios){
    
    #set up subset dataframe and running totals
    subdf <- subset(infra_retreat, Scenario == scenario & ID == id)
    total_hwy <- 0
    total_b <- 0
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
      
      total_hwy <- sum(new_hwy,total_hwy,na.rm=T)
      total_b <- sum(new_b,total_b, na.rm=T)
      total_length <- sum(total_hwy, total_b, na.rm=T)
      
      
      infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'new_hwy'] <- new_hwy
      infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'total_hwy'] <- total_hwy
      infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'new_b'] <- new_b
      infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'total_b'] <- total_b
      infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'total_length'] <- total_length
      
      #if total length is >305, then retreat occurs. relocate_hwy and relocate_b are equal to total_hwy and total_b. total_length, total_hwy, and total_b are reset
      retreatyr <- ifelse(total_length > 305, year, NA)
      infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'retreatyr'] <- retreatyr
      
      if(!is.na(retreatyr)){
        infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'relocate_hwy'] <- total_hwy
        infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'relocate_b'] <- total_b
        
        total_length <- 0
        total_hwy <- 0
        total_b <- 0
      }
      
      #if total length is <305 (retreat is NA), then riprap occurs. riprap_hwy and riprap_b are equal to new_hwy and new_b
      if(is.na(retreatyr)){
        infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'riprap_hwy'] <- new_hwy
        infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'riprap_b'] <- new_b
      }
      
      #calculate road removed (all non-hwy road is removed as it becomes under hazard)
      rd <- subdf$rd[subdf$Year == year]
      prev_rd <- ifelse(year > 2030,subdf$rd[subdf$Year == prevyear],0)
      new_rd <- ifelse(!is.na(prev_rd), rd - prev_rd, rd)
      
      infra_retreat[infra_retreat$Year == year & infra_retreat$Scenario == scenario & infra_retreat$ID == id, 'remove_rd'] <- new_rd
    }
    if(scenario == "RE"){
      #add row for AO
      ao_hwy <- subdf$hwy[subdf$Year == 2100]
      ao_b <- subdf$b[subdf$Year == 2100]
      ao_rd <- subdf$rd[subdf$Year == 2100]
      
      
      infra_retreat <- infra_retreat %>%
        ungroup() %>%
        add_row(ID=id,Year=2023,Scenario='AO',retreatyr=2023,relocate_hwy=ao_hwy,relocate_b=ao_b,remove_rd=ao_rd) %>% 
        group_by(ID, Scenario) 
    }
  }
}






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



#add infrastructure demo calcs to Retreat_Analysis df

years <- c(2023, 2026, 2030, 2040, 2050, 2062, 2075, 2087, 2100)

for (year in years) {
  for(seawall in seawalls){
    for(trigger in triggers){
      #roads
    }
  }
}
      


seawall_col <- paste0("seawall_",scenario,seawall,trigger)


taxrev_col <- paste0("Total_TaxRev_TB",seawall,"t", trigger, "_l", hazard_type, "_bv",bval)
column_name <- paste0("Total_TaxRev_", year, "_t",trigger,"_l",hazard_type,"_bv",bval)
Retreat_Analysis[[taxrev_col]][Retreat_Analysis$Years == year] <- 
  sum(clean_retreat_calcs[[column_name]][TB_matching_rows], na.rm = TRUE)





demolition_seawall <- ifelse(seawall == "_",13123,0) # conditional seawall demolition depending on scenario. $4000/ft ($13123/m) for inaccessible seawall e.g. residential area)
seawalls_col <- paste0("SEAWALL_LEN_M")
seawallm <- sum(clean_retreat_calcs[[seawalls_col]][AO_matching_rows], na.rm = TRUE)
seawallm*demolition_seawall 
seawallm <- sum(clean_retreat_calcs[[seawalls_col]][TB_matching_rows], na.rm = TRUE)
seawallm <- sum(clean_retreat_calcs[[seawalls_col]][RE_matching_rows], na.rm = TRUE)

demo_col <- paste0("demolition_AO",seawall,"t",trigger)
Retreat_Analysis[[demo_col]][Retreat_Analysis$Years == year] <- 
  ifelse(year==2023,aptcount*demolition_apartment + housecount*demolition_house,0) 
