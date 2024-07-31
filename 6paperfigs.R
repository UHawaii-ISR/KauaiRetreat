library(ggplot2)
library(ggpubr)
library(ggrepel)
library(dplyr)
library(tidyr)



source("C:/Users/rsett/Documents/KauaiRetreat/1assessorsk.R")
source("C:/Users/rsett/Documents/KauaiRetreat/1infrastructurek.R") #takes 15 min to run this code
allisland <- clean_retreat_calcs
allinfra <- infra_retreat

# beachboundaries <- read.csv('F:/slr/kauai/kauai_retreat_code/cosmos_newb.csv')
# allisland <- allisland %>%
#   left_join(beachboundaries, by = c('LittrlCell'='Cosmos'))
# allinfra <- allinfra %>%
#   left_join(beachboundaries, by = c('LittrlCell'='Cosmos'))
#tmkbeaches <- unique(allisland$NewB) 
#infrabeaches <- unique(allinfra$NewB)
#beaches <- c(tmkbeaches,infrabeaches)
beaches <- c(1:40)
# beaches <- unique(beaches)
# beaches <- beaches[!is.na(beaches)]

#beach cost plot

scen <- c('AO','TB','RE')
sw <- c('_','_','_')
trig <- c('CE','CE','CE')
land <- c('full','CE','CE')
build <- c('1','1','0')
road <- c('1','1','1')
clean <- c(NA,NA,'hi') 

#calculate cost per beach
beachdf <- setNames(data.frame(matrix(ncol = 25, nrow = 0)), c('scenario','trigger','beach','district',
                                                                'land_dwelling_cost','ambiguous_cost','infrastructure_cost',
                                                                'tax_revenue_loss','private_property_value_loss','total_cost',
                                                               'median_value','median_value_cpr','number_buildings','number_apartments','number_CPR',
                                                               'length_highway','length_riprap','length_rdremove',
                                                               'residential','vacationrental','commercial','hotel','homestead',
                                                               'resinvestor','commercialhome'))
#calculate CE/PF per beach over time
cepfbeach <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c('beach','year','trigger','area'))
cepfparcel <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c('beach','year','trigger','numparcels'))
cepfroad <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c('beach','year','trigger','lengthroad'))
cepfhazard_types <- c('CE','PF')
years <- c(2023,2026,2030,2040,2050,2062,2075,2087,2100)

for(beach in beaches){
  clean_retreat_calcs <- allisland[allisland$NewB==beach,]  #LittrlCell
  infra_retreat <- allinfra[allinfra$NewB==beach,] #LittrlCell
  
  #median home/cpr value calculation
  houses <- subset(clean_retreat_calcs, Number_CPRbldg == 1) 
  houses <- subset(houses, NEAR_CE32 == 0 | NEAR_PF32 == 0)
  medhomeval <- apply(houses[c('APRTOTMKT')],2,median,na.rm=T)[[1]]
  cprs <- subset(clean_retreat_calcs, Number_CPRbldg> 1)
  cprs <- subset(cprs, NEAR_CE32 == 0 | NEAR_PF32 == 0)
  medcprval <- apply(cprs[c('APRTOTMKT')],2,median,na.rm=T)[[1]]
  
  #taxclasses
  #sum number each tax class that must retreat by 2100 under CE
  residential <- sum(clean_retreat_calcs$TAXCLASS == '1:RESIDENTIAL' & clean_retreat_calcs$NEAR_CE32 == 0)
  vacationrental <- sum(clean_retreat_calcs$TAXCLASS == '2:VACATION RENTAL' & clean_retreat_calcs$NEAR_CE32 == 0)
  commercial <- sum(clean_retreat_calcs$TAXCLASS == '3:COMMERCIAL' & clean_retreat_calcs$NEAR_CE32 == 0)
  hotel <- sum(clean_retreat_calcs$TAXCLASS == '7:HOTEL AND RESORT' & clean_retreat_calcs$NEAR_CE32 == 0)
  homestead <- sum(clean_retreat_calcs$TAXCLASS == '8:HOMESTEAD' & clean_retreat_calcs$NEAR_CE32 == 0)
  resinvestor <- sum(clean_retreat_calcs$TAXCLASS == '9:Residential Investor' & clean_retreat_calcs$NEAR_CE32 == 0)
  commercialhome <- sum(clean_retreat_calcs$TAXCLASS == '10:Commercialized Home Use' & clean_retreat_calcs$NEAR_CE32 == 0)
  
  
  source("C:/Users/rsett/Documents/KauaiRetreat/2retreatyearvaluetaxk.R")
  source("C:/Users/rsett/Documents/KauaiRetreat/3costsovertimek.R")
  source("C:/Users/rsett/Documents/KauaiRetreat/3infrastructurek.R") 
  source("C:/Users/rsett/Documents/KauaiRetreat/4discountedtotalcostsk.R")
  
  for(i in 1:3){
    totalval_col <- paste0("Total_Value_",scen[i],sw[i],"t",trig[i],"_l",land[i],"_bv",build[i])
    demo_col <- paste0("demolition_",scen[i],sw[i],"t",trig[i])
    osds_col <- paste0("osdsremoval_",scen[i],sw[i],"t",trig[i])
    wastewater_col <- paste0("wastewaterremoval_",scen[i],sw[i],"t",trig[i])
    seawall_col <- paste0("seawall_",scen[i],sw[i],trig[i])
    cleanup_col <- paste0("cleanup",clean[i],"_",scen[i],sw[i],"t",trig[i]) 
    infra_col <- paste0("infrastructure_",scen[i],sw[i],trig[i],"_rdr",road[i])
    taxrevloss_col <- paste0("Total_TaxRev_Loss_",scen[i],sw[i],"t",trig[i],"_l",land[i],"_bv",build[i])
    privproploss_col <- paste0("Priv_Prop_Loss_",scen[i],sw[i],"t",trig[i],"_l",land[i],"_bv",build[i])
    buildings_col <- paste0('buildings_',scen[i],sw[i],"t",trig[i])
    apartments_col <- paste0("apartments_",scen[i],sw[i],"t",trig[i])
    CPRunits_col <- paste0("CPRunits_",scen[i],sw[i],"t",trig[i])
    hwylength_col <- paste0("hwylength",scen[i],sw[i],trig[i],"_rdr",road[i])
    hwyripraplen_col <- paste0("hwyripraplen",scen[i],sw[i],trig[i],"_rdr",road[i]) #total highway riprap length
    rdremovelen_col <- paste0("rdremovelen",scen[i],sw[i],trig[i],"_rdr",road[i]) #total highway riprap length
    
    beachdistricts <- unique(infra_retreat$district)
    beachdistricts <- beachdistricts[!is.na(beachdistricts)]
    
    beachdf[nrow(beachdf) + 1,] = c(scenario = scen[i], trigger = trig[i],beach = beach,district = toString(beachdistricts),
                                      land_dwelling_cost = ifelse(is.na(clean[i]),
                                                                  sum(Retreat_Analysis_Total[[totalval_col]],Retreat_Analysis_Total[[demo_col]],Retreat_Analysis_Total[[seawall_col]],
                                                                      Retreat_Analysis_Total[[osds_col]],Retreat_Analysis_Total[[wastewater_col]],na.rm=T),
                                                                  Retreat_Analysis_Total[[totalval_col]]),
                                      ambiguous_cost = ifelse(is.na(clean[i]),0,
                                                              sum(Retreat_Analysis_Total[[cleanup_col]],Retreat_Analysis_Total[[seawall_col]],na.rm=T)),
                                      infrastructure_cost = Retreat_Analysis_Total[[infra_col]],
                                      tax_revenue_loss = Retreat_Analysis_Total[[taxrevloss_col]],
                                      private_property_value_loss = Retreat_Analysis_Total[[privproploss_col]],
                                      total_cost = NA, median_value = medhomeval, median_value_cpr = medcprval,
                                    number_buildings = Retreat_Analysis_Total[[buildings_col]],number_apartments = Retreat_Analysis_Total[[apartments_col]],
                                    number_CPR = Retreat_Analysis_Total[[CPRunits_col]],length_highway = Retreat_Analysis_Total[[hwylength_col]],
                                    length_riprap = Retreat_Analysis_Total[[hwyripraplen_col]],length_rdremove = Retreat_Analysis_Total[[rdremovelen_col]],
                                    residential = residential,vacationrental = vacationrental,commercial = commercial,hotel = hotel,homestead = homestead,
                                    resinvestor = resinvestor,commercialhome = commercialhome
                                    )
    
  }
  
  #calculate erosion/passive flooding over time
  for(j in 1:length(years)){
    for(hazard_type in cepfhazard_types){
      areahazard_col <- paste0("areahazard_l",hazard_type)
      parcels_col <- paste0("parcelcpr_TB_t",hazard_type)
      hwylength_col <- paste0("hwylengthTB_",hazard_type,"_rdr1")
      
      year <- years[j]
      prevyear <- years[j-1]
      
      totalarea <- ifelse(year==2023,0,as.numeric(cepfbeach$area[cepfbeach$beach == beach & cepfbeach$trigger == hazard_type & cepfbeach$year == prevyear])) +
        Retreat_Analysis[[areahazard_col]][Retreat_Analysis$Years == year]
      totalparcel <- ifelse(year==2023,0,as.numeric(cepfparcel$numparcels[cepfparcel$beach == beach & cepfparcel$trigger == hazard_type & cepfparcel$year == prevyear])) +
        Retreat_Analysis[[parcels_col]][Retreat_Analysis$Years == year]
      totalroad <- ifelse(year==2023,0,as.numeric(cepfroad$lengthroad[cepfroad$beach == beach & cepfroad$trigger == hazard_type & cepfroad$year == prevyear])) +
        Retreat_Analysis[[hwylength_col]][Retreat_Analysis$Years == year]
      
      cepfbeach[nrow(cepfbeach) + 1,] = c(beach = beach,year = year,trigger = hazard_type, area = totalarea)
      cepfparcel[nrow(cepfparcel) + 1,] = c(beach = beach,year = year,trigger = hazard_type, numparcels = totalparcel)
      cepfroad[nrow(cepfroad) + 1,] = c(beach = beach,year = year,trigger = hazard_type, lengthroad = totalroad)
    }
  }
}

beachdf[, 5:9] <- apply(beachdf[, 5:9], 2, function(x) as.numeric(as.character(x)))
beachdf[, 3] <- as.numeric(beachdf$beach)
beachdf$total_cost <- rowSums(beachdf[5:9])
beachdf$residential_cost <- beachdf$total_cost - beachdf$infrastructure_cost

#remove '0' beaches
beachesdf <- beachdf[beachdf$beach != 0,]
beachesdf <- beachesdf[beachesdf$trigger == 'CE',]

#get beach names
beachnames <- read.csv('F:/slr/kauai/CosmosTiff/KauaiBeachNames.csv')
names(beachnames) <- c('beach','beachname')
beachesdf <- beachesdf %>%
  left_join(beachnames, by = c('beach'))

#calculate cost per beach length
beachlengthdf <- read.csv('F:/slr/kauai/CosmosTiff/Cosmos_to_Tiff.csv')
beachlengthdf = beachlengthdf[!duplicated(beachlengthdf$NewB),] #get rid of duplicates in NewB column
beachesdf <- beachesdf %>%
     left_join(beachlengthdf, by = c('beach' ='NewB'))
beachesdf$costperlength <- beachesdf$total_cost / beachesdf$Len_m

#categorize by residential type, color by infrastructure affected
restype <- read.csv('F:/slr/kauai/CosmosTiff/beachrestype.csv')
beachesdf <- beachesdf %>%
  left_join(restype, by = c('beach' ='NewB'))
beachesdf$ResType <- factor(beachesdf$ResType, levels=c('Non-Impacted Development','Infrastructure','Low-Density Residential',
                                                        'Low-Density Residential and Resort','High-Density Residential and Resort','Resort'))
#TB scenario only
beachesdftb <- beachesdf[beachesdf$scenario == 'TB',]

#add all roads affected into one column
beachesdftb$road_affected <- as.numeric(beachesdftb$length_rdremove) + ifelse(as.numeric(beachesdftb$length_highway == 0),as.numeric(beachesdftb$length_riprap),
                                                                              as.numeric(beachesdftb$length_highway))

#homes affected in one column
beachesdftb$homes_affected <- as.numeric(beachesdftb$number_buildings) + as.numeric(beachesdftb$number_CPR)

write.csv(beachesdftb,'beachesdftb.csv')








# Fig 2 COSTS

# A) total costs stacked res & inf, 
# B) erosion area retreated 
# C) count of parcels, 
# D) length of roads, 
# E) median property value 

# A: total cost with stacked res & inf
beachesdftb_stack <- beachesdftb %>%
  pivot_longer(cols = c(residential_cost, infrastructure_cost), names_to = "cost_type", values_to = "cost")

figbeachA <- ggplot(beachesdftb_stack, aes(x=as.factor(beach),y=as.numeric(cost),fill=cost_type)) + 
  geom_bar(stat='identity',position='stack')+
  #facet_wrap(~factor(scenario,levels=c('AO','TB','RE')), ncol = 3) +
  #facet_wrap(~ResType, strip.position = "bottom",scales='free_x',ncol=5,space='free_x')+
  #facet_grid(~ResType,space='free_x',scales='free_x',switch='both')+
  scale_fill_manual(values=c('grey','grey43'),labels=c('Infrastructure cost','Residential cost'),name= "Cost type")+
  xlab('Beach ID')+
  ylab('Total cost')+
  scale_y_continuous(label=scales::comma)+
  theme_classic() +
  theme(strip.placement = "outside")

# B: erosion area retreated 
cebeachdf <- cepfbeach[cepfbeach$trigger == 'CE',]
cebeach2100df <- cebeachdf[cebeachdf$year == 2100,]

figbeachB <- ggplot(cebeach2100df, aes(y=as.numeric(area), x = reorder(as.factor(beach), sort(as.numeric(beach))))) + 
  geom_bar(stat='identity',position='dodge')+
  #facet_wrap(~factor(scenario,levels=c('AO','TB','RE')), ncol = 3) +
  #facet_grid(~ResType,space='free_x',scales='free_x',switch='both')+
  xlab('Beach ID')+
  ylab(expression("Total area eroded "~(m^2)))+
  scale_y_continuous(label=scales::comma)+
  theme_classic() 

# C: number of homes
figbeachC <- ggplot(beachesdftb, aes(y=homes_affected, x=factor(beach))) + 
  geom_bar(stat='identity',position='dodge')+
  #facet_wrap(~factor(scenario,levels=c('AO','TB','RE')), ncol = 3) +
  #facet_grid(~ResType,space='free_x',scales='free_x',switch='both')+
  xlab('Beach ID')+
  ylab('Number of homes affected')+
  scale_y_continuous(label=scales::comma)+
  theme_classic() 

#D: length of roads
figbeachD <- ggplot(beachesdftb, aes(y=road_affected, x=factor(beach))) + 
  geom_bar(stat='identity',position='dodge')+
  #facet_wrap(~factor(scenario,levels=c('AO','TB','RE')), ncol = 3) +
  #facet_grid(~ResType,space='free_x',scales='free_x',switch='both')+
  xlab('Beach ID')+
  ylab('Length of road affected (m)')+
  scale_y_continuous(label=scales::comma)+
  theme_classic() 

#E: median property value
figbeachE <- ggplot(beachesdftb, aes(y=as.numeric(median_value), x=factor(beach))) + 
  geom_bar(stat='identity',position='dodge')+
  #facet_wrap(~factor(scenario,levels=c('AO','TB','RE')), ncol = 3) +
  #facet_grid(~ResType,space='free_x',scales='free_x',switch='both')+
  xlab('Beach ID')+
  ylab('Median home value')+
  scale_y_continuous(label=scales::comma)+
  theme_classic() 
  
figbeach <- ggarrange(figbeachA,figbeachB,figbeachC,figbeachD,figbeachE,
                  labels=c("A","B","C","D","E"), hjust=0.2,
                  ncol=1,nrow=5,
                  align="v",
                  common.legend = TRUE, legend="top")











# spike plot

#prep df
beachspike <- beachesdf[beachesdf$scenario =='TB',] #plot only TB
empty_bar <- 7
to_add <- matrix(NA, empty_bar, ncol(beachspike))
colnames(to_add) <- colnames(beachspike)
beachspike <- rbind(beachspike, to_add)
beachspike <- beachspike %>% bind_rows(slice(., 1:3)) %>% slice(-(1:3))
beachspike$id <- seq(1, nrow(beachspike))
beachspike$district <- ifelse(beachspike$beach %in% 1:10, 'North Shore Kauai',
                              ifelse(beachspike$beach %in% 11:17, 'Kapaa-Wailua',
                                     ifelse(beachspike$beach %in% 18:21,'Lihue',
                                            ifelse(beachspike$beach %in% 22:27,'Koloa-Poipu',
                                                   ifelse(beachspike$beach %in% 28:30,'Hanapepe-Eleele',
                                                          ifelse(beachspike$beach %in% 31:40,'Waimea-Kekaha',NA))))))
label_data <- beachspike
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar  
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)


figspike <- ggplot(beachspike, aes(y=costperlength, x=factor(id),fill=district)) + 
  geom_bar(stat='identity',position='dodge')+
  xlab('beach ID')+
  ylab('total cost')+
  theme_minimal() +
  theme(
    plot.background = element_rect(fill='transparent', color=NA),
    axis.title=element_blank(),
    axis.text=element_blank(),
    panel.grid=element_blank(),
    panel.border = element_blank(),
    legend.position='none'
  )+
  coord_polar(start=0)+
  ylim(-500000,max(beachspike$costperlength)) + #negative space for inner circle
  #facet_wrap(~factor(scenario,levels=c('AO','TB','RE')), ncol = 3) +
  geom_text(data=label_data,aes(x=id,y=costperlength+10000,label=beachname,hjust=hjust),angle= label_data$angle,size=3)
ggsave('figspike.png', figspike, bg='transparent',width=5,height=5,dpi=300,units='in')
#add coloring by category
#spotlights: kekaha (34), wailua (17), moloaa (11), poipu (24), haena (1)





#spike spotlights
#kekaha (34)
spotlt_kekaha <- beachspike[beachspike$beach ==34,]
spotlt_kekaha <- spotlt_kekaha[!is.na(spotlt_kekaha$beach),]
spotlt_kekaha <- setNames(as.data.frame(t(spotlt_kekaha[-1])), spotlt_kekaha[[1]])
names(spotlt_kekaha) <- c('cost')
spotlt_kekaha <- spotlt_kekaha %>% tibble::rownames_to_column(var = "costtype")
beachname <- spotlt_kekaha$cost[spotlt_kekaha$costtype == 'Hawaiian.Name']
spotlt_kekaha<- spotlt_kekaha[spotlt_kekaha$costtype %in% c('infrastructure_cost','residential_cost'),]
spotlt_kekaha$Hawaiian.Name <- beachname
figspotlt_kekaha <- ggplot(spotlt_kekaha,aes(x=Hawaiian.Name, y=as.numeric(cost),fill=costtype,label=as.numeric(cost)))+
  geom_bar(position='stack',stat='identity',width=0.7)+
  scale_fill_manual(values=c('grey','grey43'))+
  geom_text(aes(label=scales::comma(round(as.numeric(cost),-3))),size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(breaks=seq(0,1000000000,250000000),labels=scales::dollar_format(prefix='$',suffix='M',scale = 1e-6))+
  theme_minimal() +
  ylab('Cost')+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none")+
  expand_limits(y = 1000000000)+
  coord_flip()
ggsave('figspotlt_kekaha.png', figspotlt_kekaha, bg='transparent',width=7.5,height=1,dpi=300,units='in')

#wailua(17)
spotlt_wailua <- beachspike[beachspike$beach ==17,]
spotlt_wailua <- spotlt_wailua[!is.na(spotlt_wailua$beach),]
spotlt_wailua <- setNames(as.data.frame(t(spotlt_wailua[-1])), spotlt_wailua[[1]])
names(spotlt_wailua) <- c('cost')
spotlt_wailua <- spotlt_wailua %>% tibble::rownames_to_column(var = "costtype")
beachname <- spotlt_wailua$cost[spotlt_wailua$costtype == 'Hawaiian.Name']
spotlt_wailua<- spotlt_wailua[spotlt_wailua$costtype %in% c('infrastructure_cost','residential_cost'),]
spotlt_wailua$Hawaiian.Name <- beachname
figspotlt_wailua <- ggplot(spotlt_wailua,aes(x=Hawaiian.Name, y=as.numeric(cost),fill=costtype,label=as.numeric(cost)))+
  geom_bar(position='stack',stat='identity',width=0.7)+
  scale_fill_manual(values=c('grey','grey43'))+
  geom_text_repel(aes(label=scales::comma(round(as.numeric(cost),-3))),size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(breaks=seq(0,1000000000,250000000),labels=scales::dollar_format(prefix='$',suffix='M',scale = 1e-6))+
  theme_minimal() +
  ylab('Cost')+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none")+
  expand_limits(y = 1000000000)+
  coord_flip()
ggsave('figspotlt_wailua.png', figspotlt_wailua, bg='transparent',width=7.5,height=1,dpi=300,units='in')

#moloaa (11)
spotlt_moloaa <- beachspike[beachspike$beach ==11,]
spotlt_moloaa <- spotlt_moloaa[!is.na(spotlt_moloaa$beach),]
spotlt_moloaa <- setNames(as.data.frame(t(spotlt_moloaa[-1])), spotlt_moloaa[[1]])
names(spotlt_moloaa) <- c('cost')
spotlt_moloaa <- spotlt_moloaa %>% tibble::rownames_to_column(var = "costtype")
beachname <- spotlt_moloaa$cost[spotlt_moloaa$costtype == 'Hawaiian.Name']
spotlt_moloaa<- spotlt_moloaa[spotlt_moloaa$costtype %in% c('infrastructure_cost','residential_cost'),]
spotlt_moloaa$Hawaiian.Name <- beachname
figspotlt_moloaa <- ggplot(spotlt_moloaa,aes(x=Hawaiian.Name, y=as.numeric(cost),fill=costtype,label=as.numeric(cost)))+
  geom_bar(position='stack',stat='identity',width=0.7)+
  scale_fill_manual(values=c('grey','grey43'))+
  geom_text_repel(aes(label=scales::comma(round(as.numeric(cost),-3))),size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(breaks=seq(0,1000000000,250000000),labels=scales::dollar_format(prefix='$',suffix='M',scale = 1e-6))+
  theme_minimal() +
  ylab('Cost')+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none")+
  expand_limits(y = 1000000000)+
  coord_flip()
ggsave('figspotlt_moloaa.png', figspotlt_moloaa, bg='transparent',width=7.5,height=1,dpi=300,units='in')

#poipu (24)
spotlt_poipu <- beachspike[beachspike$beach ==24,]
spotlt_poipu <- spotlt_poipu[!is.na(spotlt_poipu$beach),]
spotlt_poipu <- setNames(as.data.frame(t(spotlt_poipu[-1])), spotlt_poipu[[1]])
names(spotlt_poipu) <- c('cost')
spotlt_poipu <- spotlt_poipu %>% tibble::rownames_to_column(var = "costtype")
beachname <- spotlt_poipu$cost[spotlt_poipu$costtype == 'Hawaiian.Name']
spotlt_poipu<- spotlt_poipu[spotlt_poipu$costtype %in% c('infrastructure_cost','residential_cost'),]
spotlt_poipu$Hawaiian.Name <- beachname
figspotlt_poipu <- ggplot(spotlt_poipu,aes(x=Hawaiian.Name, y=as.numeric(cost),fill=costtype,label=as.numeric(cost)))+
  geom_bar(position='stack',stat='identity',width=0.7)+
  scale_fill_manual(values=c('grey','grey43'))+
  geom_text_repel(aes(label=scales::comma(round(as.numeric(cost),-3))),size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(breaks=seq(0,1000000000,250000000),labels=scales::dollar_format(prefix='$',suffix='M',scale = 1e-6))+
  theme_minimal() +
  ylab('Cost')+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none")+
  expand_limits(y = 1000000000)+
  coord_flip()
ggsave('figspotlt_poipu.png', figspotlt_poipu, bg='transparent',width=7.5,height=1,dpi=300,units='in')

#haena (1)
spotlt_haena <- beachspike[beachspike$beach ==1,]
spotlt_haena <- spotlt_haena[!is.na(spotlt_haena$beach),]
spotlt_haena <- setNames(as.data.frame(t(spotlt_haena[-1])), spotlt_haena[[1]])
names(spotlt_haena) <- c('cost')
spotlt_haena <- spotlt_haena %>% tibble::rownames_to_column(var = "costtype")
beachname <- spotlt_haena$cost[spotlt_haena$costtype == 'Hawaiian.Name']
spotlt_haena<- spotlt_haena[spotlt_haena$costtype %in% c('infrastructure_cost','residential_cost'),]
spotlt_haena$Hawaiian.Name <- beachname
figspotlt_haena <- ggplot(spotlt_haena,aes(x=Hawaiian.Name, y=as.numeric(cost),fill=costtype,label=as.numeric(cost)))+
  geom_bar(position='stack',stat='identity',width=0.7)+
  scale_fill_manual(values=c('grey','grey43'))+
  geom_text_repel(aes(label=scales::comma(round(as.numeric(cost),-3))),size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(breaks=seq(0,1000000000,250000000),labels=scales::dollar_format(prefix='$',suffix='M',scale = 1e-6))+
  theme_minimal() +
  ylab('Cost')+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none")+
  expand_limits(y = 1000000000)+
  coord_flip()
ggsave('figspotlt_haena.png', figspotlt_haena, bg='transparent',width=7.5,height=1,dpi=300,units='in')

#anini (7)
spotlt_anini <- beachspike[beachspike$beach ==7,]
spotlt_anini <- spotlt_anini[!is.na(spotlt_anini$beach),]
spotlt_anini <- setNames(as.data.frame(t(spotlt_anini[-1])), spotlt_anini[[1]])
names(spotlt_anini) <- c('cost')
spotlt_anini <- spotlt_anini %>% tibble::rownames_to_column(var = "costtype")
beachname <- spotlt_anini$cost[spotlt_anini$costtype == 'Hawaiian.Name']
spotlt_anini<- spotlt_anini[spotlt_anini$costtype %in% c('infrastructure_cost','residential_cost'),]
spotlt_anini$Hawaiian.Name <- beachname
figspotlt_anini <- ggplot(spotlt_anini,aes(x=Hawaiian.Name, y=as.numeric(cost),fill=costtype,label=as.numeric(cost)))+
  geom_bar(position='stack',stat='identity',width=0.7)+
  scale_fill_manual(values=c('grey','grey43'))+
  geom_text_repel(aes(label=scales::comma(round(as.numeric(cost),-3))),size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(breaks=seq(0,1000000000,250000000),labels=scales::dollar_format(prefix='$',suffix='M',scale = 1e-6))+
  theme_minimal() +
  ylab('Cost')+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none")+
  expand_limits(y = 1000000000)+
  coord_flip()
ggsave('figspotlt_anini.png', figspotlt_anini, bg='transparent',width=7.5,height=1,dpi=300,units='in')

#legend
figspotlt_legend <- ggplot(spotlt_anini,aes(x=Hawaiian.Name, y=as.numeric(cost),fill=costtype,label=as.numeric(cost)))+
  geom_bar(position='stack',stat='identity',width=0.7)+
  scale_fill_manual(values=c('grey','grey43'),labels=c('Infrastructure cost','Residential cost'))+
  geom_text_repel(aes(label=scales::comma(round(as.numeric(cost),-3))),size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(breaks=seq(0,1000000000,250000000),labels=scales::dollar_format(prefix='$',suffix='M',scale = 1e-6))+
  theme_minimal() +
  ylab('Cost')+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title=element_blank())+
  expand_limits(y = 1000000000)
legend <- cowplot::get_legend(figspotlt_legend)
grid.newpage()
grid.draw(legend)





# FIG 3 BUBBLES

#create bubble chart and color circles based on value of team variable
ggplot(beachesdftb, aes(x=as.numeric(homes_affected), y=as.numeric(road_affected), size=as.numeric(total_cost), color=as.numeric(median_value), label=Hawaiian.Name)) +
  geom_point(alpha=0.5)  +
  scale_size(range=c(2, 30),labels=scales::label_comma()) +
  labs(size = "Total cost")+
  scale_color_viridis_c(option="plasma",labels = scales::label_comma()) +
  guides(col = guide_colourbar(title = "Median home value"))+
  geom_text_repel(aes(label=ifelse(total_cost>100000000,as.character(Hawaiian.Name),'')), size= 5,color= 'black')+ #,hjust=0.5, vjust=0,
  xlab('Number of homes affected')+
  ylab('Road affected (m)')+
  scale_y_continuous(labels=scales::comma)+
  theme_classic()






#https://htmlcolorcodes.com/






#export tables
write.csv(cepfbeach,'cepfbeach.csv')
write.csv(beachesdf,'beachdf.csv')


















#### OLD UNUSED FIGURES

#reshape for stacked bar chart
long_beachesdf <- beachesdftb %>%
  pivot_longer(cols = c(infrastructure_cost, residential_cost),
               names_to = "cost_type",
               values_to = "cost")
figstackA <- ggplot(long_beachesdf, aes(x = factor(beach), y = cost, fill = cost_type)) +
  geom_bar(stat = "identity") +
  facet_grid(~ResType,space='free_x',scales='free_x',switch='both')+
  xlab('Beach ID')+
  ylab('Total cost')+
  scale_y_continuous(label=scales::comma)+
  scale_fill_manual(values=c('grey','grey43'),labels=c('Infrastructure cost','Residential cost'))+
  theme_classic()
figbeachstack <- ggarrange(figstackA,figbeachD,
                           labels=c("A","B"),
                           ncol=1,nrow=2,
                           align="v",
                           common.legend = T)




#cluster analysis
library(tidyverse)
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms

#prepare data
beachesdftb[c("residential", "homestead", "vacationrental", "commercial", "hotel", "resinvestor", "commercialhome")] <- 
  lapply(beachesdftb[c("residential", "homestead", "vacationrental", "commercial", "hotel", "resinvestor", "commercialhome")], as.numeric)
beachesdftb$respercent <- (beachesdftb$residential+beachesdftb$homestead)/(beachesdftb$residential+beachesdftb$homestead+beachesdftb$vacationrental+beachesdftb$commercial+
                                                                             beachesdftb$hotel+beachesdftb$resinvestor+beachesdftb$commercialhome)

#add all roads affected into one column
beachesdftb$road_affected <- as.numeric(beachesdftb$length_rdremove) + ifelse(as.numeric(beachesdftb$length_highway == 0),as.numeric(beachesdftb$length_riprap),
                                                                              as.numeric(beachesdftb$length_highway))
beachesdftb$rd_affected <- as.numeric(beachesdftb$length_rdremove)
beachesdftb$hwy_affected <- ifelse(as.numeric(beachesdftb$length_highway == 0),as.numeric(beachesdftb$length_riprap),
                                   as.numeric(beachesdftb$length_highway))

#homes affected in one column
beachesdftb$homes_affected <- as.numeric(beachesdftb$number_buildings) + as.numeric(beachesdftb$number_CPR)

#standardize make all numbers per length beach
beachesdftb$road_affect_perlen <- as.numeric(beachesdftb$road_affected) / as.numeric(beachesdftb$Len_m)
beachesdftb$home_affect_perlen <- as.numeric(beachesdftb$homes_affected) / as.numeric(beachesdftb$Len_m)

beachesclust <- beachesdftb[c('costperlength','road_affect_perlen','home_affect_perlen','median_value')] 
# 'total_cost','Len_m','median_value','rd_affected','hwy_affected','number_buildings','number_CPR'

rownames(beachesclust) <- beachesdftb$Hawaiian.Name
beachesclust[is.na(beachesclust)] <- 0
beachesclust[] <- lapply(beachesclust, function(x) as.numeric(as.character(x)))

beachesclust <- scale(beachesclust)
# Dissimilarity matrix
d <- dist(beachesclust, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

# Compute with agnes
hc2 <- agnes(beachesclust, method = "complete")

# Agglomerative coefficient
hc2$ac

# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(beachesclust, method = x)$ac
}

map_dbl(m, ac)
##  average    single  complete      ward 
## 0.8712478 0.8322026 0.8757782 0.9132052


hc3 <- agnes(beachesclust, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 


# compute divisive hierarchical clustering
hc4 <- diana(beachesclust)

# Divise coefficient; amount of clustering structure found
hc4$dc
## [1] 0.8764557

# plot dendrogram
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana")



# CE over time plot
# beachesdf$combo <- paste0(beachesdf$beach,beachesdf$trigger)
# combos <- unique(beachesdf$combo) 
# cepfbeach$combo <- paste0(cepfbeach$beach,cepfbeach$trigger)
# cepfbeachdf <- cepfbeach %>%
#   filter(
#     combo %in% combos
#   )

cepfbeach$area <- as.numeric(cepfbeach$area)
cepfbeach$year <- as.numeric(cepfbeach$year)
cepfparcel$numparcels <- as.numeric(cepfparcel$numparcels)
cepfparcel$year <- as.numeric(cepfparcel$year)
cepfroad$lengthroad <- as.numeric(cepfroad$lengthroad)
cepfroad$year <- as.numeric(cepfroad$year)
cebeachdf <- cepfbeach[cepfbeach$trigger == 'CE',]
ceparcel <- cepfparcel[cepfparcel$trigger == 'CE',]
ceroad <- cepfroad[cepfroad$trigger == 'CE',]
beachegce <- c('1','7','11','17','24','34')
cebeachdf <- cebeachdf %>%
  rowwise() %>%
  mutate(label = case_when(beach %in% beachegce && year == 2100 ~ beach))
ceparcel <- ceparcel %>%
  rowwise() %>%
  mutate(label = case_when(beach %in% beachegce && year == 2100 ~ beach))
ceroad <- ceroad %>%
  rowwise() %>%
  mutate(label = case_when(beach %in% beachegce && year == 2100 ~ beach))

figceA <- ggplot(cebeachdf, aes(x=year,y=area,group=beach))+
  geom_line(aes(color=trigger),alpha=0.5,linewidth=1)+
  scale_color_manual(values=c("#989898"))+ 
  theme_bw()+
  scale_y_continuous(name=expression("Total area retreated "~(m^2)),labels = scales::comma)+
  xlab("Year") +
  geom_line(data=cebeachdf[cebeachdf$beach %in% beachegce,], color='#585858',linewidth=1.3)+
  geom_label_repel(aes(label=label),nudge_x = 1,na.rm=T,show.legend=F)+
  theme(legend.position="none")
figceB <- ggplot(ceparcel, aes(x=year,y=as.numeric(numparcels),group=beach))+
  geom_line(aes(color=trigger),alpha=0.5,linewidth=1)+
  scale_color_manual(values=c("#989898"))+ 
  theme_bw()+
  ylab("Total parcels retreated")+
  xlab("Year") +
  geom_line(data=ceparcel[ceparcel$beach %in% beachegce,], color='#585858',linewidth=1.3)+
  geom_label_repel(aes(label=label),nudge_x = 1,na.rm=T,show.legend=F)+
  theme(legend.position="none")
figceC <- ggplot(ceroad, aes(x=year,y=as.numeric(lengthroad),group=beach))+
  geom_line(aes(color=trigger),alpha=0.5,linewidth=1)+
  scale_color_manual(values=c("#989898"))+ 
  theme_bw()+
  ylab("Total road length retreated (m)")+
  xlab("Year") +
  geom_line(data=ceroad[ceroad$beach %in% beachegce,], color='#585858',linewidth=1.3)+
  geom_label_repel(aes(label=label),nudge_x = 1,na.rm=T,show.legend=F)+
  theme(legend.position="none")
figce <- ggarrange(figceA,figceB,figceC,
                   labels=c("A","B","C"),
                   ncol=1,nrow=3,
                   align="v")


# treemap plot
# infrastructure vs res color coding

tm <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("scenario","group", "subgroup", "cost"))

scen <- c('AO','TB','RE')
sw <- c('_','_','_')
trig <- c('CE','CE','CE')
land <- c('full','CE','CE')
build <- c('1','1','0')
road <- c('1','1','1')
clean <- c(NA,NA,'hi') 

for(i in 1:3){
  tm[nrow(tm)+1,] <- c(scenario=scen[i],group="Private Property Value Loss",subgroup="Property \n Value Loss",
                       cost=Retreat_Analysis_Total[[paste0("Priv_Prop_Loss_",scen[i],sw[i],"t",trig[i],"_l",land[i],"_bv",build[i])]])
  if(!is.na(clean[i])){
    tm[nrow(tm)+1,] <- c(scenario=scen[i],group="Ambiguous",subgroup="Debris",
                         cost=Retreat_Analysis_Total[[paste0("cleanup",clean[i],"_",scen[i],sw[i],"t",trig[i]) ]])
    tm[nrow(tm)+1,] <- c(scenario=scen[i],group="Ambiguous",subgroup="Seawall \n removal",
                         cost=Retreat_Analysis_Total[[paste0("seawall_",scen[i],sw[i],trig[i])]])
  }
  tm[nrow(tm)+1,] <- c(scenario=scen[i],group="Tax revenue loss",subgroup="Tax \n Revenue \n Loss",
                       cost=Retreat_Analysis_Total[[paste0("Total_TaxRev_Loss_",scen[i],sw[i],"t",trig[i],"_l",land[i],"_bv",build[i])]])
  tm[nrow(tm)+1,] <- c(scenario=scen[i],group="Land and dwelling retreat public cost",subgroup="Buyout",
                       cost=Retreat_Analysis_Total[[paste0("Total_Value_",scen[i],sw[i],"t",trig[i],"_l",land[i],"_bv",build[i])]])
  if(scen[i] != 'RE'){
    tm[nrow(tm)+1,] <- c(scenario=scen[i],group="Land and dwelling retreat public cost",subgroup="Demolition",
                         cost=Retreat_Analysis_Total[[paste0("demolition_",scen[i],sw[i],"t",trig[i])]])
    tm[nrow(tm)+1,] <- c(scenario=scen[i],group="Land and dwelling retreat public cost",subgroup="Seawall \n removal",
                         cost=Retreat_Analysis_Total[[paste0("seawall_",scen[i],sw[i],trig[i])]])
  }
  tm[nrow(tm)+1,] <- c(scenario=scen[i],group="Land and dwelling retreat public cost",subgroup="Wastewater \n removal",
                       cost=Retreat_Analysis_Total[[paste0("wastewaterremoval_",scen[i],sw[i],"t",trig[i])]])
  tm[nrow(tm)+1,] <- c(scenario=scen[i],group="Land and dwelling retreat public cost",subgroup="OSDS \n removal",
                       cost=Retreat_Analysis_Total[[paste0("osdsremoval_",scen[i],sw[i],"t",trig[i])]])
  tm[nrow(tm)+1,] <- c(scenario=scen[i],group="Infrastructure retreat cost",subgroup="Bridge \n retreat",
                       cost=Retreat_Analysis_Total[[paste0("bridgerelocate",scen[i],sw[i],trig[i],"_rdr",road[i])]])
  tm[nrow(tm)+1,] <- c(scenario=scen[i],group="Infrastructure retreat cost",subgroup="Bridge \n retrofit",
                       cost=Retreat_Analysis_Total[[paste0("bridgeretrofit",scen[i],sw[i],trig[i],"_rdr",road[i])]])
  tm[nrow(tm)+1,] <- c(scenario=scen[i],group="Infrastructure retreat cost",subgroup="Road \n realignment",
                       cost=Retreat_Analysis_Total[[paste0("hwyrelocate",scen[i],sw[i],trig[i],"_rdr",road[i])]])
  tm[nrow(tm)+1,] <- c(scenario=scen[i],group="Infrastructure retreat cost",subgroup="Road \n removal",
                       cost=Retreat_Analysis_Total[[paste0("rdremove",scen[i],sw[i],trig[i],"_rdr",road[i])]])
  tm[nrow(tm)+1,] <- c(scenario=scen[i],group="Infrastructure retreat cost",subgroup="Eminent \n domain",
                       cost=Retreat_Analysis_Total[[paste0("emdom",scen[i],sw[i],trig[i],"_rdr",road[i])]])
  tm[nrow(tm)+1,] <- c(scenario=scen[i],group="Infrastructure retreat cost",subgroup="Road \n hardening",
                       cost=Retreat_Analysis_Total[[paste0("hwyriprap",scen[i],sw[i],trig[i],"_rdr",road[i])]])
  tm[nrow(tm)+1,] <- c(scenario=scen[i],group="Infrastructure retreat cost",subgroup="Water main \n relocation",
                       cost=Retreat_Analysis_Total[[paste0("waterrelocate",scen[i],sw[i],trig[i],"_rdr",road[i])]])
  tm[nrow(tm)+1,] <- c(scenario=scen[i],group="Infrastructure retreat cost",subgroup="Riprap \n removal",
                       cost=Retreat_Analysis_Total[[paste0("riprapremove",scen[i],sw[i],trig[i],"_rdr",road[i])]])
  tm[nrow(tm)+1,] <- c(scenario=scen[i],group="Infrastructure retreat cost",subgroup="Riprap \n maintenance",
                       cost=Retreat_Analysis_Total[[paste0("maintain",scen[i],sw[i],trig[i],"_rdr",road[i])]])
}

tm$cost <- as.numeric(tm$cost)
tm <- tm %>%
  group_by(scenario) %>%
  mutate(scenario_sum = sum(cost,na.rm=T))
tm$percent <- tm$cost/tm$scenario_sum #sum(tm$cost,na.rm=T)

figpiebeach <- ggplot(tm,aes(area=cost,fill=group,
                             label=paste(subgroup,scales::percent(percent,accuracy=1),sep ="\n"),
                             subgroup=subgroup))+
  treemapify::geom_treemap(layout="squarified")+
  geom_treemap_text(color='white',place = "centre",size = 15)+
  geom_treemap_subgroup_border(colour = "white", size = 3)+
  scale_fill_brewer("Cost types",palette = "Set2")+
  scale_fill_manual(values = c('Private Property Value Loss'='#FFEB7F','Ambiguous'='#FFEB7F','Tax revenue loss'='#FFEB7F',
                               'Land and dwelling retreat public cost'='#FFEB7F','Infrastructure retreat cost'='#00b300'),
                    limits = c('Private Property Value Loss','Ambiguous','Tax revenue loss','Land and dwelling retreat public cost','Infrastructure retreat cost')) + 
  ggtitle(paste0(titlename))+
  facet_wrap(~factor(scenario,levels=c('AO','TB','RE')), ncol = 3) +
  theme(legend.position="bottom",strip.text.x = element_text(size = 15),panel.spacing=unit(1,"lines"))

#calculate cost per district
districts <- unique(allisland$district) # 6 distinct

maheledistdf <- setNames(data.frame(matrix(ncol = 10, nrow = 0)), c('scenario','trigger','beach','district',
                                                                    'land_dwelling_cost','ambiguous_cost','infrastructure_cost',
                                                                    'tax_revenue_loss','private_property_value_loss','total_cost'))
for(district in districts){
  clean_retreat_calcs <- allisland[allisland$district==district,]
  infra_retreat <- allinfra[allinfra$district==district,] 
  
  source("C:/Users/rsett/Documents/KauaiRetreat/2retreatyearvaluetaxk.R")
  source("C:/Users/rsett/Documents/KauaiRetreat/3costsovertimek.R")
  source("C:/Users/rsett/Documents/KauaiRetreat/3infrastructurek.R") 
  source("C:/Users/rsett/Documents/KauaiRetreat/4discountedtotalcostsk.R")
  
  for(i in 1:6){
    totalval_col <- paste0("Total_Value_",scen[i],sw[i],"t",trig[i],"_l",land[i],"_bv",build[i])
    demo_col <- paste0("demolition_",scen[i],sw[i],"t",trig[i])
    osds_col <- paste0("osdsremoval_",scen[i],sw[i],"t",trig[i])
    wastewater_col <- paste0("wastewaterremoval_",scen[i],sw[i],"t",trig[i])
    seawall_col <- paste0("seawall_",scen[i],sw[i],trig[i])
    cleanup_col <- paste0("cleanup",clean[i],"_",scen[i],sw[i],"t",trig[i]) 
    infra_col <- paste0("infrastructure_",scen[i],sw[i],trig[i],"_rdr",road[i])
    taxrevloss_col <- paste0("Total_TaxRev_Loss_",scen[i],sw[i],"t",trig[i],"_l",land[i],"_bv",build[i])
    privproploss_col <- paste0("Priv_Prop_Loss_",scen[i],sw[i],"t",trig[i],"_l",land[i],"_bv",build[i])
    
    districtbeaches <- unique(infra_retreat$LittrlCell)
    districtbeaches <- districtbeaches[!is.na(districtbeaches)]
    
    maheledistdf[nrow(maheledistdf) + 1,] = c(scenario = scen[i], trigger = trig[i],beach = toString(districtbeaches), district = district,
                                              land_dwelling_cost = ifelse(is.na(clean[i]),
                                                                          sum(Retreat_Analysis_Total[[totalval_col]],Retreat_Analysis_Total[[demo_col]],Retreat_Analysis_Total[[seawall_col]],
                                                                              Retreat_Analysis_Total[[osds_col]],Retreat_Analysis_Total[[wastewater_col]],na.rm=T),
                                                                          Retreat_Analysis_Total[[totalval_col]]),
                                              ambiguous_cost = ifelse(is.na(clean[i]),0,
                                                                      sum(Retreat_Analysis_Total[[cleanup_col]],Retreat_Analysis_Total[[seawall_col]],na.rm=T)),
                                              infrastructure_cost = Retreat_Analysis_Total[[infra_col]],
                                              tax_revenue_loss = Retreat_Analysis_Total[[taxrevloss_col]],
                                              private_property_value_loss = Retreat_Analysis_Total[[privproploss_col]],
                                              total_cost = NA)
  }
}
maheledistdf[, 5:9] <- apply(maheledistdf[, 5:9], 2, function(x) as.numeric(as.character(x)))
maheledistdf$total_cost <- rowSums(maheledistdf[5:9])

figmahele <- ggplot(maheledistdf, aes(fill=factor(trigger) ,y=total_cost, x=district)) + 
  geom_bar(stat='identity',position='dodge')+
  facet_wrap(~factor(scenario,levels=c('AO','TB','RE')), nrow = 3) 
