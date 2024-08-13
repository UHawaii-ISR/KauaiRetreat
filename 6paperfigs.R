library(ggplot2)
library(ggpubr)
library(ggrepel)
library(dplyr)
library(tidyr)
library(ggmagnify)

#tell R where your files are
workdir <- "F:/slr/kauai/kauai_retreat_code/" # your working directory
assessorsfile <- "F:/slr/kauai/2023_Real_Property_Tax_Data" #original assessors file (used to calculate eminent domain)
noncprshpfolder <- "F:/slr/kauai/CosmosTiff" #the folder where non-CPR bldg footprint shapefile is
noncprshplayer <- "Buildings_XA_nonCPR" # "Buildings_XA" #layer name for non-CPR bldg footprint shapefile
cprshpfolder <- "F:/slr/kauai/CosmosTiff" #the folder for CPR bldg ftprt shapefile
cprshplayer <- "Buildings_XA_CPR" # "Buildings_XA_CPR"# layer name for CPR bldg ftprt shapefile
salayer <- "tmk_XA" #Shape area layer
osdsfile <- "F:/slr/kauai/OSDSv6_Exploded_ALL.csv" #the osds file
seawallfile <- "F:/slr/kauai/TMK_realign_seawalls" #the folder with seawall shapefile
infrastructurefolder <- "F:/slr/kauai/CosmosTiff" #the folder that has all of the infrastructure hazard files
emdomfile <- "F:/slr/kauai/slrxa-adj" #folder with only parcels that are SLRXA-adjacent

source("C:/Users/rsett/Documents/KauaiRetreat/1assessorsk.R")
source("C:/Users/rsett/Documents/KauaiRetreat/1infrastructurek.R") #takes 15 min to run this code
allisland <- clean_retreat_calcs
allinfra <- infra_retreat
allbldg <- clean_assessors_bldg
allparcels <- clean_assessors_parcels 


#beach cost plot

#set up scenarios
beaches <- c(1:40)
scen <- c('AO','TB','RE')
sw <- c('_','_','_')
trig <- c('CE','CE','CE')
land <- c('full','CE','CE')
build <- c('1','1','0')
road <- c('1','1','1')
clean <- c(NA,NA,'hi') 

#calculate cost per beach
beachdf <- setNames(data.frame(matrix(ncol = 32, nrow = 0)), c('scenario','trigger','beach','district',
                                                                'land_dwelling_cost','ambiguous_cost','infrastructure_cost',
                                                                'tax_revenue_loss','private_property_value_loss','total_cost',
                                                               'median_value_noncpr','median_value_cpr','median_value_res','median_value_all',
                                                               'number_buildings','number_apartments','number_CPR','number_homes',
                                                               'length_highway','length_riprap','length_rdremove','length_bridge','length_briprap',
                                                               'length_maintain','length_totalinf',
                                                               'residential','vacationrental','commercial','hotel','homestead',
                                                               'resinvestor','commercialhome'))
#calculate CE/PF per beach over time
cepfbeach <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c('beach','year','trigger','area','area_res'))
cepfparcel <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c('beach','year','trigger','numparcels'))
cepfroad <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c('beach','year','trigger','lengthroad'))
cepfhazard_types <- c('CE','PF')
years <- c(2023,2026,2030,2040,2050,2062,2075,2087,2100)

for(beach in beaches){
  clean_retreat_calcs <- allisland[allisland$NewB==beach,]  
  infra_retreat <- allinfra[allinfra$NewB==beach,] 
  clean_assessors_bldg <- allbldg
  clean_assessors_parcels <- allparcels[allparcels$NewB==beach,] 
  
  #median home/cpr value calculation
  nonCPR <- subset(clean_retreat_calcs, Number_CPRbldg == 1) 
  nonCPR <- subset(nonCPR, NEAR_CE32 <= 6.1)
  mednonCPRval <- apply(nonCPR[c('APRTOTMKT')],2,median,na.rm=T)[[1]]
  cprs <- subset(clean_retreat_calcs, Number_CPRbldg> 1)
  cprs <- subset(cprs, NEAR_CE32 <= 6.1 )
  medcprval <- apply(cprs[c('APRTOTMKT')],2,median,na.rm=T)[[1]]
  residential <- c("1:RESIDENTIAL", "2:VACATION RENTAL","8:HOMESTEAD","9:Residential Investor","10:Commercialized Home Use") 
  reshomes <- subset(clean_retreat_calcs,TAXCLASS %in% residential)
  reshomes <- subset(reshomes,NEAR_CE32 <= 6.1)
  medresval <- apply(reshomes[c('APRTOTMKT')],2,median,na.rm=T)[[1]]
  parcelsCE <- subset(clean_retreat_calcs,NEAR_CE32 <= 6.1)
  medparcelval <- apply(parcelsCE[c('APRTOTMKT')],2,median,na.rm=T)[[1]]
  
  source("C:/Users/rsett/Documents/KauaiRetreat/2retreatyearvaluetaxk.R")
  source("C:/Users/rsett/Documents/KauaiRetreat/3costsovertimek.R")
  source("C:/Users/rsett/Documents/KauaiRetreat/3infrastructurek.R") 
  source("C:/Users/rsett/Documents/KauaiRetreat/4discountedtotalcostsk.R")
  
  #taxclasses
  #sum number each tax class that must retreat by 2100 under CE
  residential <- sum(clean_retreat_calcs$TAXCLASS == '1:RESIDENTIAL' & clean_retreat_calcs$NEAR_CE32 <= 6.1)
  vacationrental <- sum(clean_retreat_calcs$TAXCLASS == '2:VACATION RENTAL' & clean_retreat_calcs$NEAR_CE32 <= 6.1)
  commercial <- sum(clean_retreat_calcs$TAXCLASS == '3:COMMERCIAL' & clean_retreat_calcs$NEAR_CE32 <= 6.1)
  hotel <- sum(clean_retreat_calcs$TAXCLASS == '7:HOTEL AND RESORT' & clean_retreat_calcs$NEAR_CE32 <= 6.1)
  homestead <- sum(clean_retreat_calcs$TAXCLASS == '8:HOMESTEAD' & clean_retreat_calcs$NEAR_CE32 <= 6.1)
  resinvestor <- sum(clean_retreat_calcs$TAXCLASS == '9:Residential Investor' & clean_retreat_calcs$NEAR_CE32 <= 6.1)
  commercialhome <- sum(clean_retreat_calcs$TAXCLASS == '10:Commercialized Home Use' & clean_retreat_calcs$NEAR_CE32 <= 6.1)
  
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
    homes_col <- paste0("homes_",scen[i],sw[i],"t",trig[i])
    hwylength_col <- paste0("hwylength",scen[i],sw[i],trig[i],"_rdr",road[i])
    hwyripraplen_col <- paste0("hwyripraplen",scen[i],sw[i],trig[i],"_rdr",road[i]) #total highway riprap length
    rdremovelen_col <- paste0("rdremovelen",scen[i],sw[i],trig[i],"_rdr",road[i]) #total highway riprap length
    b_reloclen_col <- paste0("bridgerelocatelen",scen[i],sw[i],trig[i],"_rdr",road[i])
    b_retrofitlen_col <- paste0("bridgeretrofitlen",scen[i],sw[i],trig[i],"_rdr",road[i])
    maintainlen_col <- paste0("maintainlen",scen[i],sw[i],trig[i],"_rdr",road[i])
    total_affected_col <- paste0("affectedlen",scen[i],sw[i],trig[i],"_rdr",road[i])
    
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
                                      total_cost = NA, median_value_noncpr = mednonCPRval, median_value_cpr = medcprval,
                                    median_value_res=medresval,median_value_all=medparcelval,
                                    number_buildings = Retreat_Analysis_Total[[buildings_col]],number_apartments = Retreat_Analysis_Total[[apartments_col]],
                                    number_CPR = Retreat_Analysis_Total[[CPRunits_col]],number_homes = Retreat_Analysis_Total[[homes_col]],
                                    length_highway = Retreat_Analysis_Total[[hwylength_col]],
                                    length_riprap = Retreat_Analysis_Total[[hwyripraplen_col]],length_rdremove = Retreat_Analysis_Total[[rdremovelen_col]],
                                    length_bridge = Retreat_Analysis_Total[[b_reloclen_col]],length_briprap = Retreat_Analysis_Total[[b_retrofitlen_col]],
                                    length_maintain = Retreat_Analysis_Total[[maintainlen_col]],length_totalinf = Retreat_Analysis_Total[[total_affected_col]],
                                    residential = residential,vacationrental = vacationrental,commercial = commercial,hotel = hotel,homestead = homestead,
                                    resinvestor = resinvestor,commercialhome = commercialhome
                                    )
    
  }
  
  #calculate erosion/passive flooding over time
  for(j in 1:length(years)){
    for(hazard_type in cepfhazard_types){
      areahazardall_col <- paste0("areahazardall_l",hazard_type) #use this column that calculates total hazard coverage on all parcels, not just residential ones
      areahazard_col <- paste0("areahazard_l",hazard_type) #calculates total hazard coverage on just residential parcels
      parcels_col <- paste0("parcelcpr_TB_t",hazard_type)
      hwylength_col <- paste0("hwylengthTB_",hazard_type,"_rdr1")
      total_affected_col <- paste0("affectedlenTB_",hazard_type,"_rdr1")
      
      year <- years[j]
      prevyear <- years[j-1]
      
      totalarea <- ifelse(year==2023,0,as.numeric(cepfbeach$area[cepfbeach$beach == beach & cepfbeach$trigger == hazard_type & cepfbeach$year == prevyear])) +
        Retreat_Analysis[[areahazardall_col]][Retreat_Analysis$Years == year]
      totalarea_res <- ifelse(year==2023,0,as.numeric(cepfbeach$area_res[cepfbeach$beach == beach & cepfbeach$trigger == hazard_type & cepfbeach$year == prevyear])) +
        Retreat_Analysis[[areahazard_col]][Retreat_Analysis$Years == year]
      totalparcel <- ifelse(year==2023,0,as.numeric(cepfparcel$numparcels[cepfparcel$beach == beach & cepfparcel$trigger == hazard_type & cepfparcel$year == prevyear])) +
        Retreat_Analysis[[parcels_col]][Retreat_Analysis$Years == year]
      totalroad <- ifelse(year==2023,0,as.numeric(cepfroad$lengthroad[cepfroad$beach == beach & cepfroad$trigger == hazard_type & cepfroad$year == prevyear])) +
        Retreat_Analysis[[total_affected_col]][Retreat_Analysis$Years == year]
      
      cepfbeach[nrow(cepfbeach) + 1,] = c(beach = beach,year = year,trigger = hazard_type, area = totalarea, area_res = totalarea_res)
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


#homes affected in one column
beachesdf$homes_affected <- as.numeric(beachesdf$number_buildings) + as.numeric(beachesdf$number_CPR)

#TB scenario only
beachesdftb <- beachesdf[beachesdf$scenario == 'TB',]

write.csv(beachesdftb,'beachesdftb.csv')
write.csv(beachesdf,'beachesdf.csv')
write.csv(cepfbeach,'cepfbeach.csv')
write.csv(cepfparcel,'cepfparcel.csv')
write.csv(cepfroad,'cepfroad.csv')

beachesdftb <- read.csv('beachesdftb.csv')
beachesdf<-read.csv('beachesdf.csv')
cepfbeach<-read.csv('cepfbeach.csv')
cepfparcel<-read.csv('cepfparcel.csv')
cepfroad<-read.csv('cepfroad.csv')







# Fig 2 COSTS

# A) total costs stacked res & inf, 
# B) erosion area retreated 
# C) count of parcels, 
# D) length of roads, 
# E) median property value 

# A: total cost with stacked res & inf
beachesdftb$residential_cost_mil <- beachesdftb$residential_cost/1000000
beachesdftb$infrastructure_cost_mil <- beachesdftb$infrastructure_cost/1000000
beachesdftb_stack <- beachesdftb %>%
  pivot_longer(cols = c(residential_cost_mil, infrastructure_cost_mil), names_to = "cost_type", values_to = "cost")

figbeachA <- ggplot(beachesdftb_stack, aes(x=as.factor(beach),y=as.numeric(cost),fill=cost_type)) + 
  geom_bar(stat='identity',position='stack')+
  scale_fill_manual(values=c('grey','grey43'),labels=c('Infrastructure cost','Residential cost'),name= "Cost type")+
  xlab('Beach ID')+
  ylab('Total cost \n ($2023, mil)')+
  scale_y_continuous(label=scales::comma)+
  theme_classic() +
  theme(strip.placement = "outside")

# B: erosion area retreated 
cebeachdf <- cepfbeach[cepfbeach$trigger == 'CE',]
cebeach2100df <- cebeachdf[cebeachdf$year == 2100,]

figbeachB <- ggplot(cebeach2100df, aes(y=as.numeric(area), x = reorder(as.factor(beach), sort(as.numeric(beach))))) + 
  geom_bar(stat='identity',position='dodge')+
  xlab('Beach ID')+
  ylab(expression(paste("Total area eroded ("~m^2,")")))+
  scale_y_continuous(label=scales::comma)+
  theme_classic() 
figbeachB_res <- ggplot(cebeach2100df, aes(y=as.numeric(area_res), x = reorder(as.factor(beach), sort(as.numeric(beach))))) + 
  geom_bar(stat='identity',position='dodge')+
  xlab('Beach ID')+
  ylab(expression("Total area \n eroded "~(m^2)))+
  scale_y_continuous(label=scales::comma)+
  theme_classic() 

cebeach2100df$area <- as.numeric(cebeach2100df$area)
cebeach2100df$area_res <- as.numeric(cebeach2100df$area_res)
cebeach2100df$area_nonres <- as.numeric(cebeach2100df$area) - as.numeric(cebeach2100df$area_res)
cebeach2100df_stack <- cebeach2100df %>%
  pivot_longer(cols = c(area_nonres, area_res), names_to = "breakdown", values_to = "area_breakdown")

figbeachB <- ggplot(cebeach2100df_stack, aes(x=reorder(as.factor(beach), sort(as.numeric(beach))),y=as.numeric(area_breakdown),fill=breakdown)) + 
  geom_bar(stat='identity',position='stack')+
  scale_fill_manual(values=c('grey','grey43'),labels=c('All parcels','Residential parcels'),name= "Parcel type")+
  xlab('Beach ID')+
  ylab(expression(paste("Total area eroded ("~m^2,")")))+
  scale_y_continuous(label=scales::comma)+
  theme_classic() +
  theme(strip.placement = "outside")

# C: number of homes
figbeachC <- ggplot(beachesdftb, aes(y=number_homes, x=factor(beach))) + 
  geom_bar(stat='identity',position='dodge')+
  xlab('Beach ID')+
  ylab('Number of \n homes affected')+
  scale_y_continuous(label=scales::comma)+
  theme_classic() 

#D: length of roads
figbeachD <- ggplot(beachesdftb, aes(y=as.numeric(length_totalinf), x=factor(beach))) + 
  geom_bar(stat='identity',position='dodge')+
  xlab('Beach ID')+
  ylab('Length of road \n affected (m)')+
  scale_y_continuous(label=scales::comma)+
  theme_classic() 

#E: median property value of retreating residential parcels
beachesdftb$median_value_res_mil <- beachesdftb$median_value_res/1000000

figbeachE <- ggplot(beachesdftb, aes(y=as.numeric(median_value_res_mil), x=factor(beach))) + 
  geom_bar(stat='identity',position='dodge')+
  xlab('Beach ID')+
  ylab('Median home \n value ($2023, mil)')+
  scale_y_continuous(label=scales::comma)+
  theme_classic() 


figbeach <- cowplot::plot_grid(figbeachA,figbeachB,figbeachC,figbeachD,figbeachE,
                               labels=c("A","B","C","D","E"), 
                               ncol=1,nrow=5,hjust=0.1,
                               align="v",axis="lr",greedy=T)
ggsave('fig2_bar.png', figbeach,width=25,height=25,dpi=300,units='cm')








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

#hanalei (4)
spotlt_hanalei <- beachspike[beachspike$beach ==4,]
spotlt_hanalei <- spotlt_hanalei[!is.na(spotlt_hanalei$beach),]
spotlt_hanalei <- setNames(as.data.frame(t(spotlt_hanalei[-1])), spotlt_hanalei[[1]])
names(spotlt_hanalei) <- c('cost')
spotlt_hanalei <- spotlt_hanalei %>% tibble::rownames_to_column(var = "costtype")
beachname <- spotlt_hanalei$cost[spotlt_hanalei$costtype == 'Hawaiian.Name']
spotlt_hanalei<- spotlt_hanalei[spotlt_hanalei$costtype %in% c('infrastructure_cost','residential_cost'),]
spotlt_hanalei$Hawaiian.Name <- beachname
figspotlt_hanalei <- ggplot(spotlt_hanalei,aes(x=Hawaiian.Name, y=as.numeric(cost),fill=costtype,label=as.numeric(cost)))+
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
ggsave('figspotlt_hanalei.png', figspotlt_hanalei, bg='transparent',width=7.5,height=1,dpi=300,units='in')

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
beachesdftb$total_cost_mil <- beachesdftb$total_cost/1000000
spotlight <- c('Kekaha','ʻAnini','Hāʻena','Waipouli','Poʻipū','Moloaʻa','Hanalei')
beachesdftb$number_homes <- as.numeric(beachesdftb$number_homes)
beachesdftb$length_totalinf <- as.numeric(beachesdftb$length_totalinf)
beachesdftb$total_cost_mil <- as.numeric(beachesdftb$total_cost_mil)
labels <- beachesdftb %>%
  filter(beachname %in% spotlight | total_cost_mil > 100)

bubblezoom <- ggplot(beachesdftb, aes(x=as.numeric(number_homes), y=as.numeric(length_totalinf), 
                                      size=as.numeric(total_cost_mil), color=as.numeric(median_value_res_mil))) +
  geom_point(alpha=0.5)  +
  scale_size(range=c(2, 20),labels=scales::label_comma()) +
  scale_color_viridis_c(option="plasma",labels = scales::label_comma()) +
  geom_text_repel(data=labels,mapping=aes(label=beachname),size= 4,color= 'black')+
  scale_y_continuous(labels=scales::comma,limits=c(0,400))+
  xlim(0,30)+
  theme_classic()+
  theme(legend.position='none',
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

bubbles <- ggplot(beachesdftb, aes(x=as.numeric(number_homes), y=as.numeric(length_totalinf), size=as.numeric(total_cost_mil), 
                        color=as.numeric(median_value_res_mil), label=beachname)) +
  geom_point(alpha=0.5)  +
  scale_size(range=c(2, 20),labels=scales::label_comma()) +
  labs(size = "Total cost \n ($2023, mil)")+
  scale_color_viridis_c(option="plasma",labels = scales::label_comma()) +
  guides(col = guide_colourbar(title = "Median home value \n($2023, mil)"))+
  geom_text_repel(data=labels,aes(label=beachname), size= 4,color= 'black')+ 
  xlab('Number of homes affected')+
  ylab('Road affected (m)')+
  scale_y_continuous(labels=scales::comma) +
  expand_limits(y=4000,x=225)+
  theme_classic()+
  geom_magnify(aes(from=number_homes < 30 & length_totalinf < 400),
               to=c(xmin=125,xmax=200,ymin=1500,ymax=3000),shadow=T,plot=bubblezoom)


ggsave('fig3_bubbles.png', bubbles,width=8,height=6,dpi=300,units='in')





#https://htmlcolorcodes.com/















#median value test
beachesdftb_median <- beachesdftb %>%
  pivot_longer(cols = c(median_value_noncpr, median_value_cpr,median_value_res,median_value_all), 
               names_to = "median_type", values_to = "value")

medians <- ggplot(beachesdftb_median, aes(x=as.factor(beach),y=as.numeric(value),fill=median_type)) + 
  geom_bar(stat='identity',position='dodge')+
  scale_fill_manual(values=c("#4E84C4", "#E7B800", "#C77CFF","#00BA38"),
                    labels=c('All parcels','CPR','non-CPR','Residential'),name= "Cost type")+
  xlab('Beach ID')+
  ylab('Median value')+
  scale_y_continuous(label=scales::comma)+
  theme_classic() 

ggsave('fig_medians.png', medians,width=10,height=5,dpi=300,units='in')






