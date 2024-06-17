library(ggplot2)
library(ggpubr)
library(ggrepel)
library(dplyr)


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
beachdf <- setNames(data.frame(matrix(ncol = 16, nrow = 0)), c('scenario','trigger','beach','district',
                                                                'land_dwelling_cost','ambiguous_cost','infrastructure_cost',
                                                                'tax_revenue_loss','private_property_value_loss','total_cost',
                                                               'average_value','average_value_cpr','number_buildings','number_apartments','number_CPR',
                                                               'length_highway'))
#calculate CE/PF per beach over time
cepfbeach <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c('beach','year','trigger','area'))
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
                                    number_CPR = Retreat_Analysis_Total[[CPRunits_col]],length_highway = Retreat_Analysis_Total[[hwylength_col]]
                                    )
    
  }
  
  #calculate erosion/passive flooding over time
  for(i in 1:length(years)){
    for(hazard_type in cepfhazard_types){
      areahazard_col <- paste0("areahazard_l",hazard_type)
      year <- years[i]
      prevyear <- years[i-1]
      
      totalarea <- ifelse(year==2023,0,as.numeric(cepfbeach$area[cepfbeach$beach == beach & cepfbeach$trigger == hazard_type & cepfbeach$year == prevyear])) +
        Retreat_Analysis[[areahazard_col]][Retreat_Analysis$Years == year]
      
      cepfbeach[nrow(cepfbeach) + 1,] = c(beach = beach,year = year,trigger = hazard_type, area = totalarea)
    }
  }
}

beachdf[, 5:9] <- apply(beachdf[, 5:9], 2, function(x) as.numeric(as.character(x)))
beachdf[, 3] <- as.numeric(beachdf$beach)
beachdf$total_cost <- rowSums(beachdf[5:9])
beachdf$residential_cost <- beachdf$total_cost - beachdf$infrastructure_cost

#keep only most-prevalent hazard costs
# beachesdf <- beachdf %>%
#   group_by(scenario, beach) %>%
#   filter(total_cost == max(total_cost))
#remove '0' beaches
beachesdf <- beachdf[beachdf$beach != 0,]

#calculate cost per beach length
beachlengthdf <- read.csv('F:/slr/kauai/CosmosTiff/Cosmos_to_Tiff.csv')
beachlengthdf = beachlengthdf[!duplicated(beachlengthdf$NewB),] #get rid of duplicates in NewB column
beachesdf <- beachesdf %>%
     left_join(beachlengthdf, by = c('beach' ='NewB'))
beachesdf$costperlength <- beachesdf$total_cost / beachesdf$Len_m

# A: total cost
figbeachA <- ggplot(beachesdf, aes(y=total_cost, x=factor(beach))) + 
  geom_bar(stat='identity',position='dodge')+
  facet_wrap(~factor(scenario,levels=c('AO','TB','RE')), ncol = 3) +
  xlab('beach ID')+
  ylab('total cost')+
  theme_minimal() 
# B: infrastructure cost
figbeachB <- ggplot(beachesdf, aes(y=infrastructure_cost, x=factor(beach))) + 
  geom_bar(stat='identity',position='dodge')+
  facet_wrap(~factor(scenario,levels=c('AO','TB','RE')), ncol = 3) +
  xlab('beach ID')+
  ylab('infrastructure cost')+
  theme_minimal() 
# C: residential cost
figbeachC <- ggplot(beachesdf, aes(y=residential_cost, x=factor(beach))) + 
  geom_bar(stat='identity',position='dodge')+
  facet_wrap(~factor(scenario,levels=c('AO','TB','RE')), ncol = 3) +
  xlab('beach ID')+
  ylab('residential cost')+
  theme_minimal() 
#D: Cost per beach length (standardized)
figbeachD <- ggplot(beachesdf, aes(y=costperlength, x=factor(beach))) + 
  geom_bar(stat='identity',position='dodge')+
  facet_wrap(~factor(scenario,levels=c('AO','TB','RE')), ncol = 3) +
  xlab('beach ID')+
  ylab('cost per length (m)')+
  theme_minimal() 
  
figbeach <- ggarrange(figbeachA,figbeachB,figbeachC,figbeachD,
                  labels=c("A","B","C","D"),
                  ncol=1,nrow=4,
                  align="v")






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
  geom_text(data=label_data,aes(x=id,y=costperlength+10000,label=Hawaiian.Name,hjust=hjust),angle= label_data$angle)
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
  geom_text(aes(label=scales::comma(as.numeric(cost))),size = 3, position = position_stack(vjust = 0.5))+
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
  geom_text_repel(aes(label=scales::comma(as.numeric(cost))),size = 3, position = position_stack(vjust = 0.5))+
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
  geom_text_repel(aes(label=scales::comma(as.numeric(cost))),size = 3, position = position_stack(vjust = 0.5))+
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
  geom_text_repel(aes(label=scales::comma(as.numeric(cost))),size = 3, position = position_stack(vjust = 0.5))+
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
  geom_text_repel(aes(label=scales::comma(as.numeric(cost))),size = 3, position = position_stack(vjust = 0.5))+
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
  geom_text_repel(aes(label=scales::comma(as.numeric(cost))),size = 3, position = position_stack(vjust = 0.5))+
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



# CE over time plot
# beachesdf$combo <- paste0(beachesdf$beach,beachesdf$trigger)
# combos <- unique(beachesdf$combo) 
# cepfbeach$combo <- paste0(cepfbeach$beach,cepfbeach$trigger)
# cepfbeachdf <- cepfbeach %>%
#   filter(
#     combo %in% combos
#   )
cepfbeachdf$area <- as.numeric(cepfbeachdf$area)
cepfbeachdf$year <- as.numeric(cepfbeachdf$year)
beachegce <- c('1','7','11','17','24','34')
cebeachdf <- cepfbeachdf[cepfbeachdf$trigger == 'CE',]
cebeachdf <- cebeachdf %>%
  rowwise() %>%
  mutate(label = case_when(beach %in% beachegce && year == 2100 ~ beach))

figcepf <- ggplot(cebeachdf, aes(x=year,y=area,group=beach))+
  geom_line(aes(color=trigger),alpha=0.5,linewidth=1)+
  scale_color_manual(values=c("#FADD21"))+ 
  theme_bw()+
  scale_y_continuous(name=expression("Total area retreated "~(m^2)),labels = scales::comma)+
  xlab("Year")

figcepf + geom_line(data=cebeachdf[cebeachdf$beach %in% beachegce,], color='#E7B800',linewidth=1.3)+
  geom_label_repel(aes(label=label),nudge_x = 1,na.rm=T,show.legend=F)


#https://htmlcolorcodes.com/






#export tables
write.csv(cepfbeach,'cepfbeach.csv')
write.csv(beachesdf,'beachdf.csv')





#### OLD UNUSED FIGURES

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
