
#beach cost plot
#TB total cost y axis
#small bars = Littoral cell
#big bars = district


allisland <- clean_retreat_calcs
allinfra <- infra_retreat
beaches <- unique(allisland$LittrlCell) # 37 distinct 
districts <- unique(allisland$district) # 6 distinct

scen <- c('AO','TB','RE','AO','TB','RE')
sw <- c('_','_','_','_','_','_')
trig <- c('CE','CE','CE','PF','PF','PF')
land <- c('full','CE','CE','full','PF','PF')
build <- c('1','1','0','1','1','0')
road <- c('1','1','1','1','1','1')
clean <- c(NA,NA,'hi',NA,NA,'hi') 

#calculate cost per beach
beachdf <- setNames(data.frame(matrix(ncol = 10, nrow = 0)), c('scenario','trigger','beach','district',
                                                                'land_dwelling_cost','ambiguous_cost','infrastructure_cost',
                                                                'tax_revenue_loss','private_property_value_loss','total_cost'))

for(beach in beaches){
  clean_retreat_calcs <- allisland[allisland$LittrlCell==beach,]
  infra_retreat <- allinfra[allinfra$LittrlCell==beach,] 
  
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
    
    beachdistricts <- unique(infra_retreat$district)
    beachdistricts <- beachdistricts[!is.na(beachdistricts)]
    
    beachdf[nrow(beachdf) + 1,] = c(scenario = scen[i], trigger = trig[i],beach = beach, district = toString(beachdistricts),
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

beachdf[, 5:9] <- apply(beachdf[, 5:9], 2, function(x) as.numeric(as.character(x)))
beachdf[, 3] <- as.numeric(beachdf$beach)
beachdf$total_cost <- rowSums(beachdf[5:9])
beachdf$residential_cost <- beachdf$total_cost - beachdf$infrastructure_cost

#keep only most-prevalent hazard costs
beachesdf <- beachdf %>%
  group_by(scenario, beach) %>%
  filter(total_cost == max(total_cost))
#remove '0' beaches
beachesdf <- beachesdf[beachesdf$beach != 0,]

figbeachA <- ggplot(beachesdf, aes(y=total_cost, x=factor(beach))) + 
  geom_bar(stat='identity',position='dodge')+
  facet_wrap(~factor(scenario,levels=c('AO','TB','RE')), ncol = 3) +
  xlab('beach ID')+
  ylab('total cost')+
  theme_minimal() 
figbeachB <- ggplot(beachesdf, aes(y=infrastructure_cost, x=factor(beach))) + 
  geom_bar(stat='identity',position='dodge')+
  facet_wrap(~factor(scenario,levels=c('AO','TB','RE')), ncol = 3) +
  xlab('beach ID')+
  ylab('infrastructure cost')+
  theme_minimal() 
figbeachC <- ggplot(beachesdf, aes(y=residential_cost, x=factor(beach))) + 
  geom_bar(stat='identity',position='dodge')+
  facet_wrap(~factor(scenario,levels=c('AO','TB','RE')), ncol = 3) +
  xlab('beach ID')+
  ylab('residential cost')+
  theme_minimal() 
  
figbeach <- ggarrange(figbeachA,figbeachB,figbeachC,
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



#












#calculate cost per district
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
