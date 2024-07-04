#naming standard for sub-scenarios: RE_s_tXA_lCE_bv1_chi (approach = AO, trigger = XA, land transfer = CE, building value = 1,cleanup cost = high, seawall = stay)

library(dplyr)
library(ggplot2)
library(ggpubr)
library(data.table)
library(scales)
library(plotly) #https://plotly.com/r/
library(treemap)
library(RColorBrewer)
library(treemapify)


setwd(workdir)


# Disable scientific notation
options(scipen = 999)





########################

###SUMMARIZING TABLES AND FIGURES 
# Output tables
# Comprehensive cost table

# Create cost summary data frame
cost_summary <- data.frame(
  `CostTypes` = c("Land and dwelling retreat public cost",
                  "Land and dwelling retreat private cost",
                  "Infrastructure retreat cost",
                  "Tax revenue loss",
                  "Private property value loss") 
)

# Total costs data frame for total costs plot (fig 3, red & blue)
total_cost <- setNames(data.frame(matrix(ncol = 10, nrow = 0)), c('management','scenario','trigger','hazard','bval','cleanup','seawall','subscenario','costtype','value'))


#add values for each column in order: retreat public cost, retreat private cost, infrastructure, tax rev loss, priv prop loss, total costs and losses
#for AO
for(trigger in triggers){
  for(hazard_type in hazard_types){
    for(seawall in seawalls){
      for(rdr in rdret){
        totalval_col <- paste0("Total_Value_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
        demo_col <- paste0("demolition_AO",seawall,"t",trigger)
        osds_col <- paste0("osdsremoval_AO",seawall,"t",trigger)
        wastewater_col <- paste0("wastewaterremoval_AO",seawall,"t",trigger)
        seawall_col <- paste0("seawall_AO",seawall,trigger)
        infra_col <- paste0("infrastructure_AO",seawall,trigger,"_rdr",rdr)
        taxrevloss_col <- paste0("Total_TaxRev_Loss_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
        privproploss_col <- paste0("Priv_Prop_Loss_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
        scenario_col <- paste0("AO",seawall,"t",trigger,"_l",hazard_type,"_bv1","_rdr",rdr)
        
        cost_summary[[scenario_col]] <- c(sum(Retreat_Analysis_Total[[totalval_col]],Retreat_Analysis_Total[[demo_col]],
                                              Retreat_Analysis_Total[[osds_col]],Retreat_Analysis_Total[[wastewater_col]],na.rm=T),
                                          Retreat_Analysis_Total[[seawall_col]],
                                          Retreat_Analysis_Total[[infra_col]],
                                          Retreat_Analysis_Total[[taxrevloss_col]],
                                          Retreat_Analysis_Total[[privproploss_col]])
        
        total_cost[nrow(total_cost) + 1,] = c(management = "Managed",scenario = "AO",trigger = trigger,hazard = hazard_type,bval = 0, cleanup = 0, seawall = seawall, subscenario = scenario_col, 
                                              costtype = "Land and dwelling retreat public cost", 
                                              value = sum(Retreat_Analysis_Total[[totalval_col]],Retreat_Analysis_Total[[demo_col]],
                                                          Retreat_Analysis_Total[[osds_col]],Retreat_Analysis_Total[[wastewater_col]],
                                                          Retreat_Analysis_Total[[seawall_col]],na.rm=T))
        total_cost[nrow(total_cost) + 1,] = c(management = "Managed",scenario = "AO",trigger = trigger,hazard = hazard_type,bval = 0, cleanup = 0, seawall = seawall, subscenario = scenario_col, 
                                              costtype = "Ambiguous-payer cost", 
                                              value = 0)
        total_cost[nrow(total_cost) + 1,] = c(management = "Managed",scenario = "AO",trigger = trigger,hazard = hazard_type,bval = 0, cleanup = 0, seawall = seawall, subscenario = scenario_col, 
                                              costtype = "Infrastructure retreat cost", 
                                              value = Retreat_Analysis_Total[[infra_col]])
        total_cost[nrow(total_cost) + 1,] = c(management = "Managed",scenario = "AO",trigger = trigger,hazard = hazard_type,bval = 0, cleanup = 0, seawall = seawall, subscenario = scenario_col, 
                                              costtype = "Tax revenue loss", 
                                              value = Retreat_Analysis_Total[[taxrevloss_col]])
        total_cost[nrow(total_cost) + 1,] = c(management = "Managed",scenario = "AO",trigger = trigger,hazard = hazard_type,bval = 0, cleanup = 0, seawall = seawall, subscenario = scenario_col, 
                                              costtype = "Private property value loss", 
                                              value = Retreat_Analysis_Total[[privproploss_col]])
      }
    }
  }
}

#for TB
for(trigger in triggers){
  for(hazard_type in hazard_types){
    for(bval in bvals){
      for(seawall in seawalls){
        for(rdr in rdret){
          totalval_col <- paste0("Total_Value_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
          demo_col <- paste0("demolition_TB",seawall,"t",trigger)
          osds_col <- paste0("osdsremoval_TB",seawall,"t",trigger)
          wastewater_col <- paste0("wastewaterremoval_TB",seawall,"t",trigger)
          seawall_col <- paste0("seawall_TB",seawall,trigger)
          infra_col <- paste0("infrastructure_TB",seawall,trigger,"_rdr",rdr)
          taxrevloss_col <- paste0("Total_TaxRev_Loss_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
          privproploss_col <- paste0("Priv_Prop_Loss_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
          scenario_col <- paste0("TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval,"_rdr",rdr)
          
          cost_summary[[scenario_col]] <- c(sum(Retreat_Analysis_Total[[totalval_col]],Retreat_Analysis_Total[[demo_col]],
                                                Retreat_Analysis_Total[[osds_col]],Retreat_Analysis_Total[[wastewater_col]],na.rm=T),
                                            Retreat_Analysis_Total[[seawall_col]],
                                            Retreat_Analysis_Total[[infra_col]],
                                            Retreat_Analysis_Total[[taxrevloss_col]],
                                            Retreat_Analysis_Total[[privproploss_col]])
          
          total_cost[nrow(total_cost) + 1,] = c(management = "Managed",scenario = "TB",trigger = trigger,hazard = hazard_type,bval = bval, cleanup = 0, seawall = seawall, 
                                                subscenario = scenario_col, 
                                                costtype = "Land and dwelling retreat public cost", 
                                                value = sum(Retreat_Analysis_Total[[totalval_col]],Retreat_Analysis_Total[[demo_col]],
                                                            Retreat_Analysis_Total[[osds_col]],Retreat_Analysis_Total[[wastewater_col]],
                                                            Retreat_Analysis_Total[[seawall_col]],na.rm=T))
          total_cost[nrow(total_cost) + 1,] = c(management = "Managed",scenario = "TB",trigger = trigger,hazard = hazard_type,bval = bval, cleanup = 0, seawall = seawall, 
                                                subscenario = scenario_col, 
                                                costtype = "Ambiguous-payer cost", 
                                                value = 0)
          total_cost[nrow(total_cost) + 1,] = c(management = "Managed",scenario = "TB",trigger = trigger,hazard = hazard_type,bval = bval, cleanup = 0, seawall = seawall, 
                                                subscenario = scenario_col, 
                                                costtype = "Infrastructure retreat cost", 
                                                value = Retreat_Analysis_Total[[infra_col]])
          total_cost[nrow(total_cost) + 1,] = c(management = "Managed",scenario = "TB",trigger = trigger,hazard = hazard_type,bval = bval, cleanup = 0, seawall = seawall, 
                                                subscenario = scenario_col, 
                                                costtype = "Tax revenue loss", 
                                                value = Retreat_Analysis_Total[[taxrevloss_col]])
          total_cost[nrow(total_cost) + 1,] = c(management = "Managed",scenario = "TB",trigger = trigger,hazard = hazard_type,bval = bval, cleanup = 0, seawall = seawall, 
                                                subscenario = scenario_col, 
                                                costtype = "Private property value loss", 
                                                value = Retreat_Analysis_Total[[privproploss_col]])
        }
      }
      
    }
    
  }
}

#for RE
cleanups <- c("hi","lo")
for(trigger in triggers){
  for(hazard_type in hazard_types){
    for(cleanup in cleanups){
      for(seawall in seawalls){
        for(rdr in rdret){
          totalval_col <- paste0("Total_Value_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
          cleanup_col <- paste0("cleanup",cleanup,"_RE",seawall,"t",trigger) 
          infra_col <- paste0("infrastructure_RE",seawall,trigger,"_rdr",rdr)
          taxrevloss_col <- paste0("Total_TaxRev_Loss_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
          privproploss_col <- paste0("Priv_Prop_Loss_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
          seawall_col <- paste0("seawall_RE",seawall,trigger)
          scenario_col <- paste0("RE",seawall,"t",trigger,"_l",hazard_type,"_bv0_c",cleanup,"_rdr",rdr)
          
          cost_summary[[scenario_col]] <- c(Retreat_Analysis_Total[[totalval_col]],
                                            sum(Retreat_Analysis_Total[[cleanup_col]],Retreat_Analysis_Total[[seawall_col]]),
                                            Retreat_Analysis_Total[[infra_col]],
                                            Retreat_Analysis_Total[[taxrevloss_col]],
                                            Retreat_Analysis_Total[[privproploss_col]])
          
          total_cost[nrow(total_cost) + 1,] = c(management = "Unmanaged",scenario = "RE",trigger = trigger,hazard = hazard_type,bval = 0, cleanup = cleanup, seawall = seawall, subscenario = scenario_col, 
                                                costtype = "Land and dwelling retreat public cost", 
                                                value = Retreat_Analysis_Total[[totalval_col]])
          total_cost[nrow(total_cost) + 1,] = c(management = "Unmanaged",scenario = "RE",trigger = trigger,hazard = hazard_type,bval = 0, cleanup = cleanup, seawall = seawall, subscenario = scenario_col, 
                                                costtype = "Ambiguous-payer cost", 
                                                value = sum(Retreat_Analysis_Total[[cleanup_col]],Retreat_Analysis_Total[[seawall_col]]))
          total_cost[nrow(total_cost) + 1,] = c(management = "Unmanaged",scenario = "RE",trigger = trigger,hazard = hazard_type,bval = 0, cleanup = cleanup, seawall = seawall, subscenario = scenario_col, 
                                                costtype = "Infrastructure retreat cost", 
                                                value = Retreat_Analysis_Total[[infra_col]])
          total_cost[nrow(total_cost) + 1,] = c(management = "Unmanaged",scenario = "RE",trigger = trigger,hazard = hazard_type,bval = 0, cleanup = cleanup, seawall = seawall, subscenario = scenario_col, 
                                                costtype = "Tax revenue loss", 
                                                value = Retreat_Analysis_Total[[taxrevloss_col]])
          total_cost[nrow(total_cost) + 1,] = c(management = "Unmanaged",scenario = "RE",trigger = trigger,hazard = hazard_type,bval = 0, cleanup = cleanup, seawall = seawall, subscenario = scenario_col, 
                                                costtype = "Private property value loss", 
                                                value = Retreat_Analysis_Total[[privproploss_col]])
        }
      }
    }
  }
}


# Total Costs and Losses
cols <- names(cost_summary)
cost_summary <- cost_summary %>%
  mutate_at(cols[2:length(cols)],as.numeric)
for (col in 2:length(cols)) {
  cost_summary[6, col] <- sum(cost_summary[1:5, col])
}
cost_summary[6,1] <- c("Total costs and losses")

## divide by 1million if preferred (e.g. for the overall cost figure)
cost_summary <- cost_summary %>%
  mutate(across(2:length(cols),function(x) x/1000000))
total_cost$valueMil <- as.numeric(total_cost$value)/ 1000000




# create total_cost_retreat data frame into figure

subscenarios <- unique(total_cost$subscenario)
total_cost$subscenario <- factor(total_cost$subscenario, 
                                 levels=subscenarios)
total_cost$trigger <- factor(total_cost$trigger, levels =triggers)
total_cost$scenario <- factor(total_cost$scenario, levels = c('AO','TB','RE'))
total_cost$costtype <- factor(total_cost$costtype, levels = 
                                c('Private property value loss','Ambiguous-payer cost','Tax revenue loss',
                                  'Land and dwelling retreat public cost','Infrastructure retreat cost'))

fig3 <- ggplot(total_cost, aes(fill=factor(costtype) ,y=valueMil, x=subscenario)) + 
  geom_bar(position="stack", stat="identity", width= 0.7, colour="grey", linewidth=0.2) +
  scale_fill_manual(values = c('Private property value loss'='#F5938D','Ambiguous-payer cost'='#b55ef1','Tax revenue loss'='#B3C5F1',
                               'Land and dwelling retreat public cost'='#658EF3','Infrastructure retreat cost'='#1953E3')) + 
  xlab('Retreat Approach')+
  ylab('Cost ($2023,mil)') +
  ggtitle(paste0(titlename)) +
  labs(fill= '') + 
  theme_minimal() +
  facet_grid(~scenario, switch = "x", scales="free") + #creates groups
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, color = "white"), #remove background color
        panel.spacing = unit(0,"cm"), # remove space between groups
        panel.grid.major.x = element_blank(), #remove vertical lines
        panel.grid.minor.x = element_blank(), # remove vertical lines
        legend.text = element_text(size = 12), #legend text size
        legend.background = element_rect(fill="white",colour="white"),
        legend.position = c(0.75,0.8), # put legend within plot
        legend.spacing.y = unit(0.25, 'cm'), #add space between legend lines
        text=element_text(size=16), #overall text size
        axis.title.x = element_text(size = 16), #xaxis text size
        axis.title.y = element_text(size = 16), #yaxis text size
        axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1)) + 
  guides(fill = guide_legend(byrow = TRUE)) #add space between legend lines

# manually export figure 




total_cost_mini <- subset(total_cost, subscenario %in% miniscenarios)
# New facet label names for scenario variable
scen.labs <- c("All-at-once", "Threshold-based","Reactive")
names(scen.labs) <- c("AO", "TB","RE")

fig3mini <- ggplot(total_cost_mini, aes(fill=factor(costtype) ,y=valueMil, x=subscenario)) + 
  geom_bar(position="stack", stat="identity", width= 0.7, colour="grey", linewidth=0.2) +
  scale_fill_manual(values = c('Private property value loss'='#F5938D','Ambiguous-payer cost'='#b55ef1','Tax revenue loss'='#B3C5F1',
                               'Land and dwelling retreat public cost'='#658EF3','Infrastructure retreat cost'='#1953E3')) + 
  xlab('Retreat Approach')+
  ylab('Cost ($2023,mil)') +
  ggtitle(paste0(titlename)) +
  labs(fill= '') + 
  theme_minimal() +
  facet_grid(~scenario, switch = "x", scales="free",
             labeller = labeller(scenario = scen.labs)) + #creates groups
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, color = "white"), #remove background color
        panel.spacing = unit(0,"cm"), # remove space between groups
        panel.grid.major.x = element_blank(), #remove vertical lines
        panel.grid.minor.x = element_blank(), # remove vertical lines
        legend.text = element_text(size = 12), #legend text size
        legend.background = element_rect(fill="white",colour="white"),
        legend.position = c(0.75,0.95), # put legend within plot
        legend.spacing.y = unit(0.25, 'cm'), #add space between legend lines
        text=element_text(size=16), #overall text size
        axis.title.x = element_text(size = 16), #xaxis text size
        axis.title.y = element_text(size = 16), #yaxis text size
        axis.text.x = element_blank(), #element_text(angle = 270, vjust = 0.5, hjust=1),
        axis.ticks.x = element_blank()) + 
  guides(fill = guide_legend(byrow = TRUE)) #add space between legend lines
# add geom_errorbar(aes(ymax = yield + SE, ymin = yield - SE), position = dodge, width = 0.2)


#plot of non-infrastructure costs
res_cost_mini <- subset(total_cost_mini, costtype != 'Infrastructure retreat cost')
resmini <- ggplot(res_cost_mini, aes(fill=factor(costtype) ,y=valueMil, x=subscenario)) + 
  geom_bar(position="stack", stat="identity", width= 0.7, colour="grey", linewidth=0.2) +
  scale_fill_manual(values = c('Private property value loss'='#F5938D','Ambiguous-payer cost'='#b55ef1','Tax revenue loss'='#B3C5F1',
                               'Land and dwelling retreat public cost'='#658EF3')) + 
  xlab('Retreat Approach')+
  ylab('Cost ($2023,mil)') +
  ggtitle(paste0(titlename, " - residential-costs only")) +
  labs(fill= '') + 
  theme_minimal() +
  facet_grid(~scenario, switch = "x", scales="free") + #creates groups
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, color = "white"), #remove background color
        panel.spacing = unit(0,"cm"), # remove space between groups
        panel.grid.major.x = element_blank(), #remove vertical lines
        panel.grid.minor.x = element_blank(), # remove vertical lines
        legend.text = element_text(size = 12), #legend text size
        legend.background = element_rect(fill="white",colour="white"),
        legend.position = c(0.75,0.8), # put legend within plot
        legend.spacing.y = unit(0.25, 'cm'), #add space between legend lines
        text=element_text(size=16), #overall text size
        axis.title.x = element_text(size = 16), #xaxis text size
        axis.title.y = element_text(size = 16), #yaxis text size
        axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1)) + 
  guides(fill = guide_legend(byrow = TRUE)) #add space between legend lines



#median home costs
nrow(clean_retreat_calcs) #number of single-family homes (non-CPR'd parcels)





# Figure 4A, cost seg over time
library(plotly)

costtime <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c('year','scenario','subscenario','cost'))
costtimeres <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c('year','scenario','subscenario','cost'))
costtimeinfra <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c('year','scenario','subscenario','cost'))
costtimeinfradetail <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c('year','scenario','subscenario','costtype','cost'))
areatime <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c('year','scenario','subscenario','area'))
years <- c(2023,2026,2030,2040,2050,2062,2075,2087,2100)
infracosttypes <- c('bridgerelocate','bridgeretrofit','hwyrelocate','hwyrelocate','waterrelocate','emdom','hwyriprap','rdremove','riprapremove','maintain') #'waterremove',



#for AO
for(trigger in triggers){
  for(hazard_type in hazard_types){
    for(seawall in seawalls){
      for(rdr in rdret){
        scenario_col <- paste0("AO",seawall,"t",trigger,"_l",hazard_type,"_bv1","_rdr",rdr)
        arearetreat_col <- paste0("arearetreat_AO",seawall,"t",trigger)
        
        costtime[nrow(costtime) + 1,] = c(year = 2023, scenario = "AO",subscenario = scenario_col,
                                          cost = cost_summary[[scenario_col]][cost_summary$CostTypes == "Total costs and losses"])
        costtimeres[nrow(costtimeres) + 1,] = c(year = 2023, scenario = "AO",subscenario = scenario_col,
                                          cost = (cost_summary[[scenario_col]][cost_summary$CostTypes == "Total costs and losses"] - 
                                            cost_summary[[scenario_col]][cost_summary$CostTypes == "Infrastructure retreat cost"]))
        costtimeinfra[nrow(costtimeinfra) + 1,] = c(year = 2023, scenario = "AO",subscenario = scenario_col,
                                                cost = cost_summary[[scenario_col]][cost_summary$CostTypes == "Infrastructure retreat cost"])
        
    
          for(infracosttype in infracosttypes){
            infratype_col <- paste0(infracosttype,"AO",seawall,trigger,"_rdr",rdr)
            
            costtimeinfradetail[nrow(costtimeinfradetail) + 1,] = c(year = 2023, scenario = "AO",subscenario = scenario_col,
                                                                    costtype = infracosttype,
                                                                    cost = infra_costtime[[infratype_col]][infra_costtime$Years == 2023]/ 1000000)
          }
        
        for(year in years){
          areatime[nrow(areatime) + 1,] = c(year = year, scenario = "AO",subscenario = scenario_col,
                                            area = Retreat_Analysis[[arearetreat_col]][Retreat_Analysis$Years == 2023])
        }
      }
    }
  }
}

#for TB
for(i in 1:length(years)){
  for(trigger in triggers){
    for(hazard_type in hazard_types){
      for(bval in bvals){
        for(seawall in seawalls){
          for(rdr in rdret){
            scenario_col <- paste0("TB",seawall,"t",trigger,"_l",hazard_type,"_bv1","_rdr",rdr)
            totalval_col <- paste0("Total_Value_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
            demo_col <- paste0("demolition_TB",seawall,"t",trigger)
            osds_col <- paste0("osdsremoval_TB",seawall,"t",trigger)
            wastewater_col <- paste0("wastewaterremoval_TB",seawall,"t",trigger)
            infra_col <- paste0("infrastructure_TB",seawall,trigger,"_rdr",rdr)
            privproploss_col <- paste0("Priv_Prop_Loss_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
            taxrevloss_col <- paste0("Total_TaxRev_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
            arearetreat_col <- paste0("arearetreat_TB",seawall,"t",trigger)
            year <- years[i]
            prevyear <- years[i-1]
            
            totalcost <- ifelse(year==2023,0,as.numeric(costtime$cost[costtime$subscenario == scenario_col & costtime$year == prevyear])) +
              ((sum(as.numeric(Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year]) +
                      as.numeric(Retreat_Analysis[[demo_col]][Retreat_Analysis$Years == year]) +
                      as.numeric(Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year]) +
                      as.numeric(Retreat_Analysis[[wastewater_col]][Retreat_Analysis$Years == year]) +
                      as.numeric(Retreat_Analysis[[privproploss_col]][Retreat_Analysis$Years == year]) +
                      as.numeric(Retreat_Analysis[[taxrevloss_col]][Retreat_Analysis$Years == year])+
                      as.numeric(Retreat_Analysis[[infra_col]][Retreat_Analysis$Years == year]))) / 
                 DiscountRate26$Discount_Rates_26[DiscountRate26$year == year]) / 1000000 
            totalcostres <- ifelse(year==2023,0,as.numeric(costtimeres$cost[costtimeres$subscenario == scenario_col & costtimeres$year == prevyear])) +
              ((sum(as.numeric(Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year]) +
                      as.numeric(Retreat_Analysis[[demo_col]][Retreat_Analysis$Years == year]) +
                      as.numeric(Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year]) +
                      as.numeric(Retreat_Analysis[[wastewater_col]][Retreat_Analysis$Years == year]) +
                      as.numeric(Retreat_Analysis[[privproploss_col]][Retreat_Analysis$Years == year]) +
                      as.numeric(Retreat_Analysis[[taxrevloss_col]][Retreat_Analysis$Years == year]))) / 
                 DiscountRate26$Discount_Rates_26[DiscountRate26$year == year]) / 1000000 
            totalcostinfra <- ifelse(year==2023,0,as.numeric(costtimeinfra$cost[costtimeinfra$subscenario == scenario_col & costtimeinfra$year == prevyear])) +
              ((sum(as.numeric(Retreat_Analysis[[infra_col]][Retreat_Analysis$Years == year]))) / 
                 DiscountRate26$Discount_Rates_26[DiscountRate26$year == year]) / 1000000 
            totalarea <- ifelse(year==2023,0,as.numeric(areatime$area[areatime$subscenario == scenario_col & areatime$year == prevyear])) +
              Retreat_Analysis[[arearetreat_col]][Retreat_Analysis$Years == year]
            
            costtime[nrow(costtime) + 1,] = c(year = year, scenario = "TB",subscenario = scenario_col,cost = totalcost)
            costtimeres[nrow(costtimeres) + 1,] = c(year = year, scenario = "TB",subscenario = scenario_col,cost = totalcostres)
            costtimeinfra[nrow(costtimeinfra) + 1,] = c(year = year, scenario = "TB",subscenario = scenario_col,cost = totalcostinfra)
            areatime[nrow(areatime) + 1,] = c(year = year, scenario = "TB",subscenario = scenario_col,area = totalarea)
            
            for(infracosttype in infracosttypes){
              infratype_col <- paste0(infracosttype,"TB",seawall,trigger,"_rdr",rdr)
              
              totalcosttypeinfra <- ifelse(year==2023,0,
                                           as.numeric(costtimeinfradetail$cost[costtimeinfradetail$subscenario == scenario_col & 
                                                                                 costtimeinfradetail$year == prevyear & 
                                                                                 costtimeinfradetail$costtype == infracosttype])) +
                ((sum(as.numeric(infra_costtime[[infratype_col]][infra_costtime$Years == year]))) / 
                   DiscountRate26$Discount_Rates_26[DiscountRate26$year == year]) / 1000000
              
              costtimeinfradetail[nrow(costtimeinfradetail) + 1,] = c(year = year, scenario = "TB",subscenario = scenario_col,
                                                                      costtype = infracosttype,cost = totalcosttypeinfra)
            }
          }
        }
      }
    }
  }
}

#for RE
for(i in 1:length(years)){
  for(trigger in triggers){
    for(hazard_type in hazard_types){
      for(cleanup in cleanups){
        for(seawall in seawalls){
          for(rdr in rdret){
            scenario_col <- paste0("RE",seawall,"t",trigger,"_l",hazard_type,"_bv0_c",cleanup,"_rdr",rdr)
            totalval_col <- paste0("Total_Value_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
            cleanup_col <- paste0("cleanup",cleanup,"_RE",seawall,"t",trigger) 
            infra_col <- paste0("infrastructure_RE",seawall,trigger,"_rdr",rdr)
            privproploss_col <- paste0("Priv_Prop_Loss_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
            taxrevloss_col <- paste0("Total_TaxRev_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
            arearetreat_col <- paste0("arearetreat_RE",seawall,"t",trigger)
            
            year <- years[i]
            prevyear <- years[i-1]
            
            totalcost <- ifelse(year==2023,0,as.numeric(costtime$cost[costtime$subscenario == scenario_col & costtime$year == prevyear])) +
              ((sum(as.numeric(Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year]) +
                      as.numeric(Retreat_Analysis[[cleanup_col]][Retreat_Analysis$Years == year]) +
                      as.numeric(Retreat_Analysis[[privproploss_col]][Retreat_Analysis$Years == year]) +
                      as.numeric(Retreat_Analysis[[taxrevloss_col]][Retreat_Analysis$Years == year]) +
                      as.numeric(Retreat_Analysis[[infra_col]][Retreat_Analysis$Years == year]))) / 
                 DiscountRate26$Discount_Rates_26[DiscountRate26$year == year]) / 1000000
            totalcostres <- ifelse(year==2023,0,as.numeric(costtimeres$cost[costtimeres$subscenario == scenario_col & costtimeres$year == prevyear])) +
              ((sum(as.numeric(Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year]) +
                      as.numeric(Retreat_Analysis[[cleanup_col]][Retreat_Analysis$Years == year]) +
                      as.numeric(Retreat_Analysis[[privproploss_col]][Retreat_Analysis$Years == year]) +
                      as.numeric(Retreat_Analysis[[taxrevloss_col]][Retreat_Analysis$Years == year]))) / 
                 DiscountRate26$Discount_Rates_26[DiscountRate26$year == year]) / 1000000
            totalcostinfra <- ifelse(year==2023,0,as.numeric(costtimeinfra$cost[costtimeinfra$subscenario == scenario_col & costtimeinfra$year == prevyear])) +
              ((sum(as.numeric(Retreat_Analysis[[infra_col]][Retreat_Analysis$Years == year]))) / 
                 DiscountRate26$Discount_Rates_26[DiscountRate26$year == year]) / 1000000 
            totalarea <- ifelse(year==2023,0,as.numeric(areatime$area[areatime$subscenario == scenario_col & areatime$year == prevyear])) +
              Retreat_Analysis[[arearetreat_col]][Retreat_Analysis$Years == year]
            
            costtime[nrow(costtime) + 1,] = c(year = year, scenario = "RE",subscenario = scenario_col,cost = totalcost)
            costtimeres[nrow(costtimeres) + 1,] = c(year = year, scenario = "RE",subscenario = scenario_col,cost = totalcostres)
            costtimeinfra[nrow(costtimeinfra) + 1,] = c(year = year, scenario = "RE",subscenario = scenario_col,cost = totalcostinfra)
            areatime[nrow(areatime) + 1,] = c(year = year, scenario = "RE",subscenario = scenario_col,area = totalarea)
            
            for(infracosttype in infracosttypes){
              infratype_col <- paste0(infracosttype,"RE",seawall,trigger,"_rdr",rdr)
              
              totalcosttypeinfra <- ifelse(year==2023,0,as.numeric(costtimeinfradetail$cost[costtimeinfradetail$subscenario == scenario_col & 
                                                                                              costtimeinfradetail$year == prevyear & 
                                                                                              costtimeinfradetail$costtype == infracosttype])) +
                ((sum(as.numeric(infra_costtime[[infratype_col]][infra_costtime$Years == year]))) / 
                   DiscountRate26$Discount_Rates_26[DiscountRate26$year == year]) / 1000000
              
              costtimeinfradetail[nrow(costtimeinfradetail) + 1,] = c(year = year, scenario = "RE",subscenario = scenario_col,
                                                                      costtype = infracosttype,cost = totalcosttypeinfra)
            }
          }
        }
      }
    }
  }
}

for(i in 1:length(years)){
  for(hazard_type in hazard_types){
    areahazard_col <- paste0("areahazard_l",hazard_type)
    year <- years[i]
    prevyear <- years[i-1]
    
    totalarea <- ifelse(year==2023,0,as.numeric(areatime$area[areatime$subscenario == areahazard_col & areatime$year == prevyear])) +
      Retreat_Analysis[[areahazard_col]][Retreat_Analysis$Years == year]
    
    areatime[nrow(areatime) + 1,] = c(year = year, scenario = hazard_type,subscenario = areahazard_col, area = totalarea)
  }
}




costtime$scenario <- factor(costtime$scenario, levels = c("AO","TB","RE"))
costtime$cost <- as.numeric(costtime$cost)
costtimeres$scenario <- factor(costtimeres$scenario, levels = c("AO","TB","RE"))
costtimeres$cost <- as.numeric(costtimeres$cost)
costtimeinfra$scenario <- factor(costtimeinfra$scenario, levels = c("AO","TB","RE"))
costtimeinfra$cost <- as.numeric(costtimeinfra$cost)
areatime$scenario <- factor(areatime$scenario, levels = c("AO","TB","RE","CE","WF"))
areatime$area <- as.numeric(areatime$area)
costtimeinfradetail$scenario <- factor(costtimeinfradetail$scenario, levels = c("AO","TB","RE"))
costtimeinfradetail$cost <- as.numeric(costtimeinfradetail$cost)


costtime_mini <- subset(costtime, subscenario %in% miniscenarios)
costtimeres_mini <- subset(costtimeres, subscenario %in% miniscenarios)
costtimeinfra_mini <- subset(costtimeinfra, subscenario %in% miniscenarios)
areatime_mini <- subset(areatime, subscenario %in% miniscenarios)
costtimeinfradetail_mini <- subset(costtimeinfradetail, subscenario %in% miniscenarios)

lpc_seg <- ggplot(costtime_mini,aes(x=year,y=cost,group=subscenario,color=scenario))+
  #geom_linerange(aes(ymin=min.cost,ymax=max.cost,linewidth=0.001,alpha=0.2))+
  #geom_errorbar(aes(ymin = min.cost, ymax = max.cost), width = 0.9,alpha=0.4)+
  geom_point(shape=15,size=4)+
  theme_bw()+
  scale_color_manual(name="Retreat approach",labels=c("All-at-once","Threshold-based","Reactive"),
                     values=c("#75bf67", "#2E9FDF", "#FC4E07"))+ #,"#E7B800"
  scale_y_continuous(name="Cumulative total cost ($2023, millions)",labels = scales::label_number(scale = 1))+
  xlab("Year") 

lpb <- ggplot(areatime_mini, aes(x=year,y=area, group=subscenario,color=scenario))+
  #geom_smooth(aes(color=retreat.scenario),linewidth=1.5,se=F)+
  geom_line(aes(color=scenario),linewidth=1.5)+
  scale_color_manual(name="Retreat approach",labels=c("All-at-once","Threshold-based","Reactive","Coastal erosion","Wave flooding"),
                     values=c("#75bf67", "#2E9FDF", "#FC4E07","grey","#E7B800"))+ 
  theme_bw()+
  scale_y_continuous(name=expression("Total area retreated "~(m^2)),labels = scales::comma)+
  xlab("Year")

fig4 <- ggarrange(lpc_seg,lpb,
                  labels=c("A","B"),
                  ncol=1,nrow=2,
                  align="v")
fig4 <- annotate_figure(fig4,top=text_grob(titlename))
print(fig4)

#export as pdf: 7in x 6in. remove duplicate legend. export pdf to image

fig4res <- ggplot(costtimeres_mini,aes(x=year,y=cost,group=subscenario,color=scenario))+
  #geom_linerange(aes(ymin=min.cost,ymax=max.cost,linewidth=0.001,alpha=0.2))+
  #geom_errorbar(aes(ymin = min.cost, ymax = max.cost), width = 0.9,alpha=0.4)+
  geom_point(shape=15,size=4)+
  theme_bw()+
  scale_color_manual(name="Retreat approach",labels=c("AO","TB","RE"),
                     values=c("#75bf67", "#2E9FDF", "#FC4E07"))+ #,"#E7B800"
  scale_y_continuous(name="Cumulative total cost ($2023, millions)",labels = label_number(scale = 1))+
  xlab("Year")+
  ggtitle('Residential')

fig4infra <- ggplot(costtimeinfra_mini,aes(x=year,y=cost,group=subscenario,color=scenario))+
  #geom_linerange(aes(ymin=min.cost,ymax=max.cost,linewidth=0.001,alpha=0.2))+
  #geom_errorbar(aes(ymin = min.cost, ymax = max.cost), width = 0.9,alpha=0.4)+
  geom_point(shape=15,size=4)+
  theme_bw()+
  scale_color_manual(name="Retreat approach",labels=c("AO","TB","RE"),
                     values=c("#75bf67", "#2E9FDF", "#FC4E07"))+ #,"#E7B800"
  scale_y_continuous(name="Cumulative total cost ($2023, millions)",labels = label_number(scale = 1))+
  xlab("Year")+
  ggtitle('Infrastructure')

fig4resinfra <- ggarrange(fig4res,fig4infra,
                  labels=c("A1","A2"),
                  ncol=2,nrow=1,
                  align="h")
fig4resinfra <- annotate_figure(fig4resinfra,top=text_grob(titlename))




figinfra <- ggplot(costtimeinfradetail_mini,aes(x=year,y=cost,group=subscenario,color=costtype))+
  #geom_linerange(aes(ymin=min.cost,ymax=max.cost,linewidth=0.001,alpha=0.2))+
  #geom_errorbar(aes(ymin = min.cost, ymax = max.cost), width = 0.9,alpha=0.4)+
  geom_point(shape=15,size=2)+
  #geom_line()+ geom_point()+
  theme_bw()+
  #scale_color_manual(name="Retreat approach",labels=c("AO","TB","RE"),
  #                   values=c("#75bf67", "#2E9FDF", "#FC4E07"))+ #,"#E7B800"
  scale_y_continuous(name="Cumulative total cost ($2023, millions)",labels = label_number(scale = 1))+
  xlab("Year")+
  facet_wrap(~scenario,ncol=1)+
  ggtitle(titlename)






### pie chart / tree map

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
  tm[nrow(tm)+1,] <- c(scenario=scen[i],group="Land and dwelling retreat public cost",subgroup="Acquisition",
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

figpie <- ggplot(tm,aes(area=cost,fill=group,
                       label=paste(subgroup,scales::percent(percent,accuracy=1),sep ="\n"),
                       subgroup=subgroup))+
  treemapify::geom_treemap(layout="squarified")+
  geom_treemap_text(color='white',place = "centre",size = 15)+
  geom_treemap_subgroup_border(colour = "white", size = 3)+
  scale_fill_brewer("Cost types",palette = "Set2")+
  scale_fill_manual(values = c('Private Property Value Loss'='#F5938D','Ambiguous'='#b55ef1','Tax revenue loss'='#B3C5F1',
                             'Land and dwelling retreat public cost'='#658EF3','Infrastructure retreat cost'='#1953E3'),
                    limits = c('Private Property Value Loss','Ambiguous','Tax revenue loss','Land and dwelling retreat public cost','Infrastructure retreat cost')) + 
  ggtitle(paste0(titlename))+
  facet_wrap(~factor(scenario,levels=c('AO','TB','RE')), ncol = 3,labeller = as_labeller( c('AO'='All-at-once','TB'='Threshold-based','RE'='Reactive'))) +
  theme(legend.position="bottom",strip.text.x = element_text(size = 15),panel.spacing=unit(1,"lines"),legend.title=element_blank())
print(figpie)









plot4a <- plot_ly(costtime, type = 'scatter', mode = 'markers') 
plot4a <- plot4a %>%
  add_trace(
    x = ~year, 
    y = ~cost, 
    color= ~scenario,
    text= ~subscenario,
    hovertemplate = paste(
      "<b>%{text}</b><br><br>",
      "%{yaxis.title.text}: %{y:$,.0f}<br>",
      "%{xaxis.title.text}: %{x:.}<br>",
      "<extra></extra>"
    ),
    showlegend = T
  ) 
plot4a

plot4b <- plot_ly(areatime,type = 'scatter', mode = 'lines+markers')
plot4b<- plot4b %>%
  add_trace(
    x = ~year, 
    y = ~area, 
    color= ~scenario,
    text= ~subscenario,
    legendgroup = ~scenario,
    split = ~subscenario,
    hovertemplate = paste(
      "<b>%{text}</b><br><br>",
      "%{yaxis.title.text}: %{y:.0f}<br>",
      "%{xaxis.title.text}: %{x:.}<br>",
      "<extra></extra>"
    ),
    showlegend = F
  ) 
plot4b








