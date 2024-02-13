#naming standard for sub-scenarios: RE_s_tXA_lCE_bv1_chi (approach = AO, trigger = XA, land transfer = CE, building value = 1,cleanup cost = high, seawall = stay)

library(dplyr)
library(ggplot2)
library(ggpubr)
library(data.table)
setwd("F:/slr/kauai/kauai_retreat_code/")


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
      totalval_col <- paste0("Total_Value_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
      demo_col <- paste0("demolition_AO",seawall,"t",trigger)
      osds_col <- paste0("osdsremoval_AO",seawall,"t",trigger)
      infra_col <- paste0("infrastructure_AO",seawall)
      taxrevloss_col <- paste0("Total_TaxRev_Loss_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
      privproploss_col <- paste0("Priv_Prop_Loss_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
      scenario_col <- paste0("AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
      
      cost_summary[[scenario_col]] <- c(sum(Retreat_Analysis_Total[[totalval_col]],Retreat_Analysis_Total[[demo_col]],Retreat_Analysis_Total[[osds_col]]),
                                        0,
                                        Retreat_Analysis_Total[[infra_col]],
                                        Retreat_Analysis_Total[[taxrevloss_col]],
                                        Retreat_Analysis_Total[[privproploss_col]])
      
      total_cost[nrow(total_cost) + 1,] = c(management = "Managed",scenario = "AO",trigger = trigger,hazard = hazard_type,bval = 0, cleanup = 0, seawall = seawall, subscenario = scenario_col, 
                                            costtype = "Land and dwelling retreat public cost", 
                                            value = sum(Retreat_Analysis_Total[[totalval_col]],Retreat_Analysis_Total[[demo_col]],Retreat_Analysis_Total[[osds_col]]))
      total_cost[nrow(total_cost) + 1,] = c(management = "Managed",scenario = "AO",trigger = trigger,hazard = hazard_type,bval = 0, cleanup = 0, seawall = seawall, subscenario = scenario_col, 
                                            costtype = "Land and dwelling retreat private cost", 
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

#for TB
for(trigger in triggers){
  for(hazard_type in hazard_types){
    for(bval in bvals){
      for(seawall in seawalls){
        totalval_col <- paste0("Total_Value_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
        demo_col <- paste0("demolition_TB",seawall,"t",trigger)
        osds_col <- paste0("osdsremoval_TB",seawall,"t",trigger)
        infra_col <- paste0("infrastructure_TB",seawall)
        taxrevloss_col <- paste0("Total_TaxRev_Loss_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
        privproploss_col <- paste0("Priv_Prop_Loss_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
        scenario_col <- paste0("TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
        
        cost_summary[[scenario_col]] <- c(sum(Retreat_Analysis_Total[[totalval_col]],Retreat_Analysis_Total[[demo_col]],Retreat_Analysis_Total[[osds_col]]),
                                          0,
                                          Retreat_Analysis_Total[[infra_col]],
                                          Retreat_Analysis_Total[[taxrevloss_col]],
                                          Retreat_Analysis_Total[[privproploss_col]])
        
        total_cost[nrow(total_cost) + 1,] = c(management = "Managed",scenario = "TB",trigger = trigger,hazard = hazard_type,bval = bval, cleanup = 0, seawall = seawall, subscenario = scenario_col, 
                                              costtype = "Land and dwelling retreat public cost", 
                                              value = sum(Retreat_Analysis_Total[[totalval_col]],Retreat_Analysis_Total[[demo_col]],Retreat_Analysis_Total[[osds_col]]))
        total_cost[nrow(total_cost) + 1,] = c(management = "Managed",scenario = "TB",trigger = trigger,hazard = hazard_type,bval = bval, cleanup = 0, seawall = seawall, subscenario = scenario_col, 
                                              costtype = "Land and dwelling retreat private cost", 
                                              value = 0)
        total_cost[nrow(total_cost) + 1,] = c(management = "Managed",scenario = "TB",trigger = trigger,hazard = hazard_type,bval = bval, cleanup = 0, seawall = seawall, subscenario = scenario_col, 
                                              costtype = "Infrastructure retreat cost", 
                                              value = Retreat_Analysis_Total[[infra_col]])
        total_cost[nrow(total_cost) + 1,] = c(management = "Managed",scenario = "TB",trigger = trigger,hazard = hazard_type,bval = bval, cleanup = 0, seawall = seawall, subscenario = scenario_col, 
                                              costtype = "Tax revenue loss", 
                                              value = Retreat_Analysis_Total[[taxrevloss_col]])
        total_cost[nrow(total_cost) + 1,] = c(management = "Managed",scenario = "TB",trigger = trigger,hazard = hazard_type,bval = bval, cleanup = 0, seawall = seawall, subscenario = scenario_col, 
                                              costtype = "Private property value loss", 
                                              value = Retreat_Analysis_Total[[privproploss_col]])
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
        totalval_col <- paste0("Total_Value_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
        cleanup_col <- paste0("cleanup",cleanup,"_RE",seawall,"t",trigger) 
        infra_col <- paste0("infrastructure_RE",seawall)
        taxrevloss_col <- paste0("Total_TaxRev_Loss_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
        privproploss_col <- paste0("Priv_Prop_Loss_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
        scenario_col <- paste0("RE",seawall,"t",trigger,"_l",hazard_type,"_bv0_c",cleanup)
        
        cost_summary[[scenario_col]] <- c(Retreat_Analysis_Total[[totalval_col]],
                                          Retreat_Analysis_Total[[cleanup_col]],
                                          Retreat_Analysis_Total[[infra_col]],
                                          Retreat_Analysis_Total[[taxrevloss_col]],
                                          Retreat_Analysis_Total[[privproploss_col]])
        
        total_cost[nrow(total_cost) + 1,] = c(management = "Unmanaged",scenario = "RE",trigger = trigger,hazard = hazard_type,bval = 0, cleanup = cleanup, seawall = seawall, subscenario = scenario_col, 
                                              costtype = "Land and dwelling retreat public cost", 
                                              value = Retreat_Analysis_Total[[totalval_col]])
        total_cost[nrow(total_cost) + 1,] = c(management = "Unmanaged",scenario = "RE",trigger = trigger,hazard = hazard_type,bval = 0, cleanup = cleanup, seawall = seawall, subscenario = scenario_col, 
                                              costtype = "Ambiguous-payer cost", 
                                              value = Retreat_Analysis_Total[[cleanup_col]])
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
                                  'Infrastructure retreat cost','Land and dwelling retreat public cost'))

ggplot(total_cost, aes(fill=factor(costtype) ,y=valueMil, x=subscenario)) + 
  geom_bar(position="stack", stat="identity", width= 0.7, colour="grey", linewidth=0.2) +
  scale_fill_manual(values = c('Private property value loss'='#F5938D','Ambiguous-payer cost'='#b55ef1','Tax revenue loss'='#B3C5F1',
                               'Infrastructure retreat cost'='#658EF3','Land and dwelling retreat public cost'='#1953E3')) + 
  xlab('Retreat Approach')+
  ylab('Cost ($2023,mil)') +
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






# Figure 4A, cost seg over time
library(plotly)

costtime <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c('year','scenario','subscenario','cost'))
areatime <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c('year','scenario','subscenario','area'))
years <- c(2023,2026,2030,2040,2050,2062,2075,2087,2100)

#for AO
for(trigger in triggers){
  for(hazard_type in hazard_types){
    scenario_col <- paste0("AO_t",trigger,"_l",hazard_type,"_bv1")
    arearetreat_col <- paste0("arearetreat_AO_t",trigger)
    
    costtime[nrow(costtime) + 1,] = c(year = 2023, scenario = "AO",subscenario = scenario_col,
                                      cost = cost_summary[[scenario_col]][cost_summary$CostTypes == "Total costs and losses"])
    for(year in years){
      areatime[nrow(areatime) + 1,] = c(year = year, scenario = "AO",subscenario = scenario_col,
                                        area = Retreat_Analysis[[arearetreat_col]][Retreat_Analysis$Years == 2023])
    }
  }
}

#for TB
for(i in 1:length(years)){
  for(trigger in triggers){
    for(hazard_type in hazard_types){
      for(bval in bvals){
        scenario_col <- paste0("TB_t",trigger,"_l",hazard_type,"_bv",bval)
        totalval_col <- paste0("Total_Value_TB_t",trigger,"_l",hazard_type,"_bv",bval)
        demo_col <- paste0("demolition_TB_t",trigger)
        osds_col <- paste0("osdsremoval_TB_t",trigger)
        infra_col <- paste0("TB_Infra")
        privproploss_col <- paste0("Priv_Prop_Loss_TB_t",trigger,"_l",hazard_type,"_bv",bval)
        taxrevloss_col <- paste0("Total_TaxRev_TB_t",trigger,"_l",hazard_type,"_bv",bval)
        arearetreat_col <- paste0("arearetreat_TB_t",trigger)
        scenario_col <- paste0("TB_t",trigger,"_l",hazard_type,"_bv",bval)
        year <- years[i]
        prevyear <- years[i-1]
        
        totalcost <- ifelse(year==2023,0,as.numeric(costtime$cost[costtime$subscenario == scenario_col & costtime$year == prevyear])) +
          ((sum(as.numeric(Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year]) +
                  as.numeric(Retreat_Analysis[[demo_col]][Retreat_Analysis$Years == year]) +
                  as.numeric(Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year]) +
                  as.numeric(Retreat_Analysis[[privproploss_col]][Retreat_Analysis$Years == year]) +
                  as.numeric(Retreat_Analysis[[taxrevloss_col]][Retreat_Analysis$Years == year]))) / 
             DiscountRate26$Discount_Rates_26[DiscountRate26$year == year]  + 
             Retreat_Analysis$TB_Infrastructure[Retreat_Analysis$Years == year]) / 1000000 #** are infrastructure costs already discounted?
        totalarea <- ifelse(year==2023,0,as.numeric(areatime$area[areatime$subscenario == scenario_col & areatime$year == prevyear])) +
          Retreat_Analysis[[arearetreat_col]][Retreat_Analysis$Years == year]
        
        costtime[nrow(costtime) + 1,] = c(year = year, scenario = "TB",subscenario = scenario_col,cost = totalcost)
        areatime[nrow(areatime) + 1,] = c(year = year, scenario = "TB",subscenario = scenario_col,area = totalarea)
      }
    }
  }
}

#for RE
for(i in 1:length(years)){
  for(trigger in triggers){
    for(hazard_type in hazard_types){
      for(cleanup in cleanups){
        scenario_col <- paste0("RE_t",trigger,"_l",hazard_type,"_bv0_c",cleanup)
        totalval_col <- paste0("Total_Value_RE_t",trigger,"_l",hazard_type,"_bv0")
        cleanup_col <- paste0("cleanup",cleanup,"_RE_t",trigger) 
        infra_col <- paste0("RE_Infra")
        privproploss_col <- paste0("Priv_Prop_Loss_RE_t",trigger,"_l",hazard_type,"_bv0")
        taxrevloss_col <- paste0("Total_TaxRev_RE_t",trigger,"_l",hazard_type,"_bv0")
        arearetreat_col <- paste0("arearetreat_RE_t",trigger)
        
        year <- years[i]
        prevyear <- years[i-1]
        
        totalcost <- ifelse(year==2023,0,as.numeric(costtime$cost[costtime$subscenario == scenario_col & costtime$year == prevyear])) +
          ((sum(as.numeric(Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year]) +
                  as.numeric(Retreat_Analysis[[cleanup_col]][Retreat_Analysis$Years == year]) +
                  as.numeric(Retreat_Analysis[[privproploss_col]][Retreat_Analysis$Years == year]) +
                  as.numeric(Retreat_Analysis[[taxrevloss_col]][Retreat_Analysis$Years == year]))) / 
             DiscountRate26$Discount_Rates_26[DiscountRate26$year == year] + 
             Retreat_Analysis$RE_Infrastructure[Retreat_Analysis$Years == year]) / 1000000
        totalarea <- ifelse(year==2023,0,as.numeric(areatime$area[areatime$subscenario == scenario_col & areatime$year == prevyear])) +
          Retreat_Analysis[[arearetreat_col]][Retreat_Analysis$Years == year]
        
        costtime[nrow(costtime) + 1,] = c(year = year, scenario = "RE",subscenario = scenario_col,cost = totalcost)
        areatime[nrow(areatime) + 1,] = c(year = year, scenario = "RE",subscenario = scenario_col,area = totalarea)
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
areatime$scenario <- factor(areatime$scenario, levels = c("AO","TB","RE","CE","WF"))
areatime$area <- as.numeric(areatime$area)




library(scales)
lpc_seg <- ggplot(costtime,aes(x=year,y=cost,group=subscenario,color=scenario))+
  #geom_linerange(aes(ymin=min.cost,ymax=max.cost,linewidth=0.001,alpha=0.2))+
  #geom_errorbar(aes(ymin = min.cost, ymax = max.cost), width = 0.9,alpha=0.4)+
  geom_point(shape=15,size=4)+
  theme_bw()+
  scale_color_manual(name="Retreat approach",labels=c("AO","TB","RE"),
                     values=c("#75bf67", "#2E9FDF", "#FC4E07"))+ #,"#E7B800"
  scale_y_continuous(name="Cumulative total cost ($2023, millions)",labels = label_number(scale = 1))+
  xlab("Year") 

lpb <- ggplot(areatime, aes(x=year,y=area, group=subscenario,color=scenario))+
  #geom_smooth(aes(color=retreat.scenario),linewidth=1.5,se=F)+
  geom_line(aes(color=scenario),linewidth=1.5)+
  scale_color_manual(name="Retreat approach",labels=c("AO","TB","RE","CE","WF"),
                     values=c("#75bf67", "#2E9FDF", "#FC4E07","grey","#E7B800"))+ 
  theme_bw()+
  scale_y_continuous(name=expression("Total area retreated "~(m^2)),labels = scales::comma)+
  xlab("Year")

fig4 <- ggarrange(lpc_seg,lpb,
                  labels=c("A","B"),
                  ncol=1,nrow=2,
                  align="v")
print(fig4)
#export as pdf: 7in x 6in. remove duplicate legend. export pdf to image

library(plotly) #https://plotly.com/r/
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