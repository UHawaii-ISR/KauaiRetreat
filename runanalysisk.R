# run full scripts

source("C:/Users/rsett/Documents/KauaiRetreat/1assessorsk.R")

# add regional/ahupuaa/moku filter if desired

allisland <- clean_retreat_calcs
kekaha <- subset(clean_retreat_calcs, Community=='Kekaha') #clean_retreat_calcs[clean_retreat_calcs$Community == "Kekaha", ]
kapaa <- subset(clean_retreat_calcs, Community=='Kapaʻa') #clean_retreat_calcs[clean_retreat_calcs$Community == "Kapaʻa", ]

#overwrite clean_retreat_calcs to selected ahupuaa/qaqc
clean_retreat_calcs <- kapaa
titlename <- 'Kapaʻa' # Kapaʻa Kekaha Kauaʻi  #this is for the figure label
communityfilter <- 'Kapaʻa' #use NA if want entire island. otherwise 'Kapaʻa' 'Kekaha'  #this is for infrastructure

source("C:/Users/rsett/Documents/KauaiRetreat/2retreatyearvaluetaxk.R")
source("C:/Users/rsett/Documents/KauaiRetreat/3costsovertimek.R")
source("C:/Users/rsett/Documents/KauaiRetreat/3infrastructurek.R")
source("C:/Users/rsett/Documents/KauaiRetreat/4discountedtotalcostsk.R")

#define subscenarios of interest for figures
miniscenarios <- c('AO_tXA_lfull_bv1','AO_tCE_lfull_bv1', 
                   'TB_tCE_lWF_bv1','TB_tCE_lCE_bv1','TB_s_tXA_lWF_bv1','TB_s_tXA_lCE_bv1',
                   'RE_tCE_lnone_bv0_chi','RE_tCE_lnone_bv0_clo','RE_s_tCE_lnone_bv0_chi','RE_s_tCE_lnone_bv0_clo',
                   'RE_tCE_lCE_bv0_chi','RE_tCE_lCE_bv0_clo','RE_s_tCE_lCE_bv0_chi','RE_s_tCE_lCE_bv0_clo', 
                   'areahazard_lCE','areahazard_lWF') 
source("C:/Users/rsett/Documents/KauaiRetreat/5figuresk.R")

#view figures of interest
print(fig3) #all scenarios
print(fig3mini) #just scenarios of interest to kauai
print(resmini)
print(fig4)
print(fig4res)
print(fig4infra)
print(fig4resinfra)
print(figinfra)

#save tables as csv
write.csv(Retreat_Analysis, "Retreat_Analysis_kapaa.csv",row.names=F)
write.csv(Retreat_Analysis_Total, "Retreat_Analysis_Total_kapaa.csv",row.names=F)
write.csv(total_cost, "total_cost_kapaa.csv",row.names=F)
write.csv(infra_costtime,'infrastructure_kapaa.csv',row.names=F)





