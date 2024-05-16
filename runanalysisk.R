

#tell R where your files are
workdir <- "F:/slr/kauai/kauai_retreat_code/" # your working directory
assessorsfile <- "F:/slr/kauai/2023_Real_Property_Tax_Data" #original assessors file (used to calculate eminent domain)
noncprshpfolder <- "F:/slr/kauai/Bldg_Footprints" #the folder where non-CPR bldg footprint shapefile is
noncprshplayer <- "Buildings_XA_nonCPR" #layer name for non-CPR bldg footprint shapefile
cprshpfolder <- "F:/slr/kauai/Bldg_Footprints" #the folder for CPR bldg ftprt shapefile
cprshplayer <- "Buildings_XA_CPR"# layer name for CPR bldg ftprt shapefile
osdsfile <- "F:/slr/kauai/OSDSv6_Exploded_ALL.csv" #the osds file
seawallfile <- "F:/slr/kauai/TMK_realign_seawalls" #the folder with seawall shapefile
infrastructurefolder <- "F:/slr/kauai/Alanui/Hazards" #the folder that has all of the infrastructure hazard files

# run full scripts

source("C:/Users/rsett/Documents/KauaiRetreat/1assessorsk.R")

# add regional/ahupuaa/moku filter if desired
# community filters: 'ahupuaa','moku',"devplan_","devplan_id","district",'LittrlCell','Community',"dp","ballottype"
communitytype <- 'Community' #indicate the name of the community column used to filter
communityfilter <- NA #use NA if want entire island. otherwise 'Kapaʻa' 'Kekaha'  

allisland <- clean_retreat_calcs
kekaha <- clean_retreat_calcs[clean_retreat_calcs[[communitytype]]==communityfilter,] 
#kapaa <- subset(clean_retreat_calcs, Community=='Kapaʻa') #clean_retreat_calcs[clean_retreat_calcs$Community == "Kapaʻa", ]

#overwrite clean_retreat_calcs to selected ahupuaa/qaqc
clean_retreat_calcs <- kapaa
titlename <- 'Kauaʻi' # Kapaʻa Kekaha Kauaʻi  #this is for the figure label

source("C:/Users/rsett/Documents/KauaiRetreat/2retreatyearvaluetaxk.R")
source("C:/Users/rsett/Documents/KauaiRetreat/3costsovertimek.R")
source("C:/Users/rsett/Documents/KauaiRetreat/3infrastructurek.R")
source("C:/Users/rsett/Documents/KauaiRetreat/4discountedtotalcostsk.R")

#define subscenarios of interest for figures
miniscenarios <- c('AO_tXA_lfull_bv1','AO_tCE_lfull_bv1', 
                   'TB_tCE_lWF_bv1','TB_tCE_lCE_bv1','TB_s_tCE_lWF_bv1','TB_s_tCE_lCE_bv1',
                   'RE_tCE_lnone_bv0_chi','RE_tCE_lnone_bv0_clo','RE_s_tCE_lnone_bv0_chi','RE_s_tCE_lnone_bv0_clo',
                   'RE_tCE_lCE_bv0_chi','RE_tCE_lCE_bv0_clo','RE_s_tCE_lCE_bv0_chi','RE_s_tCE_lCE_bv0_clo', 
                   'areahazard_lCE','areahazard_lWF') 
source("C:/Users/rsett/Documents/KauaiRetreat/5figuresk.R")

#view figures of interest
print(fig3) #all scenarios
print(fig3mini) #just scenarios of interest to kauai
print(resmini)
print(fig4)
#print(fig4res)
#print(fig4infra)
print(fig4resinfra)
print(figinfra)

#save tables as csv
write.csv(Retreat_Analysis, "Retreat_Analysis_kekaha.csv",row.names=F)
write.csv(Retreat_Analysis_Total, "Retreat_Analysis_Total_kekaha.csv",row.names=F)
write.csv(total_cost, "total_cost_kekaha.csv",row.names=F)
write.csv(infra_costtime,'infrastructure_kekaha.csv',row.names=F)





