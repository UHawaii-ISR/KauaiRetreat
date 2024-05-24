

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
emdomfile <- "F:/slr/kauai/slrxa-adj" #folder with only parcels that are SLRXA-adjacent



# run full scripts
source("C:/Users/rsett/Documents/KauaiRetreat/1assessorsk.R")
source("C:/Users/rsett/Documents/KauaiRetreat/1infrastructurek.R") #takes 15 min to run this code

# apply regional/ahupuaa/moku filter if desired
# community filters: 'ahupuaa','moku',"devplan_","devplan_id","district",'LittrlCell','Community',"dp","ballottype"
communitytype <- 'Community' #indicate the name of the community column used to filter
communityfilter <- NA #use NA if want entire island. otherwise 'Kapaʻa' 'Kekaha'  
titlename <- 'Kauaʻi' # Kapaʻa Kekaha Kauaʻi  #this is for the figure label
allisland <- clean_retreat_calcs

#filter both clean_retreat_calcs and infra_retreat
kekaha <- clean_retreat_calcs[clean_retreat_calcs[[communitytype]]==communityfilter,] 
infra_retreat <- infra_retreat[infra_retreat[[communitytype]]==communityfilter,] 

######### median home filter
houses <- subset(clean_retreat_calcs, Number_CPRbldg== 1) 
houses <- subset(houses, NEAR_CE32== 0)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
modehome <- as.data.frame(apply(houses[1:13],2,FUN=Mode))
modhome <- transpose(modehome)
colnames(modhome) <- rownames(modehome)
medhome <- as.data.frame(apply(houses[14:62],2,median,na.rm=T))
medianhome <- transpose(medhome)
colnames(medianhome) <- rownames(medhome)
medianhome$TMK <- modhome$TMK
medhome <- left_join(modhome,medianhome,by='TMK')

#overwrite clean_retreat_calcs to selected ahupuaa/qaqc
clean_retreat_calcs <- medhome


source("C:/Users/rsett/Documents/KauaiRetreat/2retreatyearvaluetaxk.R")
source("C:/Users/rsett/Documents/KauaiRetreat/3costsovertimek.R")
source("C:/Users/rsett/Documents/KauaiRetreat/3infrastructurek.R") 
source("C:/Users/rsett/Documents/KauaiRetreat/4discountedtotalcostsk.R")
miniscenarios <- c('AO_tCE_lfull_bv1_rdr1',#'AO_tCE_lfull_bv1_rdr0',
                   'TB_tCE_lCE_bv1_rdr1',#'TB_tCE_lCE_bv1_rdr0',
                   'RE_tCE_lCE_bv0_chi_rdr1',#'RE_tCE_lCE_bv0_chi_rdr0',
                   'areahazard_lCE')#,'areahazard_lWF') 
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
write.csv(infra_costtime,'infrastructure_kauai.csv',row.names=F)

#median home cost tables
write.csv(medhome,'medianhome_kauai.csv')
write.csv(total_cost_mini,'medianhome_costs.csv',row.names=F)

#infra qaqc
#write.csv(infra_retreat,'infrastructure_id_kauai.csv',row.names=F)



