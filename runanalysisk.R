

#tell R where your files are
workdir <- "F:/slr/kauai/kauai_retreat_code/" # your working directory
assessorsfile <- "F:/slr/kauai/2023_Real_Property_Tax_Data" #original assessors file (used to calculate eminent domain)
noncprshpfolder <- "F:/slr/kauai/CosmosTiff" #the folder where non-CPR bldg footprint shapefile is
noncprshplayer <- "Buildings_XA_nonCPR" # "Buildings_XA" #layer name for non-CPR bldg footprint shapefile
cprshpfolder <- "F:/slr/kauai/CosmosTiff" #the folder for CPR bldg ftprt shapefile
cprshplayer <- "Buildings_XA_CPR" # "Buildings_XA_CPR"# layer name for CPR bldg ftprt shapefile
osdsfile <- "F:/slr/kauai/OSDSv6_Exploded_ALL.csv" #the osds file
seawallfile <- "F:/slr/kauai/TMK_realign_seawalls" #the folder with seawall shapefile
infrastructurefolder <- "F:/slr/kauai/CosmosTiff" #the folder that has all of the infrastructure hazard files
emdomfile <- "F:/slr/kauai/slrxa-adj" #folder with only parcels that are SLRXA-adjacent



# run full scripts
source("C:/Users/rsett/Documents/KauaiRetreat/1assessorsk.R")
source("C:/Users/rsett/Documents/KauaiRetreat/1infrastructurek.R") #takes 15 min to run this code
allisland <- clean_retreat_calcs
allinfra <- infra_retreat

# apply regional/ahupuaa/moku filter if desired
# community filters: 'ahupuaa','moku',"devplan_","devplan_id","district",'LittrlCell','Community',"dp","ballottype"
communitytype <- 'Community' #indicate the name of the community column used to filter
communityfilter <- 'Kapaʻa' #use NA if want entire island. otherwise 'Kapaʻa' 'Kekaha'  
titlename <- 'Kapaʻa' # Kapaʻa Kekaha Kauaʻi  #this is for the figure label

#filter both clean_retreat_calcs and infra_retreat
kekaha <- allisland[allisland[[communitytype]]==communityfilter,] 
kekaha <- kekaha[!is.na(kekaha[[communitytype]]),]
kapaa <- allisland[allisland[[communitytype]]==communityfilter,] 
kapaa <- kapaa[!is.na(kapaa[[communitytype]]),]


######### median home filter
houses <- subset(allisland, Number_CPRbldg== 1) 
houses <- subset(houses, NEAR_CE32== 0)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
modehome <- as.data.frame(apply(houses[1:13],2,FUN=Mode))
modhome <- transpose(modehome)
colnames(modhome) <- rownames(modehome)
medhome <- as.data.frame(apply(houses[14:ncol(houses)],2,median,na.rm=T))
medianhome <- transpose(medhome)
colnames(medianhome) <- rownames(medhome)
medianhome$TMK <- modhome$TMK
medhome <- left_join(modhome,medianhome,by='TMK')

#overwrite clean_retreat_calcs to selected ahupuaa/qaqc
clean_retreat_calcs <- kapaa
infra_retreat <- allinfra[allinfra[[communitytype]]==communityfilter,] 

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
print(fig3mini) #just scenarios of interest to kauai
print(fig4)
print(figpie)

ggsave(paste0('fig3_',titlename,'.png'),bg='white',fig3mini,width=7,height=5,dpi=300,units='in')
ggsave(paste0('fig4_',titlename,'.png'), fig4, bg='transparent',width=6,height=6.5,dpi=300,units='in')
ggsave(paste0('treemap_',titlename,'.png'), figpie, bg='transparent',width=8.5,height=4.5,dpi=300,units='in')


#save tables as csv
write.csv(Retreat_Analysis, paste0("Retreat_Analysis_",titlename,".csv"),row.names=F)
write.csv(Retreat_Analysis_Total, paste0("Retreat_Analysis_Total_",titlename,".csv"),row.names=F)
write.csv(total_cost, paste0("total_cost_",titlename,".csv"),row.names=F)
write.csv(total_cost_mini, paste0("total_cost_mini_",titlename,".csv"),row.names=F)
write.csv(infra_costtime,paste0('infrastructure_',titlename,'.csv'),row.names=F)
write.csv(tm,paste0('treemap_',titlename,'.csv'),row.names=F)

#median home cost tables
write.csv(medhome,'medianhome_kauai.csv')
medhome_cost_mini <- subset(total_cost_mini, costtype!="Infrastructure retreat cost")
write.csv(medhome_cost_mini,'medianhome_costs.csv',row.names=F)

#infra qaqc
write.csv(infra_retreat,'infrastructure_id_kauai.csv',row.names=F)



