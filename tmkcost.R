#create tmk-level output for Kapa'a

source("C:/Users/rsett/Documents/KauaiRetreat/1assessorsk.R")
source("C:/Users/rsett/Documents/KauaiRetreat/1infrastructurek.R") #takes 15 min to run this code

allbldg <- clean_assessors_bldg
allisland <- clean_retreat_calcs
allinfra <- infra_retreat

#filter to just kapaa
communitytype <- 'Community' #indicate the name of the community column used to filter
communityfilter <- 'Kapaʻa' #use NA if want entire island. otherwise 'Kapaʻa' 'Kekaha'  
titlename <- 'Kapaʻa' # Kapaʻa Kekaha Kauaʻi  #this is for the figure label

#filter both clean_retreat_calcs and infra_retreat
kapaa <- allisland[allisland[[communitytype]]==communityfilter,] 
kapaa <- kapaa[!is.na(kapaa[[communitytype]]),]
kapaainfra <- allinfra[allinfra[[communitytype]]==communityfilter,] 
kapaainfra <- kapaainfra[!is.na(kapaainfra[[communitytype]]),]

#set up dataframe to store costs per TMK
cost_TMK <- setNames(data.frame(matrix(ncol = 19, nrow = 0)), c('TMK','AO_yearretreat','TB_yearretreat','RE_yearretreat',
                                                                'AO_parcelpubliccost','AO_ambiguouscost','AO_taxrevloss','AO_propertyvalloss','AO_totalcost',
                                                                'TB_parcelpubliccost','TB_ambiguouscost','TB_taxrevloss','TB_propertyvalloss','TB_totalcost',
                                                                'RE_parcelpubliccost','RE_ambiguouscost','RE_taxrevloss','RE_propertyvalloss','RE_totalcost'))

#calculate total costs per tmk
kapaatmk <- unique(kapaa$TMK)
for(tmk in kapaatmk){
  clean_retreat_calcs <- kapaa[kapaa$TMK == tmk,]
  clean_assessors_bldg <- allbldg
  
  source("C:/Users/rsett/Documents/KauaiRetreat/2retreatyearvaluetaxk.R")
  source("C:/Users/rsett/Documents/KauaiRetreat/3costsovertimek.R")
  source("C:/Users/rsett/Documents/KauaiRetreat/3infrastructurek.R") 
  source("C:/Users/rsett/Documents/KauaiRetreat/4discountedtotalcostsk.R")
  
  AO_yr <- clean_retreat_calcs$year_AO_tCE
  TB_yr <- clean_retreat_calcs$year_TB_tCE
  RE_yr <- clean_retreat_calcs$year_RE_tCE
  
  #AO
  AOtotalval_col <- paste0("Total_Value_AO_tCE_lfull_bv1")
  AOdemo_col <- paste0("demolition_AO_tCE")
  AOosds_col <- paste0("osdsremoval_AO_tCE")
  AOwastewater_col <- paste0("wastewaterremoval_AO_tCE")
  AOseawall_col <- paste0("seawall_AO_CE")
  AOtaxrevloss_col <- paste0("Total_TaxRev_Loss_AO_tCE_lfull_bv1")
  AOprivproploss_col <- paste0("Priv_Prop_Loss_AO_tCE_lfull_bv1")
  
  AO_public <- sum(Retreat_Analysis_Total[[AOtotalval_col]],Retreat_Analysis_Total[[AOdemo_col]],
                   Retreat_Analysis_Total[[AOosds_col]],Retreat_Analysis_Total[[AOwastewater_col]],
                   Retreat_Analysis_Total[[AOseawall_col]],na.rm=T)
  AO_taxrevloss <- Retreat_Analysis_Total[[AOtaxrevloss_col]]
  AO_propertyloss <- Retreat_Analysis_Total[[AOprivproploss_col]]
  AO_total <- sum(AO_public,AO_taxrevloss,AO_propertyloss,na.rm=T)
  
  #TB
  TBtotalval_col <- paste0("Total_Value_TB_tCE_lCE_bv1")
  TBdemo_col <- paste0("demolition_TB_tCE")
  TBosds_col <- paste0("osdsremoval_TB_tCE")
  TBwastewater_col <- paste0("wastewaterremoval_TB_tCE")
  TBseawall_col <- paste0("seawall_TB_CE")
  TBtaxrevloss_col <- paste0("Total_TaxRev_Loss_TB_tCE_lCE_bv1")
  TBprivproploss_col <- paste0("Priv_Prop_Loss_TB_tCE_lCE_bv1")
  
  TB_public <- sum(Retreat_Analysis_Total[[TBtotalval_col]],Retreat_Analysis_Total[[TBdemo_col]],
                   Retreat_Analysis_Total[[TBosds_col]],Retreat_Analysis_Total[[TBwastewater_col]],
                   Retreat_Analysis_Total[[TBseawall_col]],na.rm=T)
  TB_taxrevloss <- Retreat_Analysis_Total[[TBtaxrevloss_col]]
  TB_propertyloss <- Retreat_Analysis_Total[[TBprivproploss_col]]
  TB_total <- sum(TB_public,TB_taxrevloss,TB_propertyloss,na.rm=T)
  
  #RE
  REtotalval_col <- paste0("Total_Value_RE_tCE_lCE_bv0")
  REseawall_col <- paste0("seawall_RE_CE")
  REtaxrevloss_col <- paste0("Total_TaxRev_Loss_RE_tCE_lCE_bv0")
  REprivproploss_col <- paste0("Priv_Prop_Loss_RE_tCE_lCE_bv0")
  REcleanup_col <- paste0("cleanuphi_RE_tCE")

  RE_public <- Retreat_Analysis_Total[[REtotalval_col]]
  RE_ambig <- sum(Retreat_Analysis_Total[[REcleanup_col]],Retreat_Analysis_Total[[REseawall_col]],na.rm=T)
  RE_taxrevloss <- Retreat_Analysis_Total[[REtaxrevloss_col]]
  RE_propertyloss <- Retreat_Analysis_Total[[REprivproploss_col]]
  RE_total <- sum(RE_public,RE_ambig,RE_taxrevloss,RE_propertyloss,na.rm=T)
    
  cost_TMK[nrow(cost_TMK) + 1,] = c(TMK = tmk, AO_yearretreat = AO_yr,TB_yearretreat = TB_yr,RE_yearretreat = RE_yr,
                                    AO_parcelpubliccost = AO_public,AO_ambiguouscost=0,AO_taxrevloss=AO_taxrevloss,AO_propertyvalloss=AO_propertyloss,AO_totalcost=AO_total,
                                    TB_parcelpubliccost=TB_public,TB_ambiguouscost=0,TB_taxrevloss=TB_taxrevloss,TB_propertyvalloss=TB_propertyloss,TB_totalcost=TB_total,
                                    RE_parcelpubliccost=RE_public,RE_ambiguouscost=RE_ambig,RE_taxrevloss=RE_taxrevloss,RE_propertyvalloss=RE_propertyloss,RE_totalcost=RE_total)
}



#set up dataframe to store costs per TMK
cost_infra <- setNames(data.frame(matrix(ncol = 34, nrow = 0)), c('ID','AO_totallength','TB_totallength','RE_totallength',
                                                                  'AO_bridgerelocate','AO_bridgeretrofit','AO_hwyrelocate','AO_rdremove','AO_emdom',
                                                                  'AO_hwyriprap','AO_waterrelocate','AO_riprapremove','AO_maintain','AO_total',
                                                                  'TB_bridgerelocate','TB_bridgeretrofit','TB_hwyrelocate','TB_rdremove','TB_emdom',
                                                                  'TB_hwyriprap','TB_waterrelocate','TB_riprapremove','TB_maintain','TB_total',
                                                                  'RE_bridgerelocate','RE_bridgeretrofit','RE_hwyrelocate','RE_rdremove','RE_emdom',
                                                                  'RE_hwyriprap','RE_waterrelocate','RE_riprapremove','RE_maintain','RE_total'))

#calculate total costs per road
kapaard <- unique(kapaainfra$ID)
for(rd in kapaard){
  #select specific road
  infra_retreat <- kapaainfra[kapaainfra$ID == rd,]
  #use dummy values for TMK
  clean_retreat_calcs <- kapaa[kapaa$TMK == kapaatmk[1],]
  clean_assessors_bldg <- allbldg
  
  #run calculations
  source("C:/Users/rsett/Documents/KauaiRetreat/2retreatyearvaluetaxk.R")
  source("C:/Users/rsett/Documents/KauaiRetreat/3costsovertimek.R")
  source("C:/Users/rsett/Documents/KauaiRetreat/3infrastructurek.R") 
  source("C:/Users/rsett/Documents/KauaiRetreat/4discountedtotalcostsk.R")
  
  #AO
  AO_length <- sum(Retreat_Analysis_Total[["hwylengthAO_CE_rdr1"]],Retreat_Analysis_Total[["hwyripraplenAO_CE_rdr1"]],Retreat_Analysis_Total[["rdremovelenAO_CE_rdr1"]],na.rm=T)
  AO_brelocate <- Retreat_Analysis_Total[["bridgerelocateAO_CE_rdr1"]]
  AO_bretrofit <- Retreat_Analysis_Total[["bridgeretrofitAO_CE_rdr1"]]
  AO_hwyrelocate <- Retreat_Analysis_Total[["hwyrelocateAO_CE_rdr1"]]
  AO_rdremove <- Retreat_Analysis_Total[["rdremoveAO_CE_rdr1"]]
  AO_emdom <- Retreat_Analysis_Total[["emdomAO_CE_rdr1"]]
  AO_hwyriprap <- Retreat_Analysis_Total[["hwyriprapAO_CE_rdr1"]]
  AO_waterrelocate <- Retreat_Analysis_Total[["waterrelocateAO_CE_rdr1"]]
  AO_riprapremove <- Retreat_Analysis_Total[["riprapremoveAO_CE_rdr1"]]
  AO_maintain <- Retreat_Analysis_Total[["maintainAO_CE_rdr1"]]
  AO_total <- Retreat_Analysis_Total[["infrastructure_AO_CE_rdr1"]]
  
  #TB
  TB_length <- sum(Retreat_Analysis_Total[["hwylengthTB_CE_rdr1"]],Retreat_Analysis_Total[["hwyripraplenTB_CE_rdr1"]],Retreat_Analysis_Total[["rdremovelenTB_CE_rdr1"]],na.rm=T)
  TB_brelocate <- Retreat_Analysis_Total[["bridgerelocateTB_CE_rdr1"]]
  TB_bretrofit <- Retreat_Analysis_Total[["bridgeretrofitTB_CE_rdr1"]]
  TB_hwyrelocate <- Retreat_Analysis_Total[["hwyrelocateTB_CE_rdr1"]]
  TB_rdremove <- Retreat_Analysis_Total[["rdremoveTB_CE_rdr1"]]
  TB_emdom <- Retreat_Analysis_Total[["emdomTB_CE_rdr1"]]
  TB_hwyriprap <- Retreat_Analysis_Total[["hwyriprapTB_CE_rdr1"]]
  TB_waterrelocate <- Retreat_Analysis_Total[["waterrelocateTB_CE_rdr1"]]
  TB_riprapremove <- Retreat_Analysis_Total[["riprapremoveTB_CE_rdr1"]]
  TB_maintain <- Retreat_Analysis_Total[["maintainTB_CE_rdr1"]]
  TB_total <- Retreat_Analysis_Total[["infrastructure_TB_CE_rdr1"]]
  
  #RE
  RE_length <- sum(Retreat_Analysis_Total[["hwylengthRE_CE_rdr1"]],Retreat_Analysis_Total[["hwyripraplenRE_CE_rdr1"]],Retreat_Analysis_Total[["rdremovelenRE_CE_rdr1"]],na.rm=T)
  RE_brelocate <- Retreat_Analysis_Total[["bridgerelocateRE_CE_rdr1"]]
  RE_bretrofit <- Retreat_Analysis_Total[["bridgeretrofitRE_CE_rdr1"]]
  RE_hwyrelocate <- Retreat_Analysis_Total[["hwyrelocateRE_CE_rdr1"]]
  RE_rdremove <- Retreat_Analysis_Total[["rdremoveRE_CE_rdr1"]]
  RE_emdom <- Retreat_Analysis_Total[["emdomRE_CE_rdr1"]]
  RE_hwyriprap <- Retreat_Analysis_Total[["hwyriprapRE_CE_rdr1"]]
  RE_waterrelocate <- Retreat_Analysis_Total[["waterrelocateRE_CE_rdr1"]]
  RE_riprapremove <- Retreat_Analysis_Total[["riprapremoveRE_CE_rdr1"]]
  RE_maintain <- Retreat_Analysis_Total[["maintainRE_CE_rdr1"]]
  RE_total <- Retreat_Analysis_Total[["infrastructure_RE_CE_rdr1"]]
  

  
  cost_infra[nrow(cost_infra) + 1,] = c(ID = rd, AO_totallength=AO_length,TB_totallength=TB_length,RE_totallength=RE_length,
                                        AO_bridgerelocate=AO_brelocate,AO_bridgeretrofit=AO_bretrofit,AO_hwyrelocate=AO_hwyrelocate,AO_rdremove=AO_rdremove,AO_emdom=AO_emdom,
                                        AO_hwyriprap=AO_hwyriprap,AO_waterrelocate=AO_waterrelocate,AO_riprapremove=AO_riprapremove,AO_maintain=AO_maintain,AO_total=AO_total,
                                        TB_bridgerelocate=TB_brelocate,TB_bridgeretrofit=TB_bretrofit,TB_hwyrelocate=TB_hwyrelocate,TB_rdremove=TB_rdremove,TB_emdom=TB_emdom,
                                        TB_hwyriprap=TB_hwyriprap,TB_waterrelocate=TB_waterrelocate,TB_riprapremove=TB_riprapremove,TB_maintain=TB_maintain,TB_total=TB_total,
                                        RE_bridgerelocate=RE_brelocate,RE_bridgeretrofit=RE_bretrofit,RE_hwyrelocate=RE_hwyrelocate,RE_rdremove=RE_rdremove,RE_emdom=RE_emdom,
                                        RE_hwyriprap=RE_hwyriprap,RE_waterrelocate=RE_waterrelocate,RE_riprapremove=RE_riprapremove,RE_maintain=RE_maintain,RE_total=RE_total)
  
}

#save dataframes
write.csv(cost_TMK, paste0("costTMK_kapaa.csv"),row.names=F)
write.csv(cost_infra, paste0("costinfra_kapaa.csv"),row.names=F)

