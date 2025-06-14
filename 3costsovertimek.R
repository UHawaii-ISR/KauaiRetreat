#naming standard for sub-scenarios: RE_s_tXA_lCE_bv1_chi (approach = AO, trigger = XA, land transfer = CE, building value = 1,cleanup cost = high, seawall = stay)

library(dplyr)
library(ggplot2)
library(ggpubr)
library(data.table)
setwd(workdir)


# Disable scientific notation
options(scipen = 999)


# summarize costs over time
years <- c(2025, 2028, 2030, 2040, 2050, 2062, 2075, 2087, 2100)
Retreat_Analysis <- data.frame(
  Years = years)

#seawall scenarios: "_s_" = seawall stays and parcels&buildings directly/indirectly behind are protected. "_" = seawall is removed and retreat occurs
seawalls <- c("_","_s_")

#use TMK8 to estimate area retreated so we don't double-count CPR-unit area
clean_retreat_calcs_area <- clean_retreat_calcs[!duplicated(clean_retreat_calcs$COTMK), ]

#isolate just residential homes
residential <- c("Owner-Occupied", "Vacation Rental","Non-Owner-Occupied Residential","Owner-Occupied Mixed Use")  
hotel <- c("Hotel and Resort")
commercial <- c("Commercial","Industrial")
agriculture <- c("Agricultural")
conservation <- c("Conservation")

clean_retreat_calcs_home_cpr <- clean_retreat_calcs %>%
  filter(TAXCLASS %in% residential)

clean_retreat_calcs_home <- clean_retreat_calcs %>%
  filter(TMK8_TAXCLASS %in% residential)
clean_retreat_calcs_hotel <- clean_retreat_calcs %>%
  filter(TMK8_TAXCLASS %in% hotel)
clean_retreat_calcs_comm <- clean_retreat_calcs %>%
  filter(TMK8_TAXCLASS %in% commercial)
clean_retreat_calcs_ag <- clean_retreat_calcs %>%
  filter(TMK8_TAXCLASS %in% agriculture)
clean_retreat_calcs_cons <- clean_retreat_calcs %>%
  filter(TMK8_TAXCLASS %in% conservation)

#isolate just apartment buildings
clean_retreat_calcs_apt <- clean_retreat_calcs[clean_retreat_calcs$apartment == 1, ]
clean_retreat_calcs_apt <- clean_retreat_calcs_apt[!duplicated(clean_retreat_calcs_apt$BuildingID), ]
clean_retreat_calcs_apt_res <- clean_retreat_calcs_apt %>%
  filter(TMK8_TAXCLASS %in% residential)

#buildings by tax class
clean_assessors_bldg_res <- clean_assessors_bldg %>%
  filter(TMK8_TAXCLASS %in% residential)
clean_assessors_bldg_hotel <- clean_assessors_bldg %>%
  filter(TMK8_TAXCLASS %in% hotel)
clean_assessors_bldg_comm <- clean_assessors_bldg %>%
  filter(TMK8_TAXCLASS %in% commercial)
clean_assessors_bldg_ag <- clean_assessors_bldg %>%
  filter(TMK8_TAXCLASS %in% agriculture)
clean_assessors_bldg_cons <- clean_assessors_bldg %>%
  filter(TMK8_TAXCLASS %in% conservation)

#calculate values for each year
for (year in years) {
  for(seawall in seawalls){
    for(trigger in triggers){
      yearAO_col <- paste0("year_AO",seawall,"t",trigger)
      yearTB_col <- paste0("year_TB",seawall,"t",trigger)
      yearRE_col <- paste0("year_RE",seawall,"t",trigger)
      
      retreating_parcel <- !is.na(clean_retreat_calcs[[yearTB_col]])
      AO_matching_rows <- clean_retreat_calcs[[yearAO_col]] == year
      TB_matching_rows <- clean_retreat_calcs[[yearTB_col]] == year
      RE_matching_rows <- clean_retreat_calcs[[yearRE_col]] == year
      
      AO_matching_rows_area <- clean_retreat_calcs_area[[yearAO_col]] == year
      TB_matching_rows_area <- clean_retreat_calcs_area[[yearTB_col]] == year
      RE_matching_rows_area <- clean_retreat_calcs_area[[yearRE_col]] == year
      
      AO_matching_rows_apt <- clean_retreat_calcs_apt[[yearAO_col]] == year
      TB_matching_rows_apt <- clean_retreat_calcs_apt[[yearTB_col]] == year
      RE_matching_rows_apt <- clean_retreat_calcs_apt[[yearRE_col]] == year
      
      AO_matching_rows_apt_res <- clean_retreat_calcs_apt_res[[yearAO_col]] == year
      TB_matching_rows_apt_res <- clean_retreat_calcs_apt_res[[yearTB_col]] == year
      RE_matching_rows_apt_res <- clean_retreat_calcs_apt_res[[yearRE_col]] == year
      
      AO_matching_rows_bldg <- clean_assessors_bldg[[yearAO_col]] == year
      TB_matching_rows_bldg <- clean_assessors_bldg[[yearTB_col]] == year
      RE_matching_rows_bldg <- clean_assessors_bldg[[yearRE_col]] == year
      
      AO_matching_rows_bldg_res <- clean_assessors_bldg_res[[yearAO_col]] == year
      TB_matching_rows_bldg_res <- clean_assessors_bldg_res[[yearTB_col]] == year
      RE_matching_rows_bldg_res <- clean_assessors_bldg_res[[yearRE_col]] == year
      
      AO_matching_rows_bldg_hotel <- clean_assessors_bldg_hotel[[yearAO_col]] == year
      TB_matching_rows_bldg_hotel <- clean_assessors_bldg_hotel[[yearTB_col]] == year
      RE_matching_rows_bldg_hotel <- clean_assessors_bldg_hotel[[yearRE_col]] == year
      
      AO_matching_rows_bldg_ag <- clean_assessors_bldg_ag[[yearAO_col]] == year
      TB_matching_rows_bldg_ag <- clean_assessors_bldg_ag[[yearTB_col]] == year
      RE_matching_rows_bldg_ag <- clean_assessors_bldg_ag[[yearRE_col]] == year
      
      AO_matching_rows_bldg_comm <- clean_assessors_bldg_comm[[yearAO_col]] == year
      TB_matching_rows_bldg_comm <- clean_assessors_bldg_comm[[yearTB_col]] == year
      RE_matching_rows_bldg_comm <- clean_assessors_bldg_comm[[yearRE_col]] == year
      
      AO_matching_rows_bldg_cons <- clean_assessors_bldg_cons[[yearAO_col]] == year
      TB_matching_rows_bldg_cons <- clean_assessors_bldg_cons[[yearTB_col]] == year
      RE_matching_rows_bldg_cons <- clean_assessors_bldg_cons[[yearRE_col]] == year
      
      AO_matching_rows_home_cpr <- clean_retreat_calcs_home_cpr[[yearAO_col]] == year
      TB_matching_rows_home_cpr <- clean_retreat_calcs_home_cpr[[yearTB_col]] == year
      RE_matching_rows_home_cpr <- clean_retreat_calcs_home_cpr[[yearRE_col]] == year
      
      AO_matching_rows_home <- clean_retreat_calcs_home[[yearAO_col]] == year
      TB_matching_rows_home <- clean_retreat_calcs_home[[yearTB_col]] == year
      RE_matching_rows_home <- clean_retreat_calcs_home[[yearRE_col]] == year
      
      AO_matching_rows_hotel <- clean_retreat_calcs_hotel[[yearAO_col]] == year
      TB_matching_rows_hotel <- clean_retreat_calcs_hotel[[yearTB_col]] == year
      RE_matching_rows_hotel <- clean_retreat_calcs_hotel[[yearRE_col]] == year
      
      AO_matching_rows_comm <- clean_retreat_calcs_comm[[yearAO_col]] == year
      TB_matching_rows_comm <- clean_retreat_calcs_comm[[yearTB_col]] == year
      RE_matching_rows_comm <- clean_retreat_calcs_comm[[yearRE_col]] == year
      
      AO_matching_rows_ag <- clean_retreat_calcs_ag[[yearAO_col]] == year
      TB_matching_rows_ag <- clean_retreat_calcs_ag[[yearTB_col]] == year
      RE_matching_rows_ag <- clean_retreat_calcs_ag[[yearRE_col]] == year
      
      AO_matching_rows_cons <- clean_retreat_calcs_cons[[yearAO_col]] == year
      TB_matching_rows_cons <- clean_retreat_calcs_cons[[yearTB_col]] == year
      RE_matching_rows_cons <- clean_retreat_calcs_cons[[yearRE_col]] == year
      
      ### NUM OF BUILDINGS
      
      #Count the number of AO buildings (in 2025)
      buildings_col <- paste0("buildings_AO",seawall,"t",trigger)
      Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] <- 
        ifelse(year==2025,n_distinct(clean_assessors_bldg$BuildingID[AO_matching_rows_bldg], na.rm = TRUE),0)
      
      # Count the number of TB buildings in each year
      buildings_col <- paste0("buildings_TB",seawall,"t",trigger)
      Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_assessors_bldg$BuildingID[TB_matching_rows_bldg], na.rm = TRUE)
      
      # Count the number of RE buildings in each year
      buildings_col <- paste0("buildings_RE",seawall,"t",trigger)
      Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_assessors_bldg$BuildingID[RE_matching_rows_bldg], na.rm = TRUE)
      
      #Building Count per TAXCLASS
      
      #res
      buildings_col <- paste0("buildings_res_AO",seawall,"t",trigger)
      Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] <- 
        ifelse(year==2025,n_distinct(clean_assessors_bldg_res$BuildingID[AO_matching_rows_bldg_res], na.rm = TRUE),0)
      buildings_col <- paste0("buildings_res_TB",seawall,"t",trigger)
      Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_assessors_bldg_res$BuildingID[TB_matching_rows_bldg_res], na.rm = TRUE)
      buildings_col <- paste0("buildings_res_RE",seawall,"t",trigger)
      Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_assessors_bldg_res$BuildingID[RE_matching_rows_bldg_res], na.rm = TRUE)
      
      #hotel
      buildings_col <- paste0("buildings_hotel_AO",seawall,"t",trigger)
      Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] <- 
        ifelse(year==2025,n_distinct(clean_assessors_bldg_hotel$BuildingID[AO_matching_rows_bldg_hotel], na.rm = TRUE),0)
      buildings_col <- paste0("buildings_hotel_TB",seawall,"t",trigger)
      Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_assessors_bldg_hotel$BuildingID[TB_matching_rows_bldg_hotel], na.rm = TRUE)
      buildings_col <- paste0("buildings_hotel_RE",seawall,"t",trigger)
      Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_assessors_bldg_hotel$BuildingID[RE_matching_rows_bldg_hotel], na.rm = TRUE)
      
      #comm
      buildings_col <- paste0("buildings_comm_AO",seawall,"t",trigger)
      Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] <- 
        ifelse(year==2025,n_distinct(clean_assessors_bldg_comm$BuildingID[AO_matching_rows_bldg_comm], na.rm = TRUE),0)
      buildings_col <- paste0("buildings_comm_TB",seawall,"t",trigger)
      Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_assessors_bldg_comm$BuildingID[TB_matching_rows_bldg_comm], na.rm = TRUE)
      buildings_col <- paste0("buildings_comm_RE",seawall,"t",trigger)
      Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_assessors_bldg_comm$BuildingID[RE_matching_rows_bldg_comm], na.rm = TRUE)
      
      #cons
      buildings_col <- paste0("buildings_cons_AO",seawall,"t",trigger)
      Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] <- 
        ifelse(year==2025,n_distinct(clean_assessors_bldg_cons$BuildingID[AO_matching_rows_bldg_cons], na.rm = TRUE),0)
      buildings_col <- paste0("buildings_cons_TB",seawall,"t",trigger)
      Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_assessors_bldg_cons$BuildingID[TB_matching_rows_bldg_cons], na.rm = TRUE)
      buildings_col <- paste0("buildings_cons_RE",seawall,"t",trigger)
      Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_assessors_bldg_cons$BuildingID[RE_matching_rows_bldg_cons], na.rm = TRUE)
      
      #ag
      buildings_col <- paste0("buildings_ag_AO",seawall,"t",trigger)
      Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] <- 
        ifelse(year==2025,n_distinct(clean_assessors_bldg_ag$BuildingID[AO_matching_rows_bldg_ag], na.rm = TRUE),0)
      buildings_col <- paste0("buildings_ag_TB",seawall,"t",trigger)
      Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_assessors_bldg_ag$BuildingID[TB_matching_rows_bldg_ag], na.rm = TRUE)
      buildings_col <- paste0("buildings_ag_RE",seawall,"t",trigger)
      Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_assessors_bldg_ag$BuildingID[RE_matching_rows_bldg_ag], na.rm = TRUE)
      
      ### NUM OF APARTMENT BLDGs
      
      #Count the number of AO apartments (in 2025)
      apartments_col <- paste0("apartments_AO",seawall,"t",trigger)
      apt_rows <- clean_retreat_calcs[["apartment"]] ==1
      Retreat_Analysis[[apartments_col]][Retreat_Analysis$Years == year] <- 
        ifelse(year==2025,n_distinct(clean_retreat_calcs_apt_res$BuildingID[AO_matching_rows_apt], na.rm = TRUE),0)
      
      # Count the number of TB apartments in each year
      apartments_col <- paste0("apartments_TB",seawall,"t",trigger)
      Retreat_Analysis[[apartments_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_retreat_calcs_apt_res$BuildingID[TB_matching_rows_apt], na.rm = TRUE)
      
      # Count the number of RE apartments in each year
      apartments_col <- paste0("apartments_RE",seawall,"t",trigger)
      Retreat_Analysis[[apartments_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_retreat_calcs_apt_res$BuildingID[RE_matching_rows_apt], na.rm = TRUE)
      
      ### NUM OF CPR UNITS
      
      CPRunits_col <- paste0("CPRunits_AO",seawall,"t",trigger)
      Retreat_Analysis[[CPRunits_col]][Retreat_Analysis$Years == year] <- 
        ifelse(year==2025,sum(clean_retreat_calcs_apt$Number_CPRbldg[AO_matching_rows_apt], na.rm = TRUE),0)
      
      CPRunits_col <- paste0("CPRunits_TB",seawall,"t",trigger)
      Retreat_Analysis[[CPRunits_col]][Retreat_Analysis$Years == year] <- 
        sum(clean_retreat_calcs_apt$Number_CPRbldg[TB_matching_rows_apt], na.rm = TRUE)

      CPRunits_col <- paste0("CPRunits_RE",seawall,"t",trigger)
      Retreat_Analysis[[CPRunits_col]][Retreat_Analysis$Years == year] <- 
        sum(clean_retreat_calcs_apt$Number_CPRbldg[RE_matching_rows_apt], na.rm = TRUE)
      
      ### NUM OF SEAWALLS
      
      #Count the number of AO seawalls (in 2025)
      seawalls_col <- paste0("seawallnum_AO",seawall,"t",trigger)
      Retreat_Analysis[[seawalls_col]][Retreat_Analysis$Years == year] <- 
        ifelse(seawall=="_s_",0,ifelse(year==2025,n_distinct(clean_retreat_calcs$SEAWALLID[AO_matching_rows], na.rm = TRUE),0))
      
      # Count the number of TB seawalls in each year
      seawalls_col <- paste0("seawallnum_TB",seawall,"t",trigger)
      Retreat_Analysis[[seawalls_col]][Retreat_Analysis$Years == year] <- 
        ifelse(seawall=="_s_",0,n_distinct(clean_retreat_calcs$SEAWALLID[TB_matching_rows], na.rm = TRUE))
      
      # Count the number of RE seawalls in each year
      seawalls_col <- paste0("seawallnum_RE",seawall,"t",trigger)
      Retreat_Analysis[[seawalls_col]][Retreat_Analysis$Years == year] <- 
        ifelse(seawall=="_s_",0,n_distinct(clean_retreat_calcs$SEAWALLID[RE_matching_rows], na.rm = TRUE))
      
      ### LENGTH OF SEAWALLS
      
      #Sum the length of AO seawalls that need to demo (in 2025)
      seawalls_col <- paste0("seawalllength_AO",seawall,"t",trigger)
      Retreat_Analysis[[seawalls_col]][Retreat_Analysis$Years == year] <- 
        ifelse(seawall=="_s_",0,ifelse(year==2025,sum(clean_retreat_calcs$SEAWALL_LEN_PERTMK[AO_matching_rows], na.rm = TRUE),0))
      
      # Sum the length of TB seawalls that need to demo in each year
      seawalls_col <- paste0("seawalllength_TB",seawall,"t",trigger)
      Retreat_Analysis[[seawalls_col]][Retreat_Analysis$Years == year] <- 
        ifelse(seawall=="_s_",0,sum(clean_retreat_calcs$SEAWALL_LEN_PERTMK[TB_matching_rows], na.rm = TRUE))
      
      # Sum the length of RE seawalls that need to demo in each year
      seawalls_col <- paste0("seawalllength_RE",seawall,"t",trigger)
      Retreat_Analysis[[seawalls_col]][Retreat_Analysis$Years == year] <- 
        ifelse(seawall=="_s_",0,sum(clean_retreat_calcs$SEAWALL_LEN_PERTMK[RE_matching_rows], na.rm = TRUE))
      
      ### NUM OF PARCELS (based on TMK8 - COTMK column)
      
      #sum the AO parcels (in 2025)
      parcels_col <- paste0("parcels8_AO",seawall,"t",trigger)
      Retreat_Analysis[[parcels_col]][Retreat_Analysis$Years == year] <- 
        ifelse(year==2025,n_distinct(clean_retreat_calcs$COTMK[AO_matching_rows==T],na.rm=T),0)
      
      # Count the number of TB parcels in each year
      parcels_col <- paste0("parcels8_TB",seawall,"t",trigger)
      Retreat_Analysis[[parcels_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_retreat_calcs$COTMK[TB_matching_rows==T], na.rm = TRUE)
      
      # Count the number of RE parcels in each year
      parcels_col <- paste0("parcels8_RE",seawall,"t",trigger)
      Retreat_Analysis[[parcels_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_retreat_calcs$COTMK[RE_matching_rows==T], na.rm = TRUE)
      
      
      ### NUM OF PARCELS / CPR (based on TMK12)
      
      #sum the AO parcels (in 2025)
      parcels_col <- paste0("parcelcpr_AO",seawall,"t",trigger)
      Retreat_Analysis[[parcels_col]][Retreat_Analysis$Years == year] <- 
        ifelse(year==2025,n_distinct(clean_retreat_calcs$TMK[AO_matching_rows==T],na.rm=T),0)
      
      # Count the number of TB parcels in each year
      parcels_col <- paste0("parcelcpr_TB",seawall,"t",trigger)
      Retreat_Analysis[[parcels_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_retreat_calcs$TMK[TB_matching_rows==T], na.rm = TRUE)
      
      # Count the number of RE parcels in each year
      parcels_col <- paste0("parcelcpr_RE",seawall,"t",trigger)
      Retreat_Analysis[[parcels_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_retreat_calcs$TMK[RE_matching_rows==T], na.rm = TRUE)
      
      
      ### NUM OF RESIDENTIAL PARCELS / CPR (based on TMK12)
      
      #sum the AO parcels (in 2025)
      parcels_col <- paste0("homes_AO",seawall,"t",trigger)
      Retreat_Analysis[[parcels_col]][Retreat_Analysis$Years == year] <- 
        ifelse(year==2025,n_distinct(clean_retreat_calcs_home$TMK[AO_matching_rows_home==T],na.rm=T),0)
      
      # Count the number of TB parcels in each year
      parcels_col <- paste0("homes_TB",seawall,"t",trigger)
      Retreat_Analysis[[parcels_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_retreat_calcs_home$TMK[TB_matching_rows_home==T], na.rm = TRUE)
      
      # Count the number of RE parcels in each year
      parcels_col <- paste0("homes_RE",seawall,"t",trigger)
      Retreat_Analysis[[parcels_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_retreat_calcs_home$TMK[RE_matching_rows_home==T], na.rm = TRUE)
      
      #sum the AO parcels (in 2025)
      parcels_col <- paste0("homes_cpr_AO",seawall,"t",trigger)
      Retreat_Analysis[[parcels_col]][Retreat_Analysis$Years == year] <- 
        ifelse(year==2025,n_distinct(clean_retreat_calcs_home_cpr$TMK[AO_matching_rows_home_cpr==T],na.rm=T),0)
      
      # Count the number of TB parcels in each year
      parcels_col <- paste0("homes_cpr_TB",seawall,"t",trigger)
      Retreat_Analysis[[parcels_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_retreat_calcs_home_cpr$TMK[TB_matching_rows_home_cpr==T], na.rm = TRUE)
      
      # Count the number of RE parcels in each year
      parcels_col <- paste0("homes_cpr_RE",seawall,"t",trigger)
      Retreat_Analysis[[parcels_col]][Retreat_Analysis$Years == year] <- 
        n_distinct(clean_retreat_calcs_home_cpr$TMK[RE_matching_rows_home_cpr==T], na.rm = TRUE)
      
      
      ### DEMOLITION & CLEANUP COSTS
      
      # Demolition Costs ($2025) for AO and TB
      demolition_house <- 15450   # Demolition cost per residential house, ag building, conservation building
      demolition_apartment <- 8640  # residential apartment per unit demolition cost (per unit assuming unit is 800sqft and 9ft tall, an apartment (CPR) is 7200 cubic ft.)
      demolition_aptfoundation <- 19 #residential apartment building foundation demolition cost (per sqft foundation)
      demolition_hotelcomm <- 79  # hotel or commercial building cost (per sqft)
      demolition_seawall <- 13922 # seawall demolition $4000/ft ($13123/m) for inaccessible seawall e.g. residential area)
      
      #cost of seawall demolition for AO
      seawall_col <- paste0("seawall_AO",seawall,trigger)
      seawalllen_col <- paste0("seawalllength_AO",seawall,"t",trigger)
      seawallm <- Retreat_Analysis[[seawalllen_col]][Retreat_Analysis$Years == year]
      Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] <- 
        ifelse(year==2025,seawallm*demolition_seawall,0)
      
      #cost of seawall demolition for TB
      seawall_col <- paste0("seawall_TB",seawall,trigger)
      seawalllen_col <- paste0("seawalllength_TB",seawall,"t",trigger)
      seawallm <- Retreat_Analysis[[seawalllen_col]][Retreat_Analysis$Years == year]
      Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] <- 
        seawallm*demolition_seawall
      
      #cost of seawall demolition for RE
      seawall_col <- paste0("seawall_RE",seawall,trigger)
      seawalllen_col <- paste0("seawalllength_RE",seawall,"t",trigger)
      seawallm <- Retreat_Analysis[[seawalllen_col]][Retreat_Analysis$Years == year]
      Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] <- 
        seawallm*demolition_seawall

      # sum demolition costs for AO
      demo_col <- paste0("demolition_AO",seawall,"t",trigger)
      buildings_col <- paste0("buildings_AO",seawall,"t",trigger)
      apartments_col <- paste0("apartments_AO",seawall,"t",trigger)
      aptcount <- Retreat_Analysis[[apartments_col]][Retreat_Analysis$Years == year]
      housecount <- Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] -  aptcount #num houses = #buildings - #apartments
      apartmentdemo <- sum(clean_retreat_calcs_apt[["Number_CPRbldg"]][AO_matching_rows_apt], na.rm = TRUE)*demolition_apartment + 
        sum(clean_retreat_calcs_apt[["BLDG_SQFT"]][AO_matching_rows_apt], na.rm = TRUE)*demolition_aptfoundation
      Retreat_Analysis[[demo_col]][Retreat_Analysis$Years == year] <- 
        ifelse(year==2025,apartmentdemo + housecount*demolition_house,0) 
      
      # Sum demolition costs for TB 
      demo_col <- paste0("demolition_TB",seawall,"t",trigger)
      bldg_res_col <- paste0("buildings_res_TB",seawall,"t",trigger)
      apartments_col <- paste0("apartments_TB",seawall,"t",trigger)
      aptcount <- Retreat_Analysis[[apartments_col]][Retreat_Analysis$Years == year]
      housecount <- Retreat_Analysis[[bldg_res_col]][Retreat_Analysis$Years == year] - aptcount + 
        n_distinct(clean_assessors_bldg_cons$BuildingID[TB_matching_rows_bldg_cons],na.rm=T) + 
                     n_distinct(clean_assessors_bldg_ag$BuildingID[TB_matching_rows_bldg_ag],na.rm=T)
      apartmentdemo <- sum(clean_retreat_calcs_apt_res[["Number_CPRbldg"]][TB_matching_rows_apt_res], na.rm = TRUE)*demolition_apartment + 
        sum(clean_retreat_calcs_apt_res[["BLDG_SQFT"]][TB_matching_rows_apt_res], na.rm = TRUE)*demolition_aptfoundation
      hotelcommdemo <- sum(clean_assessors_bldg_hotel[["BLDG_SQFT"]][TB_matching_rows_bldg_hotel][
        !duplicated(clean_assessors_bldg_hotel[["BuildingID"]][TB_matching_rows_bldg_hotel])], na.rm = TRUE)*demolition_hotelcomm + 
        sum(clean_assessors_bldg_comm[["BLDG_SQFT"]][TB_matching_rows_bldg_comm][
          !duplicated(clean_assessors_bldg_comm[["BuildingID"]][TB_matching_rows_bldg_comm])], na.rm = TRUE)*demolition_hotelcomm
      Retreat_Analysis[[demo_col]][Retreat_Analysis$Years == year] <- 
        apartmentdemo + housecount*demolition_house + hotelcommdemo
      
      # Sum clean-up costs for parcels that are RE for each year

      cleanlo_house <- 10000 #Low-end ($10,000 per building)
      cleanhi_house <- 200000 # High-end ($200,000 per building)
      clean_apartment <- 250000 #clean-up costs for apartment building **** this is a dummy value
      
      cleanuplo_col <- paste0("cleanuplo_RE",seawall,"t",trigger)
      cleanuphi_col <- paste0("cleanuphi_RE",seawall,"t",trigger)
      buildings_col <- paste0("buildings_RE",seawall,"t",trigger)
      apartments_col <- paste0("apartments_RE",seawall,"t",trigger)
      aptcount <- Retreat_Analysis[[apartments_col]][Retreat_Analysis$Years == year]
      housecount <- Retreat_Analysis[[buildings_col]][Retreat_Analysis$Years == year] - aptcount
      apartmentdemo <- sum(clean_retreat_calcs_apt[["Number_CPRbldg"]][RE_matching_rows_apt], na.rm = TRUE)*demolition_apartment + 
        sum(clean_retreat_calcs_apt[["BLDG_SQFT"]][RE_matching_rows_apt], na.rm = TRUE)*demolition_aptfoundation
      Retreat_Analysis[[cleanuplo_col]][Retreat_Analysis$Years == year] <- 
        apartmentdemo + housecount * cleanlo_house 
      Retreat_Analysis[[cleanuphi_col]][Retreat_Analysis$Years == year] <- 
        apartmentdemo + housecount * cleanhi_house 
      
      ### AREA RETREATED
      
      # Total area retreated in AO (sq.m.)
      arearetreat_col <- paste0("arearetreat_AO",seawall,"t",trigger)
      Retreat_Analysis[[arearetreat_col]][Retreat_Analysis$Years == year] <- 
        ifelse(year==2025,sum(clean_retreat_calcs[["area_og"]][AO_matching_rows], na.rm = TRUE),0) 
        #ifelse(year==2025,sum(clean_retreat_calcs_area[["OG_PARCEL_AREA"]][AO_matching_rows_area], na.rm = TRUE),0) 
      
      # Total area retreated in TB (sq.m.)
      arearetreat_col <- paste0("arearetreat_TB",seawall,"t",trigger)
      Retreat_Analysis[[arearetreat_col]][Retreat_Analysis$Years == year] <- 
        sum(clean_retreat_calcs[["area_og"]][TB_matching_rows], na.rm = TRUE)
        #sum(clean_retreat_calcs_area[["OG_PARCEL_AREA"]][TB_matching_rows_area], na.rm = TRUE)
      
      # Total area retreated in RE (sq.m.)
      arearetreat_col <- paste0("arearetreat_RE",seawall,"t",trigger)
      Retreat_Analysis[[arearetreat_col]][Retreat_Analysis$Years == year] <- 
        sum(clean_retreat_calcs[["area_og"]][RE_matching_rows], na.rm = TRUE)
        #sum(clean_retreat_calcs_area[["OG_PARCEL_AREA"]][RE_matching_rows_area], na.rm = TRUE)
      
      ### OSDS & WASTEWATER REMOVAL
      
      #for non-osds homes, assume they need wastewater pipe removal
      wastewater_remove <- 6032 
      osds_remove <- 2397
      
      # OSDS & wastewater removal costs for AO ($)
      osds_col <- paste0("osdsremoval_AO",seawall,"t",trigger)
      wastewater_col <- paste0("wastewaterremoval_AO",seawall,"t",trigger)
      Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year] <-
        ifelse(year==2025,sum(clean_retreat_calcs$OSDS[AO_matching_rows], na.rm = TRUE) * osds_remove,0)
      Retreat_Analysis[[wastewater_col]][Retreat_Analysis$Years == year] <-
        ifelse(year==2025,sum(clean_retreat_calcs$WASTEWATER[AO_matching_rows], na.rm = TRUE) * wastewater_remove,0)
      
      # Sum OSDS & wastewater costs for TB ($)
      osds_col <- paste0("osdsremoval_TB",seawall,"t",trigger)
      wastewater_col <- paste0("wastewaterremoval_TB",seawall,"t",trigger)
      Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year] <-
        sum(clean_retreat_calcs[["OSDS"]][TB_matching_rows], na.rm = TRUE) * osds_remove
      Retreat_Analysis[[wastewater_col]][Retreat_Analysis$Years == year] <-
        sum(clean_retreat_calcs[["WASTEWATER"]][TB_matching_rows], na.rm = TRUE) * wastewater_remove
      
      # Sum OSDS & wastewater costs for RE ($)
      osds_col <- paste0("osdsremoval_RE",seawall,"t",trigger)
      wastewater_col <- paste0("wastewaterremoval_RE",seawall,"t",trigger)
      Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year] <-
        sum(clean_retreat_calcs[["OSDS"]][RE_matching_rows], na.rm = TRUE) * osds_remove
      Retreat_Analysis[[wastewater_col]][Retreat_Analysis$Years == year] <-
        sum(clean_retreat_calcs[["WASTEWATER"]][RE_matching_rows], na.rm = TRUE) * wastewater_remove
      
      for(hazard_type in hazard_types){
        
        ### AREA UNDER HAZARD
        
        # Total area affected under each hazard (sq.m.)
        areahazard_col <- paste0("areahazard_l",hazard_type)
        parcelhazard <- paste0("SA_", year, "_", hazard_type) 
        Retreat_Analysis[[areahazard_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_retreat_calcs[[parcelhazard]], na.rm = TRUE) #for total area of hazard
          #sum(clean_retreat_calcs[[parcelhazard]][retreating_parcel], na.rm = TRUE) #for fig4 displaying only parcels that are retreating
        
        ### AREA UNDER HAZARD ALL PARCELS (including non-residential)
        
        # Total area affected under each hazard (sq.m.)
        areahazardall_col <- paste0("areahazardall_l",hazard_type)
        parcelhazard <- paste0("SA_", year, "_", hazard_type) 
        Retreat_Analysis[[areahazardall_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_assessors_parcels[[parcelhazard]], na.rm = TRUE) #for total area of hazard
        
        ### TOTAL VALUE, PROPERTY LOSS, TAX REV LOSS
        
        #total building + land value of parcels that are AO (full building value under AO)
        totalval_col <- paste0("Total_Value_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
        Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year] <- 
          ifelse(year==2025,sum(clean_retreat_calcs$APRTOTMKT[AO_matching_rows],na.rm=T),0)
        
        #priv prop loss under AO is 0
        
        # Sum the total building + land value of parcels that are RE (building value is none under RE)
        totalval_col <- paste0("Total_Value_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
        column_name <- paste0("Total_Appraise_", year, "_t",trigger,"_l",hazard_type,"_bv0")
        Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_retreat_calcs[[column_name]][RE_matching_rows], na.rm = TRUE)
        
        # Sum private property loss of parcels that are RE
        privproploss_col <- paste0("Priv_Prop_Loss_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
        column_name <- paste0("Priv_Prop_Loss_", year, "_t",trigger,"_l",hazard_type,"_bv0")
        Retreat_Analysis[[privproploss_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_retreat_calcs[[column_name]][RE_matching_rows], na.rm = TRUE)
        
        # Sum tax revenue loss for parcels that are RE (no building value)
        taxrev_col <- paste0("Total_TaxRev_RE",seawall,"t", trigger, "_l", hazard_type, "_bv0")
        column_name <- paste0("Total_TaxRev_", year, "_t",trigger,"_l",hazard_type,"_bv0")
        Retreat_Analysis[[taxrev_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_retreat_calcs[[column_name]][RE_matching_rows], na.rm = TRUE)
        
        
        #BREAKDOWNS PER TAXCLASS
        
        #total value per taxclass
        #AO
        totalval_col <- paste0("Total_Value_res_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
        Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year] <- 
          ifelse(year==2025,sum(clean_retreat_calcs_home$APRTOTMKT[AO_matching_rows_home],na.rm=T),0)
        
        totalval_col <- paste0("Total_Value_hotel_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
        Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year] <- 
          ifelse(year==2025,sum(clean_retreat_calcs_hotel$APRTOTMKT[AO_matching_rows_hotel],na.rm=T),0)
        
        totalval_col <- paste0("Total_Value_ag_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
        Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year] <- 
          ifelse(year==2025,sum(clean_retreat_calcs_ag$APRTOTMKT[AO_matching_rows_ag],na.rm=T),0)
        
        totalval_col <- paste0("Total_Value_comm_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
        Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year] <- 
          ifelse(year==2025,sum(clean_retreat_calcs_comm$APRTOTMKT[AO_matching_rows_comm],na.rm=T),0)
        
        totalval_col <- paste0("Total_Value_cons_AO",seawall,"t",trigger,"_l",hazard_type,"_bv1")
        Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year] <- 
          ifelse(year==2025,sum(clean_retreat_calcs_cons$APRTOTMKT[AO_matching_rows_cons],na.rm=T),0)
        
        #RE
        totalval_col <- paste0("Total_Value_res_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
        column_name <- paste0("Total_Appraise_", year, "_t",trigger,"_l",hazard_type,"_bv0")
        Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_retreat_calcs_home[[column_name]][RE_matching_rows_home], na.rm = TRUE)
        
        totalval_col <- paste0("Total_Value_hotel_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
        column_name <- paste0("Total_Appraise_", year, "_t",trigger,"_l",hazard_type,"_bv0")
        Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_retreat_calcs_hotel[[column_name]][RE_matching_rows_hotel], na.rm = TRUE)
        
        totalval_col <- paste0("Total_Value_ag_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
        column_name <- paste0("Total_Appraise_", year, "_t",trigger,"_l",hazard_type,"_bv0")
        Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_retreat_calcs_ag[[column_name]][RE_matching_rows_ag], na.rm = TRUE)
        
        totalval_col <- paste0("Total_Value_comm_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
        column_name <- paste0("Total_Appraise_", year, "_t",trigger,"_l",hazard_type,"_bv0")
        Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_retreat_calcs_comm[[column_name]][RE_matching_rows_comm], na.rm = TRUE)
        
        totalval_col <- paste0("Total_Value_cons_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
        column_name <- paste0("Total_Appraise_", year, "_t",trigger,"_l",hazard_type,"_bv0")
        Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_retreat_calcs_cons[[column_name]][RE_matching_rows_cons], na.rm = TRUE)
        
        #demo cost per taxclass 
        #AO
        demo_col <- paste0("demolition_res_AO",seawall,"t",trigger)
        housecount <- n_distinct(clean_assessors_bldg_res$BuildingID[AO_matching_rows_bldg_res], na.rm = TRUE) - 
                               n_distinct(clean_retreat_calcs_apt_res$BuildingID[AO_matching_rows_apt_res], na.rm = TRUE)
        Retreat_Analysis[[demo_col]][Retreat_Analysis$Years == year] <- 
          ifelse(year==2025,sum(clean_retreat_calcs_apt_res[["Number_CPRbldg"]][AO_matching_rows_apt_res], na.rm = TRUE)*demolition_apartment + 
                   sum(clean_retreat_calcs_apt_res[["BLDG_SQFT"]][AO_matching_rows_apt_res], na.rm = TRUE)*demolition_aptfoundation+
                   housecount*demolition_house,0)

        demo_col <- paste0("demolition_hotel_AO",seawall,"t",trigger)
        Retreat_Analysis[[demo_col]][Retreat_Analysis$Years == year] <- 
          ifelse(year==2025,sum(clean_assessors_bldg_hotel[["BLDG_SQFT"]][AO_matching_rows_bldg_hotel], na.rm = TRUE)*demolition_hotelcomm,0)
        
        demo_col <- paste0("demolition_ag_AO",seawall,"t",trigger)
        Retreat_Analysis[[demo_col]][Retreat_Analysis$Years == year] <- 
          ifelse(year==2025,n_distinct(clean_assessors_bldg_ag$BuildingID[AO_matching_rows_bldg_ag], na.rm = TRUE)*demolition_house,0)
        
        demo_col <- paste0("demolition_comm_AO",seawall,"t",trigger)
        Retreat_Analysis[[demo_col]][Retreat_Analysis$Years == year] <- 
          ifelse(year==2025,sum(clean_assessors_bldg_comm[["BLDG_SQFT"]][AO_matching_rows_bldg_comm], na.rm = TRUE)*demolition_hotelcomm,0)
        
        demo_col <- paste0("demolition_cons_AO",seawall,"t",trigger)
        Retreat_Analysis[[demo_col]][Retreat_Analysis$Years == year] <- 
          ifelse(year==2025,n_distinct(clean_assessors_bldg_cons$BuildingID[AO_matching_rows_bldg_cons], na.rm = TRUE)*demolition_house,0)
        
        #TB 
        demo_col <- paste0("demolition_res_TB",seawall,"t",trigger)
        housecount <- n_distinct(clean_assessors_bldg_res$BuildingID[TB_matching_rows_bldg_res], na.rm = TRUE) - 
          n_distinct(clean_retreat_calcs_apt_res$BuildingID[TB_matching_rows_apt_res], na.rm = TRUE)
        Retreat_Analysis[[demo_col]][Retreat_Analysis$Years == year] <- sum(clean_retreat_calcs_apt_res[["Number_CPRbldg"]][TB_matching_rows_apt_res], na.rm = TRUE)*demolition_apartment + 
                   sum(clean_retreat_calcs_apt_res[["BLDG_SQFT"]][TB_matching_rows_apt_res], na.rm = TRUE)*demolition_aptfoundation+
                   housecount*demolition_house
        
        demo_col <- paste0("demolition_hotel_TB",seawall,"t",trigger)
        Retreat_Analysis[[demo_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_assessors_bldg_hotel[["BLDG_SQFT"]][TB_matching_rows_bldg_hotel][
          !duplicated(clean_assessors_bldg_hotel[["BuildingID"]][TB_matching_rows_bldg_hotel])], na.rm = TRUE)*demolition_hotelcomm
        
        demo_col <- paste0("demolition_ag_TB",seawall,"t",trigger)
        Retreat_Analysis[[demo_col]][Retreat_Analysis$Years == year] <- 
          n_distinct(clean_assessors_bldg_ag$BuildingID[TB_matching_rows_bldg_ag], na.rm = TRUE)*demolition_house
        
        demo_col <- paste0("demolition_comm_TB",seawall,"t",trigger)
        Retreat_Analysis[[demo_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_assessors_bldg_comm[["BLDG_SQFT"]][TB_matching_rows_bldg_comm][
            !duplicated(clean_assessors_bldg_comm[["BuildingID"]][TB_matching_rows_bldg_comm])], na.rm = TRUE)*demolition_hotelcomm
        
        demo_col <- paste0("demolition_cons_TB",seawall,"t",trigger)
        Retreat_Analysis[[demo_col]][Retreat_Analysis$Years == year] <- 
          n_distinct(clean_assessors_bldg_cons$BuildingID[TB_matching_rows_bldg_cons], na.rm = TRUE)*demolition_house
        
        
        #seawall demo per taxclass
        #AO
        seawall_col <- paste0("seawall_res_AO",seawall,trigger)
        Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] <- 
          ifelse(seawall=="_s_",0,ifelse(year==2025,
                                         sum(clean_retreat_calcs_home$SEAWALL_LEN_PERTMK[AO_matching_rows_home], na.rm = TRUE)*demolition_seawall,0))
        
        seawall_col <- paste0("seawall_hotel_AO",seawall,trigger)
        Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] <- 
          ifelse(seawall=="_s_",0,ifelse(year==2025,
                                         sum(clean_retreat_calcs_hotel$SEAWALL_LEN_PERTMK[AO_matching_rows_hotel], na.rm = TRUE)*demolition_seawall,0))
        
        seawall_col <- paste0("seawall_ag_AO",seawall,trigger)
        Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] <- 
          ifelse(seawall=="_s_",0,ifelse(year==2025,
                                         sum(clean_retreat_calcs_ag$SEAWALL_LEN_PERTMK[AO_matching_rows_ag], na.rm = TRUE)*demolition_seawall,0))
        
        seawall_col <- paste0("seawall_comm_AO",seawall,trigger)
        Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] <- 
          ifelse(seawall=="_s_",0,ifelse(year==2025,
                                         sum(clean_retreat_calcs_comm$SEAWALL_LEN_PERTMK[AO_matching_rows_comm], na.rm = TRUE)*demolition_seawall,0))
        
        seawall_col <- paste0("seawall_cons_AO",seawall,trigger)
        Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] <- 
          ifelse(seawall=="_s_",0,ifelse(year==2025,
                                         sum(clean_retreat_calcs_cons$SEAWALL_LEN_PERTMK[AO_matching_rows_cons], na.rm = TRUE)*demolition_seawall,0))
        
        #TB
        seawall_col <- paste0("seawall_res_TB",seawall,trigger)
        Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] <- 
          ifelse(seawall=="_s_",0,
                 sum(clean_retreat_calcs_home$SEAWALL_LEN_PERTMK[TB_matching_rows_home], na.rm = TRUE)*demolition_seawall)
        
        seawall_col <- paste0("seawall_hotel_TB",seawall,trigger)
        Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] <- 
          ifelse(seawall=="_s_",0,
                 sum(clean_retreat_calcs_hotel$SEAWALL_LEN_PERTMK[TB_matching_rows_hotel], na.rm = TRUE)*demolition_seawall)
        
        seawall_col <- paste0("seawall_ag_TB",seawall,trigger)
        Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] <- 
          ifelse(seawall=="_s_",0,
                 sum(clean_retreat_calcs_ag$SEAWALL_LEN_PERTMK[TB_matching_rows_ag], na.rm = TRUE)*demolition_seawall)
        
        seawall_col <- paste0("seawall_comm_TB",seawall,trigger)
        Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] <- 
          ifelse(seawall=="_s_",0,
                 sum(clean_retreat_calcs_comm$SEAWALL_LEN_PERTMK[TB_matching_rows_comm], na.rm = TRUE)*demolition_seawall)
        
        seawall_col <- paste0("seawall_cons_TB",seawall,trigger)
        Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] <- 
          ifelse(seawall=="_s_",0,
                 sum(clean_retreat_calcs_cons$SEAWALL_LEN_PERTMK[TB_matching_rows_cons], na.rm = TRUE)*demolition_seawall)
        
        #RE
        seawall_col <- paste0("seawall_res_RE",seawall,trigger)
        Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] <- 
          ifelse(seawall=="_s_",0,
                 sum(clean_retreat_calcs_home$SEAWALL_LEN_PERTMK[RE_matching_rows_home], na.rm = TRUE)*demolition_seawall)
        
        seawall_col <- paste0("seawall_hotel_RE",seawall,trigger)
        Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] <- 
          ifelse(seawall=="_s_",0,
                 sum(clean_retreat_calcs_hotel$SEAWALL_LEN_PERTMK[RE_matching_rows_hotel], na.rm = TRUE)*demolition_seawall)
        
        seawall_col <- paste0("seawall_ag_RE",seawall,trigger)
        Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] <- 
          ifelse(seawall=="_s_",0,
                 sum(clean_retreat_calcs_ag$SEAWALL_LEN_PERTMK[RE_matching_rows_ag], na.rm = TRUE)*demolition_seawall)
        
        seawall_col <- paste0("seawall_comm_RE",seawall,trigger)
        Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] <- 
          ifelse(seawall=="_s_",0,
                 sum(clean_retreat_calcs_comm$SEAWALL_LEN_PERTMK[RE_matching_rows_comm], na.rm = TRUE)*demolition_seawall)
        
        seawall_col <- paste0("seawall_cons_RE",seawall,trigger)
        Retreat_Analysis[[seawall_col]][Retreat_Analysis$Years == year] <- 
          ifelse(seawall=="_s_",0,
                 sum(clean_retreat_calcs_cons$SEAWALL_LEN_PERTMK[RE_matching_rows_cons], na.rm = TRUE)*demolition_seawall)
        
        
        #osds & wastewater per taxclass
        #AO
        osds_col <- paste0("osdsremoval_res_AO",seawall,"t",trigger)
        wastewater_col <- paste0("wastewaterremoval_res_AO",seawall,"t",trigger)
        Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year] <-
          ifelse(year==2025,sum(clean_retreat_calcs_home$OSDS[AO_matching_rows_home], na.rm = TRUE) * osds_remove,0)
        Retreat_Analysis[[wastewater_col]][Retreat_Analysis$Years == year] <-
          ifelse(year==2025,sum(clean_retreat_calcs_home$WASTEWATER[AO_matching_rows_home], na.rm = TRUE) * wastewater_remove,0)
        
        osds_col <- paste0("osdsremoval_hotel_AO",seawall,"t",trigger)
        wastewater_col <- paste0("wastewaterremoval_hotel_AO",seawall,"t",trigger)
        Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year] <-
          ifelse(year==2025,sum(clean_retreat_calcs_hotel$OSDS[AO_matching_rows_hotel], na.rm = TRUE) * osds_remove,0)
        Retreat_Analysis[[wastewater_col]][Retreat_Analysis$Years == year] <-
          ifelse(year==2025,sum(clean_retreat_calcs_hotel$WASTEWATER[AO_matching_rows_hotel], na.rm = TRUE) * wastewater_remove,0)
        
        osds_col <- paste0("osdsremoval_ag_AO",seawall,"t",trigger)
        wastewater_col <- paste0("wastewaterremoval_ag_AO",seawall,"t",trigger)
        Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year] <-
          ifelse(year==2025,sum(clean_retreat_calcs_ag$OSDS[AO_matching_rows_ag], na.rm = TRUE) * osds_remove,0)
        Retreat_Analysis[[wastewater_col]][Retreat_Analysis$Years == year] <-
          ifelse(year==2025,sum(clean_retreat_calcs_ag$WASTEWATER[AO_matching_rows_ag], na.rm = TRUE) * wastewater_remove,0)
        
        osds_col <- paste0("osdsremoval_comm_AO",seawall,"t",trigger)
        wastewater_col <- paste0("wastewaterremoval_comm_AO",seawall,"t",trigger)
        Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year] <-
          ifelse(year==2025,sum(clean_retreat_calcs_comm$OSDS[AO_matching_rows_comm], na.rm = TRUE) * osds_remove,0)
        Retreat_Analysis[[wastewater_col]][Retreat_Analysis$Years == year] <-
          ifelse(year==2025,sum(clean_retreat_calcs_comm$WASTEWATER[AO_matching_rows_comm], na.rm = TRUE) * wastewater_remove,0)
        
        osds_col <- paste0("osdsremoval_cons_AO",seawall,"t",trigger)
        wastewater_col <- paste0("wastewaterremoval_cons_AO",seawall,"t",trigger)
        Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year] <-
          ifelse(year==2025,sum(clean_retreat_calcs_cons$OSDS[AO_matching_rows_cons], na.rm = TRUE) * osds_remove,0)
        Retreat_Analysis[[wastewater_col]][Retreat_Analysis$Years == year] <-
          ifelse(year==2025,sum(clean_retreat_calcs_cons$WASTEWATER[AO_matching_rows_cons], na.rm = TRUE) * wastewater_remove,0)
        
        # TB
        osds_col <- paste0("osdsremoval_res_TB",seawall,"t",trigger)
        wastewater_col <- paste0("wastewaterremoval_res_TB",seawall,"t",trigger)
        Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year] <-
          sum(clean_retreat_calcs_home[["OSDS"]][TB_matching_rows_home], na.rm = TRUE) * osds_remove
        Retreat_Analysis[[wastewater_col]][Retreat_Analysis$Years == year] <-
          sum(clean_retreat_calcs_home[["WASTEWATER"]][TB_matching_rows_home], na.rm = TRUE) * wastewater_remove
        
        osds_col <- paste0("osdsremoval_hotel_TB",seawall,"t",trigger)
        wastewater_col <- paste0("wastewaterremoval_hotel_TB",seawall,"t",trigger)
        Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year] <-
          sum(clean_retreat_calcs_hotel[["OSDS"]][TB_matching_rows_hotel], na.rm = TRUE) * osds_remove
        Retreat_Analysis[[wastewater_col]][Retreat_Analysis$Years == year] <-
          sum(clean_retreat_calcs_hotel[["WASTEWATER"]][TB_matching_rows_hotel], na.rm = TRUE) * wastewater_remove
        
        osds_col <- paste0("osdsremoval_ag_TB",seawall,"t",trigger)
        wastewater_col <- paste0("wastewaterremoval_ag_TB",seawall,"t",trigger)
        Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year] <-
          sum(clean_retreat_calcs_ag[["OSDS"]][TB_matching_rows_ag], na.rm = TRUE) * osds_remove
        Retreat_Analysis[[wastewater_col]][Retreat_Analysis$Years == year] <-
          sum(clean_retreat_calcs_ag[["WASTEWATER"]][TB_matching_rows_ag], na.rm = TRUE) * wastewater_remove
        
        osds_col <- paste0("osdsremoval_comm_TB",seawall,"t",trigger)
        wastewater_col <- paste0("wastewaterremoval_comm_TB",seawall,"t",trigger)
        Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year] <-
          sum(clean_retreat_calcs_comm[["OSDS"]][TB_matching_rows_comm], na.rm = TRUE) * osds_remove
        Retreat_Analysis[[wastewater_col]][Retreat_Analysis$Years == year] <-
          sum(clean_retreat_calcs_comm[["WASTEWATER"]][TB_matching_rows_comm], na.rm = TRUE) * wastewater_remove
        
        osds_col <- paste0("osdsremoval_cons_TB",seawall,"t",trigger)
        wastewater_col <- paste0("wastewaterremoval_cons_TB",seawall,"t",trigger)
        Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year] <-
          sum(clean_retreat_calcs_cons[["OSDS"]][TB_matching_rows_cons], na.rm = TRUE) * osds_remove
        Retreat_Analysis[[wastewater_col]][Retreat_Analysis$Years == year] <-
          sum(clean_retreat_calcs_cons[["WASTEWATER"]][TB_matching_rows_cons], na.rm = TRUE) * wastewater_remove
        
        # RE 
        osds_col <- paste0("osdsremoval_res_RE",seawall,"t",trigger)
        wastewater_col <- paste0("wastewaterremoval_res_RE",seawall,"t",trigger)
        Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year] <-
          sum(clean_retreat_calcs_home[["OSDS"]][RE_matching_rows_home], na.rm = TRUE) * osds_remove
        Retreat_Analysis[[wastewater_col]][Retreat_Analysis$Years == year] <-
          sum(clean_retreat_calcs_home[["WASTEWATER"]][RE_matching_rows_home], na.rm = TRUE) * wastewater_remove
        
        osds_col <- paste0("osdsremoval_hotel_RE",seawall,"t",trigger)
        wastewater_col <- paste0("wastewaterremoval_hotel_RE",seawall,"t",trigger)
        Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year] <-
          sum(clean_retreat_calcs_hotel[["OSDS"]][RE_matching_rows_hotel], na.rm = TRUE) * osds_remove
        Retreat_Analysis[[wastewater_col]][Retreat_Analysis$Years == year] <-
          sum(clean_retreat_calcs_hotel[["WASTEWATER"]][RE_matching_rows_hotel], na.rm = TRUE) * wastewater_remove
        
        osds_col <- paste0("osdsremoval_ag_RE",seawall,"t",trigger)
        wastewater_col <- paste0("wastewaterremoval_ag_RE",seawall,"t",trigger)
        Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year] <-
          sum(clean_retreat_calcs_ag[["OSDS"]][RE_matching_rows_ag], na.rm = TRUE) * osds_remove
        Retreat_Analysis[[wastewater_col]][Retreat_Analysis$Years == year] <-
          sum(clean_retreat_calcs_ag[["WASTEWATER"]][RE_matching_rows_ag], na.rm = TRUE) * wastewater_remove
        
        osds_col <- paste0("osdsremoval_comm_RE",seawall,"t",trigger)
        wastewater_col <- paste0("wastewaterremoval_comm_RE",seawall,"t",trigger)
        Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year] <-
          sum(clean_retreat_calcs_comm[["OSDS"]][RE_matching_rows_comm], na.rm = TRUE) * osds_remove
        Retreat_Analysis[[wastewater_col]][Retreat_Analysis$Years == year] <-
          sum(clean_retreat_calcs_comm[["WASTEWATER"]][RE_matching_rows_comm], na.rm = TRUE) * wastewater_remove
        
        osds_col <- paste0("osdsremoval_cons_RE",seawall,"t",trigger)
        wastewater_col <- paste0("wastewaterremoval_cons_RE",seawall,"t",trigger)
        Retreat_Analysis[[osds_col]][Retreat_Analysis$Years == year] <-
          sum(clean_retreat_calcs_cons[["OSDS"]][RE_matching_rows_cons], na.rm = TRUE) * osds_remove
        Retreat_Analysis[[wastewater_col]][Retreat_Analysis$Years == year] <-
          sum(clean_retreat_calcs_cons[["WASTEWATER"]][RE_matching_rows_cons], na.rm = TRUE) * wastewater_remove
        
        #tax rev loss per taxclass
        taxrev_col <- paste0("Total_TaxRev_res_RE",seawall,"t", trigger, "_l", hazard_type, "_bv0")
        column_name <- paste0("Total_TaxRev_", year, "_t",trigger,"_l",hazard_type,"_bv0")
        Retreat_Analysis[[taxrev_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_retreat_calcs_home[[column_name]][RE_matching_rows_home], na.rm = TRUE)
        
        taxrev_col <- paste0("Total_TaxRev_hotel_RE",seawall,"t", trigger, "_l", hazard_type, "_bv0")
        column_name <- paste0("Total_TaxRev_", year, "_t",trigger,"_l",hazard_type,"_bv0")
        Retreat_Analysis[[taxrev_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_retreat_calcs_hotel[[column_name]][RE_matching_rows_hotel], na.rm = TRUE)
        
        taxrev_col <- paste0("Total_TaxRev_ag_RE",seawall,"t", trigger, "_l", hazard_type, "_bv0")
        column_name <- paste0("Total_TaxRev_", year, "_t",trigger,"_l",hazard_type,"_bv0")
        Retreat_Analysis[[taxrev_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_retreat_calcs_ag[[column_name]][RE_matching_rows_ag], na.rm = TRUE)
        
        taxrev_col <- paste0("Total_TaxRev_comm_RE",seawall,"t", trigger, "_l", hazard_type, "_bv0")
        column_name <- paste0("Total_TaxRev_", year, "_t",trigger,"_l",hazard_type,"_bv0")
        Retreat_Analysis[[taxrev_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_retreat_calcs_comm[[column_name]][RE_matching_rows_comm], na.rm = TRUE)
        
        taxrev_col <- paste0("Total_TaxRev_cons_RE",seawall,"t", trigger, "_l", hazard_type, "_bv0")
        column_name <- paste0("Total_TaxRev_", year, "_t",trigger,"_l",hazard_type,"_bv0")
        Retreat_Analysis[[taxrev_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_retreat_calcs_cons[[column_name]][RE_matching_rows_cons], na.rm = TRUE)
      
        #private property value loss per taxclass
        privproploss_col <- paste0("Priv_Prop_Loss_res_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
        column_name <- paste0("Priv_Prop_Loss_", year, "_t",trigger,"_l",hazard_type,"_bv0")
        Retreat_Analysis[[privproploss_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_retreat_calcs_home[[column_name]][RE_matching_rows_home], na.rm = TRUE)
        
        privproploss_col <- paste0("Priv_Prop_Loss_hotel_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
        column_name <- paste0("Priv_Prop_Loss_", year, "_t",trigger,"_l",hazard_type,"_bv0")
        Retreat_Analysis[[privproploss_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_retreat_calcs_hotel[[column_name]][RE_matching_rows_hotel], na.rm = TRUE)
        
        privproploss_col <- paste0("Priv_Prop_Loss_ag_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
        column_name <- paste0("Priv_Prop_Loss_", year, "_t",trigger,"_l",hazard_type,"_bv0")
        Retreat_Analysis[[privproploss_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_retreat_calcs_ag[[column_name]][RE_matching_rows_ag], na.rm = TRUE)
        
        privproploss_col <- paste0("Priv_Prop_Loss_comm_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
        column_name <- paste0("Priv_Prop_Loss_", year, "_t",trigger,"_l",hazard_type,"_bv0")
        Retreat_Analysis[[privproploss_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_retreat_calcs_comm[[column_name]][RE_matching_rows_comm], na.rm = TRUE)
        
        privproploss_col <- paste0("Priv_Prop_Loss_cons_RE",seawall,"t",trigger,"_l",hazard_type,"_bv0")
        column_name <- paste0("Priv_Prop_Loss_", year, "_t",trigger,"_l",hazard_type,"_bv0")
        Retreat_Analysis[[privproploss_col]][Retreat_Analysis$Years == year] <- 
          sum(clean_retreat_calcs_cons[[column_name]][RE_matching_rows_cons], na.rm = TRUE)
        
        
        
        for(bval in bvals){
          
          # Sum the total building + land value of parcels that are TB 
          totalval_col <- paste0("Total_Value_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
          column_name <- paste0("Total_Appraise_", year, "_t",trigger,"_l",hazard_type,"_bv",bval)
          Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year] <- 
            sum(clean_retreat_calcs[[column_name]][TB_matching_rows & clean_retreat_calcs$Public == 0], na.rm = TRUE)
          
          # Sum private property loss of parcels that are TB 
          privproploss_col <- paste0("Priv_Prop_Loss_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
          column_name <- paste0("Priv_Prop_Loss_", year, "_t",trigger,"_l",hazard_type,"_bv",bval)
          Retreat_Analysis[[privproploss_col]][Retreat_Analysis$Years == year] <- 
            sum(clean_retreat_calcs[[column_name]][TB_matching_rows], na.rm = TRUE)
          
          # Sum tax revenue for parcels that are TB 
          taxrev_col <- paste0("Total_TaxRev_TB",seawall,"t", trigger, "_l", hazard_type, "_bv",bval)
          column_name <- paste0("Total_TaxRev_", year, "_t",trigger,"_l",hazard_type,"_bv",bval)
          Retreat_Analysis[[taxrev_col]][Retreat_Analysis$Years == year] <- 
            sum(clean_retreat_calcs[[column_name]][TB_matching_rows], na.rm = TRUE)
          
          #BREAKDOWNS PER TAXCLASS
          
          #total value per taxclass
          totalval_col <- paste0("Total_Value_res_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
          column_name <- paste0("Total_Appraise_", year, "_t",trigger,"_l",hazard_type,"_bv",bval)
          Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year] <- 
            sum(clean_retreat_calcs_home[[column_name]][TB_matching_rows_home & clean_retreat_calcs_home$Public == 0], na.rm = TRUE)
          
          totalval_col <- paste0("Total_Value_hotel_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
          column_name <- paste0("Total_Appraise_", year, "_t",trigger,"_l",hazard_type,"_bv",bval)
          Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year] <- 
            sum(clean_retreat_calcs_hotel[[column_name]][TB_matching_rows_hotel & clean_retreat_calcs_hotel$Public == 0], na.rm = TRUE)
          
          totalval_col <- paste0("Total_Value_ag_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
          column_name <- paste0("Total_Appraise_", year, "_t",trigger,"_l",hazard_type,"_bv",bval)
          Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year] <- 
            sum(clean_retreat_calcs_ag[[column_name]][TB_matching_rows_ag & clean_retreat_calcs_ag$Public == 0], na.rm = TRUE)
          
          totalval_col <- paste0("Total_Value_comm_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
          column_name <- paste0("Total_Appraise_", year, "_t",trigger,"_l",hazard_type,"_bv",bval)
          Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year] <- 
            sum(clean_retreat_calcs_comm[[column_name]][TB_matching_rows_comm & clean_retreat_calcs_comm$Public == 0], na.rm = TRUE)
          
          totalval_col <- paste0("Total_Value_cons_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
          column_name <- paste0("Total_Appraise_", year, "_t",trigger,"_l",hazard_type,"_bv",bval)
          Retreat_Analysis[[totalval_col]][Retreat_Analysis$Years == year] <- 
            sum(clean_retreat_calcs_cons[[column_name]][TB_matching_rows_cons & clean_retreat_calcs_cons$Public == 0], na.rm = TRUE)
          
          #tax rev loss per taxclass
          taxrev_col <- paste0("Total_TaxRev_res_TB",seawall,"t", trigger, "_l", hazard_type, "_bv",bval)
          column_name <- paste0("Total_TaxRev_", year, "_t",trigger,"_l",hazard_type,"_bv",bval)
          Retreat_Analysis[[taxrev_col]][Retreat_Analysis$Years == year] <- 
            sum(clean_retreat_calcs_home[[column_name]][TB_matching_rows_home], na.rm = TRUE)
          
          taxrev_col <- paste0("Total_TaxRev_hotel_TB",seawall,"t", trigger, "_l", hazard_type, "_bv",bval)
          column_name <- paste0("Total_TaxRev_", year, "_t",trigger,"_l",hazard_type,"_bv",bval)
          Retreat_Analysis[[taxrev_col]][Retreat_Analysis$Years == year] <- 
            sum(clean_retreat_calcs_hotel[[column_name]][TB_matching_rows_hotel], na.rm = TRUE)
          
          taxrev_col <- paste0("Total_TaxRev_ag_TB",seawall,"t", trigger, "_l", hazard_type, "_bv",bval)
          column_name <- paste0("Total_TaxRev_", year, "_t",trigger,"_l",hazard_type,"_bv",bval)
          Retreat_Analysis[[taxrev_col]][Retreat_Analysis$Years == year] <- 
            sum(clean_retreat_calcs_ag[[column_name]][TB_matching_rows_ag], na.rm = TRUE)
          
          taxrev_col <- paste0("Total_TaxRev_comm_TB",seawall,"t", trigger, "_l", hazard_type, "_bv",bval)
          column_name <- paste0("Total_TaxRev_", year, "_t",trigger,"_l",hazard_type,"_bv",bval)
          Retreat_Analysis[[taxrev_col]][Retreat_Analysis$Years == year] <- 
            sum(clean_retreat_calcs_comm[[column_name]][TB_matching_rows_comm], na.rm = TRUE)
          
          taxrev_col <- paste0("Total_TaxRev_cons_TB",seawall,"t", trigger, "_l", hazard_type, "_bv",bval)
          column_name <- paste0("Total_TaxRev_", year, "_t",trigger,"_l",hazard_type,"_bv",bval)
          Retreat_Analysis[[taxrev_col]][Retreat_Analysis$Years == year] <- 
            sum(clean_retreat_calcs_cons[[column_name]][TB_matching_rows_cons], na.rm = TRUE)
          
          #private property value loss per taxclass
          privproploss_col <- paste0("Priv_Prop_Loss_res_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
          column_name <- paste0("Priv_Prop_Loss_", year, "_t",trigger,"_l",hazard_type,"_bv",bval)
          Retreat_Analysis[[privproploss_col]][Retreat_Analysis$Years == year] <- 
            sum(clean_retreat_calcs_home[[column_name]][TB_matching_rows_home], na.rm = TRUE)
          
          privproploss_col <- paste0("Priv_Prop_Loss_hotel_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
          column_name <- paste0("Priv_Prop_Loss_", year, "_t",trigger,"_l",hazard_type,"_bv",bval)
          Retreat_Analysis[[privproploss_col]][Retreat_Analysis$Years == year] <- 
            sum(clean_retreat_calcs_hotel[[column_name]][TB_matching_rows_hotel], na.rm = TRUE)
          
          privproploss_col <- paste0("Priv_Prop_Loss_ag_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
          column_name <- paste0("Priv_Prop_Loss_", year, "_t",trigger,"_l",hazard_type,"_bv",bval)
          Retreat_Analysis[[privproploss_col]][Retreat_Analysis$Years == year] <- 
            sum(clean_retreat_calcs_ag[[column_name]][TB_matching_rows_ag], na.rm = TRUE)
          
          privproploss_col <- paste0("Priv_Prop_Loss_comm_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
          column_name <- paste0("Priv_Prop_Loss_", year, "_t",trigger,"_l",hazard_type,"_bv",bval)
          Retreat_Analysis[[privproploss_col]][Retreat_Analysis$Years == year] <- 
            sum(clean_retreat_calcs_comm[[column_name]][TB_matching_rows_comm], na.rm = TRUE)
          
          privproploss_col <- paste0("Priv_Prop_Loss_cons_TB",seawall,"t",trigger,"_l",hazard_type,"_bv",bval)
          column_name <- paste0("Priv_Prop_Loss_", year, "_t",trigger,"_l",hazard_type,"_bv",bval)
          Retreat_Analysis[[privproploss_col]][Retreat_Analysis$Years == year] <- 
            sum(clean_retreat_calcs_cons[[column_name]][TB_matching_rows_cons], na.rm = TRUE)
          
        }
      }
    }
  }
}



