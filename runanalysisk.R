# run full scripts

source("C:/Users/rsett/Documents/KauaiRetreat/1assessorsk.R")

# add regional/ahupuaa/moku filter if desired

allisland <- clean_retreat_calcs
kekaha <- clean_retreat_calcs[clean_retreat_calcs$Community == "Kekaha", ]
kapaa <- clean_retreat_calcs[clean_retreat_calcs$Community == "Kapaʻa", ]

#overwrite clean_retreat_calcs to selected ahupuaa/qaqc
clean_retreat_calcs <- kapaa
titlename <- 'Kapaʻa' # Kapaʻa Kekaha Kauaʻi 

#kauai

source("C:/Users/rsett/Documents/KauaiRetreat/2retreatyearvaluetaxk.R")
source("C:/Users/rsett/Documents/KauaiRetreat/3costsovertimek.R")
source("C:/Users/rsett/Documents/KauaiRetreat/3infrastructurek.R")
source("C:/Users/rsett/Documents/KauaiRetreat/4discountedtotalcostsk.R")
source("C:/Users/rsett/Documents/KauaiRetreat/5figuresk.R")

#view figures of interest
print(fig3) #all scenarios
print(fig4)

