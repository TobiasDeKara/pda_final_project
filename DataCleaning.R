#load required libraries
library(tidyverse)
library(readxl)
library(dplyr)

#load in data
providers <- read.csv("provider_list_geo.csv")
countycentroids <- read.csv("county_pop_centroids.csv")
countydem <- read.csv("county_demographics.csv")
abortions <- read.csv("abortions_by_county.csv")

#look at missing data by state for a given year
aggregate(X2017 ~ state, data=abortions, function(x) {sum(is.na(x))/length(x)}, na.action = NULL)
aggregate(X2018 ~ state, data=abortiondata, function(x) {sum(is.na(x))/length(x)}, na.action = NULL)

#load in specific abortion ban data
preabortionreqs <- read_excel("PreabortionRequirements.xlsx", sheet = 2)
preabortionreqs <- preabortionreqs %>% dplyr::select("Jurisdiction", "Effective Date", "Valid Through Date", "MWP_WaitPeriod")
preabortionreqs$MWP_WaitPeriod[preabortionreqs$MWP_WaitPeriod == "."] <- 0
preabortionreqs <- preabortionreqs %>% dplyr::rename(waitperiod = MWP_WaitPeriod)

minorsreqs <- read_excel("Minors.xlsx", sheet = 2)
minorsreqs <- minorsreqs %>% dplyr::select("Jurisdiction", "Effective Date", "Valid Through Date", "Minor_consentreq")
minorsreqs$Minor_consentreq[minorsreqs$Minor_consentreq == "."] <- 0
minorsreqs <- minorsreqs %>% dplyr::rename(parentalconsent = Minor_consentreq)

abortionbans <- read_excel("AbortionBans.xlsx", sheet = 2)
abortionbans <- abortionbans %>% dplyr::select("Jurisdiction", "Effective Date", "Valid Through Date", "Prohibit_Req")
abortionbans$Prohibit_Req[abortionbans$Prohibit_Req == "."] <- 0
abortionbans <- abortionbans %>% dplyr::rename(gestcutoff = Prohibit_Req)

traphospital <- read_excel("TRAPHospitalization.xlsx", sheet = 2)
traphospital <- traphospital %>% dplyr::select("Jurisdiction", "Effective Date", "Valid Through Date", "trap-hr-reghosp")
traphospital <- traphospital %>% dplyr::rename(traphospitalreqs = "trap-hr-reghosp")

trapasc <- read_excel("TRAPASC.xlsx", sheet = 2)
trapasc <- trapasc %>% dplyr::select("Jurisdiction", "Effective Date", "Valid Through Date", "trap-ascr-regambsurgcenrequirm")
trapasc <- trapasc %>% dplyr::rename(trapascreqs = "trap-ascr-regambsurgcenrequirm")

trapafl <- read_excel("TRAPAFL.xlsx", sheet = 2)
trapafl <- trapafl %>% dplyr::select("Jurisdictions", "Effective Date", "Valid Through Date", "trap-aflr-regaborprocforaborprov2") %>% dplyr::rename(licensingreqs = "trap-aflr-regaborprocforaborprov2")

trapafl$licensingreqs <- case_when(trapafl$licensingreqs == "No, but the law imposes requirements for transfer agreements and/or admitting privileges" | trapafl$licensingreqs == "Yes" ~ 1, trapafl$licensingreqs == "No" ~ 0)

#look at years represented by specific ban data
min(preabortionreqs$"Effective Date")
min(minorsreqs$"Effective Date")
min(abortionbans$"Effective Date")
min(traphospital$"Effective Date")
min(trapasc$"Effective Date")
min(trapafl$"Effective Date")

#update files
write.csv(abortions, "cleanedabortiondata.csv")
write.csv(preabortionreqs, "preabortionreqs.csv")
write.csv(minorsreqs, "minorreqs.csv")
write.csv(abortionbans, "abortionbans.csv")
write.csv(traphospital, "traphospital.csv")
write.csv(trapasc, "trapasc.csv")
write.csv(trapafl, "trapafl.csv")

# recreate summary pdf on abortion bans for 2010

state_restrictions <- data.frame(state = state.name)
state_restrictions$parental_involvement <- c(1, 0, 1, 1, 0, 1, 0, rep(1, 3), 0, rep(1, 7), 0, rep(1, 6), 0, 1, rep(0, 5), rep(1, 4), 0, rep(1, 7), 0, 1, 0, rep(1, 3))
state_restrictions$two_trips <- c(0, 0, 1, rep(0, 10), 1, rep(0, 3), 1, rep(0, 5), 1, 1, rep(0, 9), 1, rep(0, 8), 1, rep(0, 4), 1, 0)
state_restrictions$trap <- c(1, 0, 0, 1, rep(0, 9), 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, rep(0,9), 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, rep(0, 6))
state_restrictions$unconstitutional_ban <- c(1, rep(0, 7), 1, 1, 0, 1, 0, 1, 1, rep(0, 5), 1, 1, rep(0, 3), rep(1, 3), rep(0, 3), rep(1, 3), rep(0, 3), rep(1, 4), 0, 1, 1, 0, 1, 0, 0, 0,1) 

write.csv(state_restrictions, "state_restrictions2010")

# calculate distances from county centroids to closest provider location
library(geosphere)

for (i in 1:nrow(countycentroids)) {
  lat <- countycentroids$LATITUDE[i]
  long <- countycentroids$LONGITUDE[i]
  dists <- distHaversine(cbind(long, lat), cbind(providers$Lon, providers$Lat))
  countycentroids$distancetoprovider[i] <- min(dists)
}

write.csv(countycentroids, "countypopcentroids")


abortions <- read.csv("cleanedabortiondata.csv")
names(abortions)
abortions2010 <- abortions %>% dplyr::select(state, county, X2010)

# clean countydem data to make counties and states match abortion count data 
countydem <- read.csv("county_demographics.csv")
library(stringr)
for (i in 1:nrow(countydem)) {
  countydem$state[i] <- unlist(strsplit(countydem$NAME[i], ", "))[length(unlist(strsplit(countydem$NAME[i], ", ")))]
  county <- unlist(strsplit(countydem$NAME[i], " "))[unlist(strsplit(countydem$NAME[i], " ")) != "County," & unlist(strsplit(countydem$NAME[i], " ")) != countydem$state[i]]
  countydem$county[i] <- paste(county, collapse = " ")
}

countydem <- countydem %>% filter(countydem$state %in% state.name)

for (i in 1:nrow(countydem)) {
  countydem$state.abb[i] <- state.abb[which(state.name == countydem$state[i])]
}

for (i in 1:nrow(abortions2010)) {
  if (length(countydem$Women[countydem$state.abb == abortions2010$state[i] & countydem$county == abortions2010$county[i]]) == 0) {
    abortions2010$Women[i] <- NA
  }
  else {
    abortions2010$Women[i] <- countydem$Women[countydem$state.abb == abortions2010$state[i] & countydem$county == abortions2010$county[i]]
  }
}

#fix NA values
abortions2010$X2010 <- na_if(abortions2010$X2010, "?")
abortions2010$X2010 <- na_if(abortions2010$X2010, "")
abortions2010$X2010 <- as.numeric(abortions2010$X2010)
abortions2010 <- abortions2010 %>% mutate(rate = X2010/Women)


write.csv(abortions2010, "abortionrates2010")


