library(tidyverse)
library(readxl)
library(dplyr)

providers <- read.csv("provider_list_geo.csv")
countycentroids <- read.csv("county_pop_centroids.csv")
countydem <- read.csv("county_demographics.csv")
abortions <- read.csv("abortions_by_county.csv")
names(abortions)
aggregate(X2017 ~ state, data=abortions, function(x) {sum(is.na(x))/length(x)}, na.action = NULL)
aggregate(X2018 ~ state, data=abortiondata, function(x) {sum(is.na(x))/length(x)}, na.action = NULL)


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

min(preabortionreqs$"Effective Date")
min(minorsreqs$"Effective Date")
min(abortionbans$"Effective Date")
min(traphospital$"Effective Date")
min(trapasc$"Effective Date")
min(trapafl$"Effective Date")

write.csv(abortions, "cleanedabortiondata.csv")
write.csv(preabortionreqs, "preabortionreqs.csv")
write.csv(minorsreqs, "minorreqs.csv")
write.csv(abortionbans, "abortionbans.csv")
write.csv(traphospital, "traphospital.csv")
write.csv(trapasc, "trapasc.csv")
write.csv(trapafl, "trapafl.csv")


state_restrictions <- data.frame(state = state.name)

state_restrictions$parental_involvement <- c(1, 0, 1, 1, 0, 1, 0, rep(1, 3), 0, rep(1, 7), 0, rep(1, 6), 0, 1, rep(0, 5), rep(1, 4), 0, rep(1, 7), 0, 1, 0, rep(1, 3))
state_restrictions$two_trips <- c(0, 0, 1, rep(0, 10), 1, rep(0, 3), 1, rep(0, 5), 1, 1, rep(0, 9), 1, rep(0, 8), 1, rep(0, 4), 1, 0)
state_restrictions$trap <- c(1, 0, 0, 1, rep(0, 9), 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, rep(0,9), 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, rep(0, 6))
state_restrictions$unconstitutional_ban <- c(1, rep(0, 7), 1, 1, 0, 1, 0, 1, 1, rep(0, 5), 1, 1, rep(0, 3), rep(1, 3), rep(0, 3), rep(1, 3), rep(0, 3), rep(1, 4), 0, 1, 1, 0, 1, 0, 0, 0,1) 

write.csv(state_restrictions, "state_restrictions2010")
