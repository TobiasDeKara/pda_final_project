library(tidyverse)
library(readxl)
library(dplyr)

providers <- read.csv("provider_list_geo.csv")
countycentroids <- read.csv("county_pop_centroids.csv")
countydem <- read.csv("county_demographics.csv")
abortions <- read.csv("abortions_by_county.csv")

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

#####state data
arizona <- read.csv("arizona.csv")
arizona$state <- "Arizona"
delaware <- read.csv("delaware.csv")
delaware$state <- "Delaware"
abortiondata <- full_join(arizona, delaware, by = names(delaware))
georgia <- read.csv("georgia.csv")
georgia$state <- "Georgia"
abortiondata <- full_join(abortiondata, georgia, by = names(georgia))
illinois <- read.csv("illinois.csv")
illinois$state <- "Illinois"
abortiondata <- full_join(abortiondata, illinois, by = names(illinois))
indiana <- read.csv("indiana.csv")
indiana$state <- "Indiana"
abortiondata <- full_join(abortiondata, indiana, by = names(indiana))
michigan <- read.csv("michigan.csv")
michigan$state <- "Michigan"
abortiondata <- full_join(abortiondata, michigan, by = names(michigan))
newyork <- read.csv("new_york.csv")
newyork$state <- "New York"
abortiondata <- full_join(abortiondata, newyork, by = names(newyork))
northcarolina <- read.csv("north_carolina.csv")
northcarolina$state <- "North Carolina"
abortiondata <- full_join(abortiondata, northcarolina, by = names(northcarolina))
ohio <- read.csv("ohio.csv")
ohio$state <- "Ohio"
abortiondata <- full_join(abortiondata, ohio, by = names(ohio))
oklahoma <- read.csv("oklahoma.csv")
oklahoma$state <- "Oklahoma"
abortiondata <- full_join(abortiondata, oklahoma, by = names(oklahoma))
oregon <- read.csv("oregon.csv")
oregon$state <- "Oregon"
abortiondata <- full_join(abortiondata, oregon, by = names(oregon)[-32])
pennsylvania <- read.csv("pennsylvania.csv")
pennsylvania$state <- "Pennsylvania"
abortiondata <- full_join(abortiondata, pennsylvania, by = names(pennsylvania))
southcarolina <- read.csv("south_carolina.csv")
southcarolina$state <- "South Carolina"
abortiondata <- full_join(abortiondata, southcarolina, by = names(southcarolina))
texas <- read.csv("texas.csv")
texas$state <- "Texas"
abortiondata <- full_join(abortiondata, texas, by = names(texas))
utah <- read.csv("utah.csv")
utah$state <- "Utah"
abortiondata <- full_join(abortiondata, utah, by = names(utah))
vermont <- read.csv("vermont.csv")
vermont$state <- "Vermont"
vermont$X2017 <- as.character(vermont$X2017)
abortiondata <- full_join(abortiondata, vermont, by = names(vermont))
washington <- read.csv("washington.csv")
washington$state <- "Washington"
abortiondata <- full_join(abortiondata, washington, by = names(washington))
wisconsin <- read.csv("wisconsin.csv")
wisconsin$state <- "Wisconsin"
abortiondata <- full_join(abortiondata, wisconsin, by = names(wisconsin))
names(abortiondata)
abortiondata <- abortiondata[, c(33, 1:32, 34)]

abortiondata %>% group_by(state) %>% summarize(percmissing = sum(is.na(X2017)))
names(abortiondata)
aggregate(X2017 ~ state, data=abortiondata, function(x) {sum(is.na(x))}, na.action = NULL)
aggregate(X2018 ~ state, data=abortiondata, function(x) {sum(is.na(x))}, na.action = NULL)

write.csv()
write.csv(abortiondata, "cleanedabortiondata.csv")
write.csv(preabortionreqs, "preabortionreqs.csv")
write.csv(minorsreqs, "minorreqs.csv")
write.csv(abortionbans, "abortionbans.csv")
write.csv(traphospital, "traphospital.csv")
write.csv(trapasc, "trapasc.csv")
write.csv(trapafl, "trapafl.csv")
