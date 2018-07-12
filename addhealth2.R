# SETUP 
library(AER)
library(stargazer)
library(tidyverse)
load("~/data/ahabuse/data_r/DS0022/21600-0022-Data.rda")
load("~/data/ahabuse/data_r/DS0001/21600-0001-Data.rda")
load("~/data/ahabuse/data_r/DS0031/21600-0031-Data.rda")
WIKeep <- select(da21600.0001, c("AID", "S4", "S5", "S6A", "S6B", "S6C", "S6D", "S6E", "S7", "S12", "S18", "AH_PVT", "PA55", "PA1", "PA2", "PA12", "PB2", "PB8"))
plusWI <- merge(da21600.0022, WIKeep, by="AID")
WIVWeight <- select(da21600.0031, c("AID",  "GSWGT4_2"))
ah <- merge(plusWI, WIVWeight, by="AID")
ah <- as_data_frame(ah)
ah$weight <- ah$GSWGT4_2


# CLEANING
ah$income08 <- ah$H4EC2
ah$income08_no0 <- na_if(ah$income08, 0)
ah$loginc08 <- log(ah$income08_no0)

noyes <- "1=0; 2=1"
ah$female <- as.integer(ah$BIO_SEX4)
ah$female <- Recode(ah$female, noyes)

ah$edlvl <- as.integer(ah$H4ED2)
ah$edlvl <- Recode(ah$edlvl, "1:2 = 1; 3 = 2; 4:6 = 3; 7:8 = 4; 9:10 = 5; 11 = 6; 12 = 4; 13 = 5")
ah$edlvl <- factor(ah$edlvl, labels = c("_noHS", "_HS", "_someColl", "_BA", "_MAProf", "_PhD"))

ah$physabuse <- as.integer(ah$H4MA3)
ah$physabuse <- Recode(ah$physabuse, "1:5 = 1; 6 = 0")

ah$sexabuse <- as.integer(ah$H4MA5)
ah$sexabuse <- Recode(ah$sexabuse, "1:5 = 1; 6 = 0")

ah$sexassault <- as.integer(ah$H4SE34)
ah$sexassault <- Recode(ah$sexassault, noyes)

ah$hhinc94 <- ah$PA55 / 10 # Childhood household income, 1 = $10,000

ah$testscore <- ah$AH_PVT


# Mental health indicator matrix
ah$depression <- as.integer(ah$H4ID5H)
ah$depression <- Recode(ah$depression, noyes)
ah$anxiety <- as.integer(ah$H4ID5J)
ah$anxiety <- Recode(ah$anxiety, noyes)
ah$ptsd <- as.integer(ah$H4ID5I)
ah$ptsd <- Recode(ah$ptsd, noyes)
ah$mhdx <- ifelse(ah$depression==1 | ah$anxiety==1 | ah$ptsd==1, 1, 0)


# Parents' Education, reported by responding parent, Wave I
ah$PA1 <- ah$PA1 %>% fct_recode(male = "(1) (1) Male", female = "(2) (2) Female")
ah$PB2 <- ah$PB2 %>% fct_recode(male = "(1) (1) Male", female = "(2) (2) Female")
ah$p1ed <- as.integer(ah$PA12)
ah$p2ed <- as.integer(ah$PB8)

ah <- within(ah, {
     edmother <- NA
     edmother[PA1 == "female" &  (p1ed == 1 | p1ed == 2 | p1ed == 3 | p1ed == 10)] <- "_noHS"
     edmother[PA1 == "female" &  (p1ed == 4 | p1ed == 5)] <- "_HS"
     edmother[PA1 == "female" &  (p1ed == 6 | p1ed == 7)] <- "_someColl"
     edmother[PA1 == "female" &  (p1ed == 8 )] <- "_BA"
     edmother[PA1 == "female" &  (p1ed == 9 )] <- "_PostBA"
     edmother[PA1 == "female" &  (p1ed == 11 | p1ed == 12)] <- NA_character_
     
     edmother[PA2 == "female" &  PA1 != "female" & (p2ed == 1 | p2ed == 2 | p2ed == 3 | p2ed == 10)] <- "_noHS"
     edmother[PB2 == "female" &  PA1 != "female" & (p2ed == 4 | p2ed == 5)] <- "_HS"
     edmother[PB2 == "female" &  PA1 != "female" & (p2ed == 6 | p2ed == 7)] <- "_someColl"
     edmother[PB2 == "female" &  PA1 != "female" & (p2ed == 8 )] <- "_BA"
     edmother[PB2 == "female" &  PA1 != "female" & (p2ed == 9 )] <- "_PostBA"
     edmother[PB2 == "female" &  PA1 != "female" & (p2ed == 11 | p2ed == 12)] <- NA_character_
     
     edfather <- NA
     edfather[PA1 == "male" &  (p1ed == 1 | p1ed == 2 | p1ed == 3 | p1ed == 10)] <- "_noHS"
     edfather[PA1 == "male" &  (p1ed == 4 | p1ed == 5)] <- "_HS"
     edfather[PA1 == "male" &  (p1ed == 6 | p1ed == 7)] <- "_someColl"
     edfather[PA1 == "male" &  (p1ed == 8 )] <- "_BA"
     edfather[PA1 == "male" &  (p1ed == 9 )] <- "_PostBA"
     edfather[PA1 == "male" &  (p1ed == 11 | p1ed == 12)] <- NA_character_
     
     edfather[PA2 == "male" &  PA1 != "male" & (p2ed == 1 | p2ed == 2 | p2ed == 3 | p2ed == 10)] <- "_noHS"
     edfather[PB2 == "male" &  PA1 != "male" & (p2ed == 4 | p2ed == 5)] <- "_HS"
     edfather[PB2 == "male" &  PA1 != "male" & (p2ed == 6 | p2ed == 7)] <- "_someColl"
     edfather[PB2 == "male" &  PA1 != "male" & (p2ed == 8 )] <- "_BA"
     edfather[PB2 == "male" &  PA1 != "male" & (p2ed == 9 )] <- "_PostBA"
     edfather[PB2 == "male" &  PA1 != "male" & (p2ed == 11 | p2ed == 12)] <- NA_character_
})

ah$edmother <- factor(ah$edmother, levels = c("_noHS", "_HS", "_someColl", "_BA", "_PostBA"))
ah$edfather <- factor(ah$edfather, levels = c("_noHS", "_HS", "_someColl", "_BA", "_PostBA"))

# Race / Ethnicity, self-reported, Wave I
ah$h <- as.integer(ah$S4)
ah$w <- as.integer(ah$S6A)
ah$b <- as.integer(ah$S6B)
ah$a <- as.integer(ah$S6C)
ah$i <- as.integer(ah$S6D)
ah$o <- as.integer(ah$S6E)


ah <- within(ah, {
     ethrace <- NA
     ethrace[w == 2 & b == 1 & a == 1 & i == 1 & h == 1 & o == 1] <- "_white"
     ethrace[w == 1 & b == 2 & a == 1 & i == 1 & h == 1 & o == 1] <- "_black"
     ethrace[w == 1 & b == 1 & a == 2 & i == 1 & h == 1 & o == 1] <- "_asian"
     ethrace[w == 1 & b == 1 & a == 1 & i == 2 & h == 1 & o == 1] <- "_native"
     ethrace[w == 1 & b == 1 & a == 1 & i == 1 & h == 2 & o == 1] <- "_hispanic"
     ethrace[w == 1 & b == 1 & a == 1 & i == 1 & h == 2 & o == 2] <- "_hispanic"
     ethrace[ (w == 2 & (b == 2 | a == 2 | i == 2 | h == 2 | o == 2)) |
                   (b == 2 & (a == 2 | i == 2 | h == 2 | o == 2)) |
                   (a == 2 & (i == 2 | h == 2 | o == 2)) |
                   (i == 2 & (h == 2 | o == 2)) |
                   (h == 2 & o == 2)] <- "_twoplus"
     ethrace[w == 1 & b == 1 & a == 1 & i == 1 & h == 1 & o == 2] <- "_other"
})

ah$ethrace <- factor(ah$ethrace, levels = c("_white", "_black", "_asian", "_native", "_hispanic", "_twoplus", "_other"))


# MODELLING
unweighted <- lm(loginc08 ~ sexabuse + physabuse + edlvl + female + ethrace + hhinc94 + testscore, data = ah)

weighted <- lm(loginc08 ~ sexabuse + physabuse + edlvl + female + ethrace + hhinc94 + testscore, weights = weight, data = ah)

weighted_ped <- lm(loginc08 ~ sexabuse + physabuse + edlvl + female + ethrace + hhinc94 + testscore + edmother + edfather, weights = weight, data = ah)

weighted_int <- lm(loginc08 ~ sexabuse + physabuse + edlvl + female*ethrace + hhinc94 + testscore, weights = weight, data = ah)

weighted_int_ped <- lm(loginc08 ~ sexabuse + physabuse + edlvl + female*ethrace + hhinc94 + testscore + edmother + edfather, weights = weight, data = ah)

stargazer(unweighted, weighted, weighted_ped, weighted_int, weighted_int_ped, type="text")

