
###---Tutorial5

###----Tut5,child='./children/Tutorial5.rnw', eval = TRUE------------------

###----Tutorial5, include=FALSE--------------------------------------------
library(knitr)
library(tidyverse)


###----T5ExploringGhanaData1, warning = warn, eval = EVAL------------------
library(tidyverse)
load("../Data/BackPain.Rdata")
bP %>% filter(country == "Ghana") %>% summary()
bP <- bP %>%
  drop_na()
bP %>% filter(country == "Ghana") %>% summary()


###----T5ExploringGhanaData2, warning = warn, eval = EVAL------------------
library(tidyverse)
load("../Data/BackPain.Rdata")
bP <- bP %>%
  filter(complete.cases(eduS, wealthQ)) %>%
  filter(country == "Ghana")
summary(bP)


###----T5tabylTWay, warning=warn , eval = EVAL-----------------------------
library(janitor)
c2 <- bP %>%
  tabyl(eduS, wealthQ) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2)
c2


###----T5dplyr1, eval = EVAL-----------------------------------------------
load("../Data/BackPain.Rdata")
by_country <- group_by(bP, country)
sumry <- summarise(by_country,
                   waistcM = mean(waistc, na.rm=TRUE),
                   waistcSD = sd(waistc, na.rm=TRUE),
                   heightM  = mean(height, na.rm=TRUE),
                   heightSD = sd(height, na.rm=TRUE),
                   bmiM     = mean(bmi, na.rm=TRUE),
                   bmiSD    = sd(bmi, na.rm=TRUE))
sumry



###----T5dplyr2, eval = EVAL-----------------------------------------------
library(dplyr)
bmisum <- group_by(bP, country)
bmiM <- summarise(bmisum,bmiM = mean(bmi, na.rm=TRUE))
bmiM

