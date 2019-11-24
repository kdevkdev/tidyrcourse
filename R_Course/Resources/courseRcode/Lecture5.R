## Lecture 5

library(tidyverse)
library(knitr)

load("../Data/BackPain.Rdata")
bP <- drop_na(bP)


###----L5toyDF,  warning = warn , eval =  EVAL-----------------------------
# Toy example

hhID <- 1:4
hData1 <- c("X1", "X2", "X3", "X4")
hData2 <- letters[5:8]
hhDf <- data.frame(hID = as.factor(hhID),
                   hD1 = hData1,
                   hD2 = hData2)
hhDf


###----L5BindDupRow,  warning = warn , eval =  EVAL---------f---------------
hhDf <- bind_rows(hhDf,hhDf[2,])    # bind_rows adds rows of equal length below the data frame
hhDf


###----L5RemoveDupRow, warning = warn , eval =  EVAL-----------------------
hhDf <- distinct(hhDf)    # Removes the subsequent duplicated rows
hhDf


###----L5toy2,  warning=warn , eval =  EVAL--------------------------------
#
ID <- 1:15
hhID <- c(1,1,1,1,1,2,2,3,3,3,4,4,4,4,4)
iData1 <- LETTERS[1:15]
iData2 <- letters[12:26]

iDf <- data.frame(id = as.factor(ID),
                  hID = as.factor(hhID),
                  iD1 = iData1,
                  iD2 = iData2)
iDf



###----L3MergeWithLeftJoin,  warning=warn , eval =  EVAL-------------------
merged <- left_join(iDf, hhDf, by="hID")
merged
merged2 <- left_join(iDf, hhDf)
merged2



###----L5Tabyls1way,  warning=warn , eval =  EVAL--------------------------
library(janitor)
c1 <- bP %>%
  tabyl(country)
c1


###----L5Tabyls2way,  warning=warn , eval =  EVAL--------------------------
c2 <- bP %>%
  tabyl(country, wealthQ)
c2


###----L5Tabyls2wayPct,  warning=warn , eval =  EVAL-----------------------

c3 <- bP %>%
  tabyl(country, bmi4) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2)
c3



###----L5TableRe-order,  warning=warn  , eval =  EVAL----------------------
c3 <- bP %>%
  tabyl(country, eduS) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2)
c3


###----L5tabyl3Wayr,  warning=warn  , eval =  EVAL-------------------------
c3 <- bP %>%
  tabyl(eduS, wealthQ, country) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2)
c3


###----L7dplyr2waycount,  warning=warn , eval =  EVAL----------------------
library(knitr)
bP%>%
  count(country, agegr) %>%
  spread(key = agegr, value = n) %>%
  as.data.frame() %>%
  kable()


###----L5dplyr3waycount,  warning=warn , eval =  EVAL----------------------
bP %>% count(country, sex, agegr,  wealthQ) %>%
  group_by(country, sex, agegr,  wealthQ) %>%
  spread(key = wealthQ, value = n)



###----L5dplyr2waypct,  warning=warn , eval =  EVAL------------------------
bP%>%
  count(country, agegr) %>%
  count_to_pct() %>%
  as.data.frame() %>%
  kable()


###----L5SummariseIf1,  warning=warn  , eval =  EVAL-----------------------
summarise_if(bP, is.numeric,list(
  Mean  = mean,
  Median = median,
  SD    = sd,
  IQR   = IQR,
  MAX   = max,
  MIN   = min))


###----L5SummaryNum, warning=warn  , eval =  EVAL--------------------------

# Modified from Stack Overflow #

bP_sum <- bP %>%
  summarise_if(is.numeric, list(min = ~min, 
                                q25 = ~quantile(., 0.25), 
                                median = ~median, 
                                q75 = ~quantile(., 0.75), 
                                max = ~max,
                                mean = ~mean, 
                                sd = ~sd))

bP_sum <- bP_sum %>% gather(stat, val) %>%                     # tidyr
  separate(stat, into = c("var", "stat"), sep = "_") %>%       # tidyr
  spread(stat, val) %>%                                        # another tidyr function
  select(var, min, q25, median, q75, max, mean, sd)            # reorder columns
bP_sum


###----L5SummaryFactors,  warning=warn , eval =  EVAL----------------------

bPFactors <- bP %>%
  select_if(is.factor) %>%
  gather(name,value) %>%  # reshape datset
  count(name, value)      # count combinations
as.data.frame(bPFactors)  # There' a bug in this!!!!!!!!!!!!


###----L5Chaining_tidyr_commands , eval =  EVAL----------------------------
bPgather <- bP %>%
  mutate( wHR=100*wHR) %>%                           # Multiply wHR by 100
  gather(key = variable, value = value, disability, bmi, wHR)      # Convert to long form



###----L5ggplotEx1 , eval =  EVAL------------------------------------------
library(ggplot2)
p <- ggplot(bPgather, aes(x = interaction(country,sex), y = value, colour=variable))
p <- p + geom_boxplot()
p


###----L5ggplotEx2 , eval =  EVAL------------------------------------------
library(ggplot2)
p <- ggplot(bPgather, aes(x = country, y = value, colour=variable))
p <- p + geom_boxplot()
p


###----L5ggplotEx3 , eval =  EVAL------------------------------------------
library(ggplot2)
p <- ggplot(bPgather, aes(x = eduS, y = value, colour=variable))
p <- p + geom_boxplot()
p


###----L5ggplotEx4 , eval =  EVAL------------------------------------------
library(ggplot2)
p <- ggplot(bPgather, aes(x = wealthQ, y = value, colour=variable))
p <- p + geom_boxplot()
p


###----L5ggplotEx5 , eval =  EVAL------------------------------------------
library(ggplot2)
p <- ggplot(bPgather, aes(x = physical, y = value, colour=variable))
p <- p + geom_boxplot()
p

