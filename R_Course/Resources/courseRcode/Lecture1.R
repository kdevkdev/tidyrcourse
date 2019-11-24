

###----Lecture1 basic-random,  warning=warn, eval=EVAL----------------------
##Normally distributed N(0,1) variates, mean, var
#-----------------------------------------------------------------------------------
set.seed(1121)  # set a seed for pseudo-random number generation
(x <- rnorm(20))   # creates a vector, x, containing 20 N(0,1) variates
mean(x)         # computes the mean of the sample
var(x)          # computes the variance of the sample
#----------------------------------------------------------------------------------


###----L1Dataframe1, warning=warn, eval=EVAL-------------------------------
##Reading and summarising data frames
#----------------------------------------------------------------------------------
library(tidyverse)

bP        <- read_csv(file="../Data/BackPain.csv", na = c(""," ","NA"),col_types = cols())      # (1), (2), (3), (4)
glimpse(bP)
bP             # bP is a tibble so only 10 lines are printed (and a limited number of variables, too)


###----L1Dataframe2, warning=warn, eval=EVAL-------------------------------
bP <- bP %>%                                      # (1)
  mutate_if(is.character, as.factor) %>%          # (2)
  filter(complete.cases(.)) %>%                   # (3)
  mutate(waistHtRatio = waistc/height)            # adds a new variable called waistHtRatio  (4)


###----L1Tabyls1way, warning=warn, eval=EVAL-------------------------------
library(janitor)
c1 <- bP %>%
  tabyl(country)
c1


###----L1Tabyls2way, warning=warn, eval=EVAL-------------------------------
c2 <- bP %>%
  tabyl(country, eduS)
c2


###----L1Tabyls2wayPct, warning=warn, eval=EVAL----------------------------

c3 <- bP %>%
  tabyl(country, agegr) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2)
c3



###----L1Re-orderLevels, warning=warn , eval=EVAL--------------------------
bP <- bP %>%
  mutate(eduS = fct_relevel(eduS,"No primary","Compl Primary",
                            "Compl Sec/HS", "Compl Uni/Coll"),
         fct_relevel(bmi4, "Underweight","Normal",
                     "Pre-Obese", "Obese"))
#-------------------------------------------------------------------------------


###----L1TableReOrder, warning=warn , eval=EVAL----------------------------
c3 <- bP %>%
  tabyl(country, eduS) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2)
c3


###----L1tables2, warning=warn, eval=EVAL----------------------------------
bySex <- group_by(bP,country,residence, sex) %>%
  summarise( meanDisability = mean(disability), sdDisability=sd(disability))
# kable(bySex)


###----L1_FacetedHistograms, warning = warn, eval=EVAL---------------------

pp1 <- ggplot(bP, aes(x=bmi, fill=agegr)) +
  geom_histogram(binwidth = 2.5) +
  facet_grid(country ~ sex,scales="free_y")
pp1


###----L1ScatterFacet, warning=warn, eval=EVAL-----------------------------
bP <- bP %>% mutate(bmi4 = factor(bmi4, levels =
        c("Underweight","Normal", "Pre-Obese", "Obese")))
p <- ggplot(bP, aes(x = age,  y = disability ,color = residence, shape = sex)) +
  geom_point() +
  facet_grid(country~bmi4)
p



###----Tut1,child='./children/Tutorial1.Rnw', eval = TRUE------------------

