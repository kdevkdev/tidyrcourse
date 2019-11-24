

###----Lecture3, include=FALSE, eval = TRUE--------------------------------
library(tidyverse)
library(knitr)

###----L3Preliminaries1,  warning=warn, eval = EVAL , eval = EVAL----------
bP  <- read_csv(file="../Data/BackPain.csv", col_types = cols())
bP <- bP %>% mutate_if(is.character, as.factor)
glimpse(bP)


###----L3Filter,  warning=warn, eval = EVAL , eval = EVAL------------------
bPf <- filter(bP, country == 'China' ,
              residence == 'Rural', sex=='Female',
              diabetes == 'yes')
bPf

###----L3Filter1a,  warning=warn, eval = EVAL------------------------------
bPGhana <- filter(bP, country == 'Ghana' )


###----L3Filter2, warning=warn, eval = EVAL--------------------------------
bP <-  filter(bP,complete.cases(bmi,waistc,age, height))    # complete data in bmi etc
summary(bP)


###----L3Filter2a,  warning=warn, eval = EVAL------------------------------
bPg <- filter(bP, country =="Ghana") %>%
  select(bmi)
sum(is.na(bPg))
bPg %>% tally()


###----L3Slice,  warning=warn, eval = EVAL---------------------------------
bPsl <- slice(bP, 1:5)
bPsl
bPs10 <- slice(bP,seq(10,nrow(bP), by =10))  # Select every 10th observation


###----L3arrange,  warning=warn, eval = EVAL-------------------------------
bPa <- arrange(bP,waistc)
bPa   # default is ascending order


###----L3Descending,  warning=warn, eval = EVAL----------------------------
bPa <- arrange(bP,desc(waistc))
head(bPa[,'waistc'],20)    #   Here's the first 20 of them


###----L3SelectNames,  warning=warn, eval = EVAL---------------------------
bPse <- select(bP,country, residence, sex, height, disability, diabetes)
bPse
kable(head(bPse))


###----L3SelectNumber,  warning=warn, eval = EVAL--------------------------
bPsNum <- select(bP, 1:4)
bPsNum


###----L3SelectDelete,  warning=warn, eval = EVAL--------------------------
bPsNum <- select(bPsNum, -sex)
bPsNum


###----L3DeleteNum,  warning=warn, eval = EVAL-----------------------------
bPsNum <- select(bPsNum, -2)
bPsNum



###----L3rename,  warning=warn, eval = EVAL--------------------------------
bPsNum <- rename(bPsNum, wealthQuantile = wealthQ)  # New name = old name
head(bPsNum)



###----L3Mutate,  warning=warn, eval = EVAL--------------------------------
bPse <- mutate(bPse, heightInches = height/2.54)
head(bPse)


###----L3MutateInPlace,  warning=warn, eval = EVAL-------------------------
bPse <- mutate(bPse, height = height/1.0)


###----L3MutateMany, warning = warn, eval = EVAL---------------------------
bP <- mutate(bP, heightM = height/100,
             wHr = waistc/height,
             sageAge = age-50)
head(select(bP, heightM, wHr, sageAge), 10)


###----L3transmute,  warning=warn, eval = EVAL-----------------------------
bPse <-transmute(bPse, htInches = height/2.54, dis100=disability/100, country=country)
head(bPse)


###----L3count1,  warning=warn, eval = EVAL--------------------------------

count(bP,wealthQ)
bPcc <- count(bP,country, residence, wealthQ)     # Produces long form output


###----L3count2,  warning=warn, eval = EVAL--------------------------------
summary(select(bP, residence, sex, wealthQ, country, agegr))


###----L3count3,  warning=warn, eval = EVAL--------------------------------
summary(select_if(bP,is.factor))


###----L2Factors1, warning=warn, eval  = EVAL------------------------------
select(bP, country)  #
levels(bP$country)       # The levels function can't use argument select(bP, country)
str(select(bP, country) )        #structure of bP$country
head(bP, 20)  # print numeric values of first 20 elements
#----------------------------------------------------------------------------------


###----L3Mutate2factor0, warning=warn, eval  = EVAL------------------------
bP %>% select(comorb)
bP <- bP %>% mutate(comorb=as.factor(comorb))
bP %>% select(comorb)


###----L3MutateRecode------------------------------------------------------
bP <- bP %>% mutate(comorb = fct_recode(comorb, 
                                        "None" = "0",
                                        "One" = "1",
                                        "Two+" =  "2"))
bP %>% select(comorb)


###----L3FActors1,  warning=warn, eval = EVAL------------------------------
levels(bP$bmi4)
bP <- bP %>%  mutate(bmi4 = fct_relevel(bmi4, "Underweight","Normal", "Pre-Obese", "Obese"))
levels(bP$bmi4)


###----L3Factors3, warning=warn, eval  = EVAL------------------------------

bP %>% count(country)
bP <- bP %>%
  mutate(country = fct_recode(country,
                              "Russian Fed" = "Russian Federation",
                              "Sth Africa" = "South Africa") )
bP %>% count(country)


###----L2cut, warning=warn, eval  = EVAL-----------------------------------
bP <- bP %>% mutate(height6 =  cut_number(height,
                                          n = 6,
                                          labels = c("Very Short", "Short",
                                                     "Average", "Tall", "Very Tall", "Extremely Tall")))
summary(bP$height6)


###----L3Height4cutInterval,  warning=warn, eval = EVAL--------------------

bP <- bP %>% mutate(height4 = cut_interval(height, n=4))  #,
summary(bP$height4)

bP <- bP %>% mutate(height4 = cut_interval(height, n=4,
                                           labels = c("Very Short", "Short", "Average", "Tall" )))
bP %>% count(height4)




###----L3cutwidth,  warning=warn, eval = EVAL------------------------------
bP <- bP %>% mutate(height4 = cut_width(height, width = 15))  #,
summary(bP$height4)

bP <- bP %>% mutate(height4 = cut_interval(height, n=4,
                                           labels = c("Very Short", "Short", "Average", "Tall" )))
bP %>% count(height4)



###----Lecture3BreaksCut,  warning=warn, eval = EVAL-----------------------

bP <- bP %>% mutate(height4 = cut(height, breaks = c(0,120,150,170,Inf)))
summary(bP$height4)
# Adding nice names is easiest all in one command this way:
bP <- bP %>% mutate(height4 = cut(height, breaks = c(0,120,150,170,Inf),
                                  labels = c("Very Short", "Short", "Average", "Tall" )))
summary(bP$height4)


###----L2CombiningLevels, warning=warn, eval  = EVAL-----------------------
summary(bP$height6)
bP <- bP %>%  mutate(h6_to_4 = fct_recode(height6,
                                          "short" = "Very Short",
                                          "short" = "Short",
                                          "very tall" = "Very Tall",
                                          "very tall" = "Extremely Tall"))

summary(bP$h6_to_4)


###----L3dplyr::summarise,  warning=warn, eval = EVAL----------------------
library(knitr)
crs1 <- group_by(bP,country,residence, sex)
bySex <- summarise(crs1, meanDisability = mean(disability), sdDisability=sd(disability))
bySex
kable(bySex)


###----L3Piping1,  warning=warn, eval = EVAL-------------------------------
crs2 <- bP %>%
  group_by(country,residence, sex) %>%
  summarise(Disability = mean(disability), IQR = IQR(disability))
crs2


###----L3Piping2,  warning=warn, eval = EVAL-------------------------------
crs2 <- bP %>%
  group_by(country,residence, sex) %>%
  summarise(Disability = mean(disability), disIQR = IQR(disability), Bmi = mean(bmi))
crs2


###----L3Piping3,  warning=warn, eval = EVAL-------------------------------
crs2 <- bP %>%
  group_by(country,residence, sex, angina) %>%
  summarise(Disability = mean(disability), disIQR = IQR(disability), Bmi = mean(bmi))
crs2


###----L3Piping4,  warning=warn, eval = EVAL-------------------------------
crs2 <- bP %>%
  group_by(angina, country,residence, sex) %>%
  summarise(Disability = mean(disability), disIQR = IQR(disability), Bmi = mean(bmi))
crs2


###----L3pipe5,  warning=warn, eval = EVAL---------------------------------
crs2 <- bP %>%
  filter(country== "China")%>%
  group_by(angina, residence, sex) %>%
  summarise(Disability = mean(disability), disIQR = IQR(disability), Bmi = mean(bmi))
crs2



###----L3observation_counts1,  warning=warn, eval = EVAL-------------------
aa <- group_by(bP, country, sex, residence, wealthQ) %>%
  summarise( count=n(), mdis=mean(disability))
aa


###----L3SavedfbP,  warning=warn , eval = EVAL-----------------------------
bP  <- read_csv(file="../Data/BackPain.csv", col_types = cols())%>%
  mutate_if(is.character, as.factor) %>%
  mutate(bmi4  = fct_relevel(bmi4, "Underweight","Normal", "Pre-Obese", "Obese"),
         physical = fct_relevel(physical,
                                "low phys act", "mod phys act", "high phys act"),
         works    = fct_relevel(workS,
                                "never worked", "currently not working", "currently working" ),
         eduS     = fct_relevel(eduS,
                                "No primary", "Compl Primary", "Compl Sec/HS", "Compl Uni/Coll"),
         alcohol  = fct_recode(alcohol, "Abstainers",
                               "Drinkers" ="Non-heavy/Infreq heavy/Freq heavy drinkers" ),
         country  = fct_recode(country, 'Russian Fedn.' = 'Russian Federation',
                               'Sth Africa' = 'South Africa'),
         comorb   = as.factor(comorb),
         comorb   = fct_recode(comorb, 'None' = '0',
                               'One'  = '1',
                               'Two+' = '2'),
         wHR      = waistc/height)

count_to_pct <- function(data, ..., col = n) {
  grouping_vars_expr <- quos(...)
  col_expr <- enquo(col)
  data %>%
    group_by(!!! grouping_vars_expr) %>%
    mutate(pct = (!! col_expr) / sum(!! col_expr)) %>%
    mutate(pct = 100*pct) %>%
    ungroup()
}

save(bP, count_to_pct, file="../Data/BackPain.RData")


###----L3LoadbP, eval = FALSE, eval = EVAL---------------------------------
load("../Data/BackPain.RData")


