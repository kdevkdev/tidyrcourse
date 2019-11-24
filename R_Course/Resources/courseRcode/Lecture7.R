
###----Lecture7, include=FALSE---------------------------------------------
library(knitr)
library(tidyverse)

###----L7load1, eval = EVAL------------------------------------------------
# Initial read of back pain data set
load("../Data/Backpain.Rdata")


###----L7normalityVariables, warning=warn , fig.height=4.5, eval = EVAL----
p <- ggplot(bP, aes(sample=age)) +
  stat_qq() + stat_qq_line()
p


###----L7SHapiroTest, warning=warn , eval = EVAL---------------------------
shapiro.test(bP$bmi[1:2500])


###----L7Cor1, warning=warn , eval = EVAL----------------------------------
bP %>% select(bmi, waistc) %>%
  cor(use = "complete.obs")   #removes NA's where necessary


###----L7Cor2, warning=warn , eval = EVAL----------------------------------
bP %>% select(bmi, waistc, age) %>%
  cor(use = "complete.obs")   #removes NA's where necessary


###----L7Cor3, warning=warn , eval = EVAL----------------------------------
bP %>% select_if(is.numeric) %>% cor(use = "complete.obs")   #removes NA's where necessary


###----L7Cor4, warning=warn , eval = EVAL----------------------------------
bP %>% select(bmi, age, disability) %>%
  cor(use = "complete.obs")   #removes NA's where necessary


###----L7corTest1, warning=warn, eval = EVAL-------------------------------
cor.test(bP$age, bP$disability, use = "complete.obs")


###----L7ChiSQ2, warning=warn,  fig.height=5, eval = EVAL------------------
ALPHA <- 0.1
TITLE <- paste("Jittered plot of disability vs age with alpha = ", as.character(ALPHA))
bP %>%
  select(age, disability) %>%
  ggplot(aes(x=age, y = disability)) +
  geom_jitter(alpha = ALPHA) +
  xlim(50, 100) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle(TITLE)


###----L7Tabyls2way, warning=warn, eval = EVAL-----------------------------
library(janitor)
bP %>%
  tabyl(agegr, comorb) %>%
  adorn_title()



###----L7Tabyls2wayPct1, warning=warn, eval = EVAL-------------------------
bP %>%
  tabyl(country, agegr) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1, affix_sign = FALSE ) %>%
  adorn_title()


###----L7ThreeWayXtab, warning=warn, eval = EVAL---------------------------
tabyl(bP, sex, backPain30, agegr) %>%
  adorn_title()


###----L7ThreeWayXtabPC, warning=warn, eval = EVAL-------------------------
bP %>%
  filter(complete.cases(sex, backPain30, agegr)) %>%
  tabyl( sex, backPain30, agegr) %>%
  adorn_title()


###----L7ChiSQ1, warning=warn, eval = EVAL---------------------------------
ch_age_edu <- chisq.test(bP$agegr, bP$eduS)
ch_age_edu


###----L7ChiSQ1a, warning=warn, eval = EVAL--------------------------------
class(ch_age_edu)
str(ch_age_edu)


###----L7ChiSQ1bb, warning=warn, eval = EVAL-------------------------------
ch_age_edu$observed
ch_age_edu$expected
ch_age_edu$residuals


###----L7ChiSQ1c, warning=warn, eval = EVAL--------------------------------
bP %>%
  filter(complete.cases(agegr, eduS)) %>%
  tabyl(agegr, eduS) %>%
  adorn_totals("row") %>%
  adorn_percentages() %>%
  adorn_pct_formatting(digits = 1, affix_sign = FALSE) %>%
  adorn_title()



###----L7facet1, warning=warn, fig.height=5 , eval = EVAL------------------
ggplot(bP, aes(x= age, y = disability, colour = sex)) +
  geom_point() +
  facet_wrap(~country, nrow = 2)


###----L7LM1, warning=warn , eval = EVAL-----------------------------------
RegModel.1 <- lm(disability~age+bmi, data=bP)
RegModel.1


###----L7LM1summary, warning = warn, eval = EVAL---------------------------
summary(RegModel.1, warning = warn)


###----L7LM2, warning=warn  , eval = EVAL----------------------------------
LinearModel.2 <- lm(disability ~ age + bmi + eduS + physical + residence +
                      wealthQ + comorb + country, data = bP)
summary(LinearModel.2)


###----L7LM3, warning=warn  , eval = EVAL----------------------------------
LinearModel.3 <- lm(disability ~ age + bmi + eduS + physical + residence +
                      wealthQ + comorb + country, data=bP, subset = sex == "Male")
summary(LinearModel.3)


###----L7glm6, warning = warn, eval = EVAL---------------------------------
GLM.6 <- glm(disability ~ age + backPain30 + sex, family = gaussian(identity),
             data = bP)
summary(GLM.6)


###----L7glm8, warning = warn, eval = EVAL---------------------------------
GLM.8 <- glm(backPain30 ~ age + bmi + sex + arthritis + wealthQ,
             family = binomial(logit), data = bP)
summary(GLM.8)

