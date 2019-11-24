
###----Lecture8, include=FALSE---------------------------------------------

###----L8load1, eval = EVAL------------------------------------------------
# Initial read of back pain data set
load("../Data/Backpain.Rdata")


###----L8LM1, warning=warn , eval = EVAL-----------------------------------
RegModel.1 <- lm(disability~age+bmi, data=bP)
RegModel.1


###----L8LM3, warning=warn  , eval = EVAL----------------------------------
LinearModel.3 <- lm(disability ~ age + bmi + eduS + physical + residence +
                      wealthQ + comorb + country, data=bP, subset = sex == "Male")
summary(LinearModel.3)


###----L8lmObjects, warning=warn , eval = EVAL-----------------------------
str(LinearModel.3)


###----L8LM1anova, warning=warn , eval = EVAL------------------------------
anova(RegModel.1)


###----L8Anova1, warning = warn, eval = EVAL-------------------------------
library(car)
Anova(LinearModel.3, type="II")


###----L8LM5a, warning = warn, eval = EVAL---------------------------------
LinearModel.5 <- lm(disability ~ age + bmi + eduS + physical + residence +
                      wealthQ + comorb, data = bP, subset  = sex == "Male")
summary(LinearModel.5)


###----L8anova2, warning = warn , eval = EVAL------------------------------
anova(LinearModel.3, LinearModel.5)


###----L8subset1, warning = warn, eval = EVAL------------------------------
China <- filter(bP,  country == "China")%>%
  select( -country)%>%
  drop_na()
China


###----L8modelSubset2, warning = warn, eval = EVAL-------------------------
LinearModel.1 <- lm(  bmi  ~ age + sex + residence + wealthQ + wHR,
                      data = China)
summary(LinearModel.1)


###----L8resid1, warning = warn, eval = EVAL-------------------------------
library(car)
fitsLM1 <- fitted(LinearModel.1)
residLM1  <- residuals(LinearModel.1)
df <- data.frame(fitsLM1,residLM1)
China<- bind_cols(China, df)
head(as.data.frame(China))


###----L8qqplot, warning = warn, eval = EVAL-------------------------------

p <- China %>% ggplot(aes(sample=residLM1))
p <- p + stat_qq() + stat_qq_line()
p


###----L8Scatterplot, warning = warn, eval = EVAL--------------------------
ggplot(China) + geom_point(aes(x = bmi, y = bmi), colour = "red") + geom_point(aes(x = bmi, y = fitsLM1), colour = "dark green", alpha = 0.1)


###----L8scatter1, warning = warn , eval = EVAL----------------------------

p <- ggplot(China, aes(x=fitsLM1, y = residLM1)) + geom_point(alpha = 0.1, colour = "brown")
p



###----L8Foxhetero, warning = warn, eval = EVAL----------------------------
library(car)
China %>% scatterplot(residLM1~fitsLM1, reg.line = lm, smooth = TRUE, spread =TRUE,
                      id.method = 'mahal, id.n = 2', span = 0.5, data = .)



###----L8residHist, warning = warn, eval = EVAL----------------------------
p <- ggplot(China, aes(x=residLM1)) +geom_histogram(binwidth = 0.5, colour = "brown", fill = "beige" )
p


###----L8vif1, warning = warn , eval = EVAL--------------------------------
vif(LinearModel.1)


###----L8qqplot2, warning = warn, eval = EVAL------------------------------
car::qqPlot(LinearModel.1, simulate=TRUE, id.method="y", id.n=2)


###----L8plotlm,warning = warn, eval = EVAL--------------------------------
plot(LinearModel.1)

