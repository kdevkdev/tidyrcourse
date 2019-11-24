## Lecture6


###----Lect6,child='./children/Lecture6.rnw', eval = TRUE------------------

###----Lecture6, include=FALSE---------------------------------------------
library(knitr)
library(tidyverse)


###----L6Preliminaries1, warning = warn------------------------------------
load("../Data/BackPain.Rdata") 
bP <- drop_na(bP)


###----L6Multiplot2, eval = EVAL-------------------------------------------
library(gridExtra)

p1 <- ggplot(data = bP, aes(x = bmi, fill = sex))
binsize <- diff(range(bP$bmi))/60
p1 <- p1 + geom_histogram(binwidth = binsize,
                          position = 'identity',
                          colour   = 'brown',
                          alpha    = 0.5)

p2 <- ggplot(data = bP, aes(x = sex, y = bmi))
p2 <- p2 + geom_boxplot(fill = 'red')

grid.arrange(p1,p2,ncol = 2)



###----L6FacetedHistogramsByCountry, eval = EVAL---------------------------
pp1 <- ggplot(bP, aes(x=bmi, fill=agegr)) +
  geom_histogram(binwidth = 1.) +
  facet_grid(country ~ sex,scales="free_y")
pp1


###----L6complCases,warning=warn, eval = EVAL------------------------------
load("../Data/BackPain.Rdata")
bP%>%group_by(country)%>%
  tally()
bP <- filter(bP, complete.cases(
  residence, disability, sex, age, country, bmi4))

bP%>%group_by(country)%>%
  tally()


###----L6bFacetedHistogramsByCountry, eval = EVAL--------------------------
pp1 <- ggplot(bP, aes(x=bmi, fill=agegr)) +
  geom_histogram(binwidth = 1.) +
  facet_grid(country ~ sex,scales="free_y")
pp1


###----L6complCasesA,warning=warn, eval = EVAL-----------------------------
p <- ggplot(bP, aes(x = age,  y = disability, color = residence, shape = sex))
p <- p + geom_point()  + stat_smooth(method = lm, se = FALSE)
p <- p + facet_grid(country~bmi4)
p


###----L6subs9.1, warning=warn, eval = EVAL--------------------------------
bbR <- bP %>%
  filter(country == "Russian Fedn." ) %>%
  count(bmi4, .drop=F) %>%
  rename( nRussia = n)     #Missing code (incuding preceding %>%)
bbR


###----L6Mexnumbers, eval = EVAL-------------------------------------------
bbM <- bP %>%
  filter( country == "Mexico")  %>%
  count(bmi4)%>%
  rename(nMexico = n)    #Missing code (incuding preceding %>%)
bbM
bbb <- left_join(bbR,bbM)  #default key!!
bbb


###----L6gather9.2,warning=warn, eval = EVAL-------------------------------
# Now gather the reduced version of bP on 3 measured variables
library(tidyr)
bPg <- gather(bP, variable, value = value, age, disability, bmi)
dim(bPg)     # We're expecting 3* bP's rows
str(bPg)     # Much better

levels(bPg$variable)

p <- ggplot(na.omit(bPg), aes(x = country, y = value, colour=variable))
p + geom_boxplot()



###----L6boxes,warning=warn, eval = EVAL-----------------------------------
p <- p + geom_boxplot() + ylim(0,90)
p
p <- p + facet_grid(residence~sex)
p


###----L6WDI1,warning=warn, eval = EVAL------------------------------------
library(WDI)
WDIsearch(string='gnp', field='indicator', cache=NULL)


###----L6WDI1a,warning=warn, eval = EVAL-----------------------------------
WDIsearch(string='mobile', field='name', cache=NULL)


###----L6WDI11b,warning=warn, eval = EVAL----------------------------------

SAGE_PPP <- WDI(country = c("CN", "GH", "IN", "MX", "RU", "ZA") ,
                indicator = 'NY.GNP.PCAP.PP.CD',
                start = 1985, end = 2017, extra = FALSE, cache = NULL)   #GNP
otherC <- c("AU", "DE","IR", "SE", "VN" )
SAGE_MPH <-  WDI(country = c("CN", "GH", "IN", "MX", "RU", "ZA") ,
                 indicator = 'IT.CEL.SETS.P2',
                 start = 1985, end = 2017, extra = FALSE, cache = NULL)      #Mob Phones
SAGE_HPG <-  WDI(country = c("CN", "GH", "IN", "MX", "RU", "ZA") ,
                 indicator = 'SH.XPD.CHEX.PP.CD',         # Code not in WDI_CETS.xls !?
                 start = 1985, end = 2017, extra = FALSE, cache = NULL)
str(SAGE_PPP)
head(SAGE_PPP,40)
names(SAGE_PPP)[3] <- "PPP"
names(SAGE_MPH)[3] <- "MPH"
names(SAGE_HPG)[3] <- "HPG"
SAGE_HPG <- SAGE_HPG %>% mutate(HPG = HPG * 10.)  # NB (Easier to see in the plot)  !!!!!!
sum(is.na(SAGE_MPH))
sum(is.na(SAGE_HPG))



###----L6merge_ts9_2,warning=warn, eval = EVAL-----------------------------
df <- left_join(SAGE_PPP, SAGE_MPH, sort = FALSE)
df <- left_join(df, SAGE_HPG, sort = FALSE)
df$country <- factor(df$country)
#!!!!!! Abbreviating RF and SA improves the plot a little
df <- df %>% mutate(country = factor(country, labels = c('China', 'Ghana',
                                                         'India', 'Mexico', 'Russian Fed.', 'Sth Africa')))
head(df)
head(SAGE_PPP)
head(SAGE_MPH)
head(SAGE_HPG)

#--------------------------------------------------------------------------------


jj <- 0
dd <- rep(0,6)      # create a 6 element vector, each element is 0

# Normalize per capita income for plotting (divide each year's per capita income
#      data by the max of the country's per capita income data)

for (i in levels(df$country)){        # loop through the levels of country
  jj <- jj + 1                        # jj will take values 1-6 as we cycle through the loop
  dd[jj] <- max(df$PPP[df$country == i], na.rm=T)   # Find the maximum PPP for country i
  # Store in vector dd
  df$PPP[df$country == i] <- df$PPP[df$country==i]/dd[jj]*100. # divide PPP for country i                                                            # by the max PPP for country i
}
df <- select(df, -iso2c)    # Cleaning up - don't need this


###----L6plot_TS,warning=warn, eval = EVAL---------------------------------
tsg <- gather(df, variable, value, PPP, MPH, HPG)      # !!!!!!!!Note removal of -ve's

head(tsg,50)


###----L6textdf,warning=warn, eval = EVAL----------------------------------
tdf <- data.frame( country = levels(df$country),                           #1.
                   x1 = rep(2000, 6),
                   y1 = rep(Inf, 6),                                       #2.
                   text1 = paste("PPP (max) Normalized to 100 = ", as.character(dd)))
tdf$text1 <- as.character(tdf$text1)                                         #3.
tdf


###----L6plotTS2,warning=warn, eval = EVAL---------------------------------
p <- ggplot(data = tsg,
            aes(x = year, y = value, colour = variable))                   #1.
p <- p + geom_line() + facet_grid(country ~ ., scale = "free_y")           #2.
p + geom_text(data = tdf, aes(x = x1, y = Inf, label = text1),             #3.
              vjust=2, inherit.aes = FALSE)



###----L6Anno9_1,warning=warn, eval = EVAL---------------------------------
lm_eqn = function(bb){
  m = lm(wHR ~ bmi, bb);# Note: the function here has not been generalised
  # with arguments for different variable names
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(coef(m)[1], digits = 2),
                        b = format(coef(m)[2], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}



###----L6Anno9_2,warning=warn, eval = EVAL---------------------------------

pp4 <- ggplot(bP)
pp4 <- pp4 + aes(x = bmi, y = wHR*100, shape = residence, colour = agegr, group = 1)
pp4 <- pp4 + geom_point()
pp4 <- pp4 + stat_smooth(method = lm,level = 0.99) # assumes formula = y ~ x
pp4 <- pp4 + facet_grid(country ~ sex)
pp4 <- pp4 + labs(title = "Compare bmi vs waistHtRaio")
pp4 <- pp4 + ylab("waistHtRatio*100")
pp4 <- pp4 + xlab("BMI")

eqnDF <- bP %>% group_by(country, sex ) %>% summarise(le = lm_eqn(.))
pp4 <- pp4 + geom_text(data = eqnDF,aes(x = 40,y = 180,label = le),
                       parse = TRUE,inherit.aes = FALSE)

ggsave(filename = "plotsout/plot8.png",plot = pp4)




###----L6AnnotateFacetLBF, eval = EVAL-------------------------------------
library(ggpmisc)
my.formula <- y ~ x
p <- ggplot(data = bP, aes(x = bmi, y = wHR, shape = residence, colour = agegr, group = 1)) +
  geom_smooth(method = "lm", se=FALSE) + ylim(0,1.5) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~",x=30, y=1.4), y=1.4),
               parse = TRUE) +
  geom_point()+ facet_grid(country~sex)
p
ggsave(filename = "plotsout/plot9.png",plot = p)

