

###----Lecture4, include=FALSE---------------------------------------------

library(tidyverse)
library(knitr)

###----L4Preliminaries1, eval = EVAL , eval = EVAL-------------------------
load("../Data/BackPain.Rdata")
bP <- drop_na(bP)


###----L4wHRvsBMI1a, warning=warn, eval = EVAL , eval = EVAL---------------
p <- ggplot(bP )


###----L4wHRvsBMI1b, warning=warn, eval = EVAL-----------------------------
p <- p + geom_point(aes(x = bmi, y = waistc))


###----L4wHRvsBMI1c, warning=warn, eval = EVAL-----------------------------
p


###----L4filterMex,warning=warn, eval = EVAL-------------------------------
bPMex <-  filter(bP, country=="Mexico", agegr=="60-69")


###----L4WHRvsBMIc,warning=warn, eval = EVAL-------------------------------

p <- ggplot(bPMex) +
  geom_point(aes(x = bmi, y = wHR,  colour = residence ),size=3 )
p


###----L4WHRvsBMId,warning=warn, eval = EVAL-------------------------------
p <- ggplot(bPMex) +
  geom_point(aes(x = bmi, y = wHR,  colour = residence, shape = sex ),size=3 )
p


###----L4WHRvsBMI,warning=warn, eval = EVAL--------------------------------
p <- ggplot(bPMex, aes(x = bmi, y = wHR,  colour = residence, shape = sex)) +
  geom_point(size=3)
p


###----L4ggplot4,warning=warn, eval = EVAL---------------------------------
p <- p   +
  geom_smooth(method=lm, size = 1)
p


###----L4ggplotResAngBMIa,warning=warn, eval = EVAL------------------------
p <- ggplot(bP, aes(x = residence, y = bmi,  colour = angina )) +
  geom_boxplot()
p


###----L4ggplotResAngBMIb,warning=warn, eval = EVAL------------------------

p <- ggplot(bP) +
  geom_boxplot(aes(x = residence, y = bmi,  colour = angina ))
p


###----L4BoxplotWEa,warning=warn, eval = EVAL------------------------------
p <- ggplot(bP, aes(x = eduS, y = height,  colour = wealthQ )) + geom_boxplot()
p


###----L4BoxplotWEb,warning=warn, eval = EVAL------------------------------
p <- ggplot() +
  geom_boxplot(data = bP, aes(x = eduS, y = height,  colour = wealthQ ))
p


###----L4box1, eval = EVAL-------------------------------------------------
p <- ggplot(data = bP, aes(x = interaction(sex, residence, backPain30), y=bmi))
p <- p + geom_boxplot(fill = 'orange')
p


###----L4box2, eval = EVAL-------------------------------------------------
p <- ggplot(data = bP, aes(x = interaction(backPain30, residence, sex), y=bmi))
p <- p + geom_boxplot(fill = 'pink')
p



###----L4hist1, eval = EVAL------------------------------------------------
p <- ggplot(data = bP, aes(x = bmi))
p <- p + geom_histogram()
p


###----L4hist2, eval = EVAL------------------------------------------------
p <- ggplot(data = bP, aes(x = bmi))
p <- p + geom_histogram(binwidth = 0.25, fill = 'beige', colour = 'brown')
p


###----L4hist3, eval = EVAL------------------------------------------------
binsize <- diff(range(bP$bmi))/60
p <- ggplot(data = bP, aes(x = bmi))
p <- p + geom_histogram(binwidth = binsize, fill = 'beige', colour = 'brown')
p


###----L4hist4, eval = EVAL------------------------------------------------
p <- ggplot(data = bP, aes(x = bmi, fill = sex))
p <- p + geom_histogram(binwidth = binsize,
                        position = 'identity',
                        colour   = 'brown',
                        alpha    = 0.5)
p


###----L4op1, eval = EVAL--------------------------------------------------
p <- ggplot(bP, aes(x = bmi, y = waistc))
p <- p + geom_point(alpha=0.1)
p <- p + stat_smooth(method = lm, lwd = 2)
p

