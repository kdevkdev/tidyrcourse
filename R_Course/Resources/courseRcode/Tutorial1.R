##----Tutorial1, include=FALSE--------------------------------------------

###----Tl1Getwd, eval = EVAL-----------------------------------------------
getwd()


###----T1dir, eval = EVAL--------------------------------------------------
dir()


###----T1read.table, eval = EVAL-------------------------------------------
library(tidyverse)
bP        <- read_csv(file="../Data/BackPain.csv", na = "", comment = '#', skip = 0, col_types = cols())
# bP is a tibble so only 10 lines are printed (and a limited number of variables, too)
select(bP,alcohol)
bP <- bP %>% mutate_if(is.character, as.factor)
select(bP,alcohol)


###----T1Dataframe3, eval = EVAL-------------------------------------------
sum(is.na(bP))   # compute number of NA's
bP <- bP %>% drop_na()  # Use this to remove all cases (rows) containing at least one NA from
#  The non-tidyverse way  bP <- na.omit(bP)
sum(is.na(bP))     #check whether all NA's are removed
#----------------------------------------------------------------------------------


###----T1Columns, eval = EVAL----------------------------------------------
##Column selection in data frames  *******Use names!!!!!!
#-----------------------------------------------------------------------------------
head(bP[11])      #print out the 1st 6 elements of bmi....OOOPS!
head(bP$bmi)       #print out the 1st 6 elements of bmi
names(bP)          # Ah - bmi is col 12!
#-----------------------------------------------------------------------------------
bP %>% select(bmi)


###----T1readStat, eval = EVAL---------------------------------------------
library(haven)
bPx <- read_dta("../Data/BackPainData_1.dta")
summary(bPx)
save(bPx, file =  "../Data/BackPainData_1.Rdata")


###----T1listRemove, eval = EVAL-------------------------------------------
vect1 <- bP$agegr                                #1 Created a vector object
ls()                                      #2 List all objects in WD
objects()                                                 #2 List all objects in WD
remove(vect1)                                            #3 Remove object 'vect1'
ls()


###----T1save, eval = EVAL-------------------------------------------------
save("bP", file="BackPain.RData")
dir()
unlink("BackPain.RData")


###----T1load1, eval = EVAL------------------------------------------------
remove(bP)
ls()


###----T1load2, eval = EVAL------------------------------------------------
load("BackPain.RData")
ls()



###----T1renameObject, eval = EVAL-----------------------------------------
bP2 <- bP
ls()
rm(bP)
ls()


###----T1dput1, eval = EVAL------------------------------------------------
t1 <- "The cow jumped over the moon, the little dog laughed to see such fun"
dput(t1, file="t.txt")
t2 <- dget("t.txt")
t2
unlink("t.txt")

###----T1packages1, eval = EVAL--------------------------------------------
## Using the data in the base release of R
search()
library(help=datasets)
data(Nile)
summary(Nile)
plot(Nile)
