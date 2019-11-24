
###----Lecture10, include=FALSE--------------------------------------------
library(knitr)
library(tidyverse)
load("../Data/BackPain.Rdata")
bP <- drop_na(bP)


###----L10List1, eval = EVAL-----------------------------------------------
##
#---------------------------------------------------------------------------------
a  <- matrix(sample(1:20, 16, replace=TRUE), ncol=4)      #1. see note on sample()
b  <- rep(c(3, 17, 4, 6, 11), 4)
c1 <- c('Lena', 'Nice day!')
l1 <- list(M = a, b = b, c2 = c1)                         #2. creating the list
l1



###----L10ListAccess, eval = EVAL------------------------------------------

class(l1$c2)                   #1. Dollar notation returns original object class
l1$c2[2]                       #2.  accessing an element
#    of a list member


###----L2List10, eval = EVAL-----------------------------------------------
l1[2]                          #1.  Single bracket
class(l1[2])                   #2.  Single bracket returns list!!
l1[2:3]                        #3.  Single bracket can access multiple items !!
class(l1[2:3])


###----L10List3, eval = EVAL-----------------------------------------------

class(l1[['c2']])
l1[['c2']][1]                                #1.  double bracket with name

class(l1[[1]])
l1[[1]][4,1]                                 #2.  and with index


###----L10addRemoveListItem, eval = EVAL-----------------------------------
d <- c(64, 69)
l1$d <- d                                    #1.  Add a new member
names(l1)[4] <- 'newName.d'                  #2.  Give it a name
l1
# Remove the item, using its name
l1$newName.d <- NULL
l1



###----L10apply, eval = EVAL-----------------------------------------------
bPNum <- subset(bP,select= c(age,bmi,waistc,disability, height)) #select most of the
# numeric variables
pbw <-  function(x, power)
{
  x^power
}

head(bPNum)
sqbP <- apply(bPNum, 2, FUN = pbw, power = 1/3)
head(sqbP)
apply(bPNum, 2, range, na.rm = T)



###----L10sapply, eval = EVAL----------------------------------------------
nums <- sapply(bP, is.numeric)    # -> a vector of T/F for the columns (variables)
bPnum <- bP[ , nums]
names(bPnum)
# and here are the factors
facs <- sapply(bP,is.factor)
bPfac <- bP[ , facs]
names(bPfac)


###----L10lapply, eval = EVAL----------------------------------------------
bP <- as.data.frame(bP)
countryList <- list(bmiChina = bP[bP$country == "China","bmi"],
                    bmiIndia = bP[bP$country == "India","bmi"],
                    bmiGhana = bP[bP$country == "Ghana","bmi"])
str(countryList)
lapply(countryList,mean, na.rm=T)
bP <- as_tibble(bP)



###----L10dplyr2, eval = EVAL----------------------------------------------
library(dplyr)
bmisum <- group_by(bP, country)
bmiM <- summarise(bmisum, bmiMean = mean(bmi, na.rm=TRUE), bmiSD = sd(bmi))
bmiM



###----L10tapply-----------------------------------------------------------

waistc   <- tapply(bP$waistc,bP$country,mean, na.rm = TRUE)
waistcSD <- tapply(bP$waistc,bP$country,sd,  na.rm = TRUE)
height   <- tapply(bP$height,bP$country,mean,  na.rm = TRUE)
heightSD <- tapply(bP$height,bP$country,sd,  na.rm = TRUE)
bmi      <- tapply(bP$bmi,bP$country,mean,  na.rm = TRUE)
bmiSD    <- tapply(bP$bmi,bP$country,sd,  na.rm = TRUE)
cbind(waistc, waistcSD, height, heightSD, bmi, bmiSD)  # make them into a table


###----Lecture10FncEx1, eval = EVAL----------------------------------------
f3 <- function(x = 0, y = 0){
  x*3*y
}
x <- 7
y <- 2
f3(x,y)


###----L10FunctionComponents, eval = EVAL----------------------------------
x <- 7
y <- 2
f3(x,y)
f3
ff3 <- formals(f3)
ff3
str(ff3)
af3 <- args(f3)
af3
str(af3)
body(f3)
environment(f3)




###----L10InlineFUnction, eval = EVAL--------------------------------------
singleLine <- function(x)  mean(c(x+6, x^2))
singleLine(6)


###----L10Function1, eval = EVAL-------------------------------------------
#Simple functions showing alternative output mechanisms
gg1 <- function(a, b){
  (a == b)                                   # Last evaluation is returned
}

gg2 <- function(a, b){
  return(a == b)                            # Explicit statement of return
}
gg3 <- function(a, b){
  d <- (a == b)
  return(d)                                #  Another way
}

a1 <- c(1, 1, 2)
b1 <- c(1, 1, 1)

gg1(a1, b1)
gg2(a1, b1)
gg3(a1, b1)



###----L10Function2, eval = EVAL-------------------------------------------
# USe of default arguments in a function, showing alternative modes of calling
gg4 <- function(a = c(1, 1, 1), b = c(1, 1, 1), f = c(1, 1, 1)){
  # All arguments have a default value
  d <- ( a == b & a == f)
  return(d)
}
a1 <- c(1, 1, 2)
b1 <- c(1, 1, 2)
f1 <- c(1, 1, 2)
gg4(a = a1, b1 ,f1)                                          #1.
gg4(a1, b1)                                                  #2.
gg4()                                                        #3.
gg4(f = f1)                                                  #4.
gg4(a1, f = f1)


###----L10funcVec, eval = EVAL---------------------------------------------
# Here's a function which applies functions to a vector, using ...
myVecFunc <- function(vec,FUN,...){
  FUN(vec,...)
}

x <- sample(1:50,20)
x[7] <- NA
x
myVecFunc(x,sum)                           #  sum returns NA because there is an NA
myVecFunc(x,sum,na.omit = TRUE)            #  sum returns NA - na.omit not arg
myVecFunc(x,sum,na.rm = TRUE)              #  sum removes NA, computes sum
myVecFunc(x,sort)                          #  default sort removes NA
myVecFunc(x,sort,na.last=TRUE)             #  argument puts NA last in sorted list



###----L10Function3, eval = EVAL-------------------------------------------
# Here's an example making use of default arguments and showing the effect of
# different ways of calling the function
gg5 <- function(a = c(1, 1, 1), b = c(1, 1, 1), f = c(1, 1, 1),text="Comparisons"){
# All arguments have a default value
d <- (a == b )
e <- (a == f)
h <- (b == f)
LL <- list(text = text, aEqb =d, aEqf = e, bEqf = h)
return(LL)
}
a1 <- c(1, 1, 2)
b1 <- c(1, 1, 2)
f1 <- c(1, 1, 2)
mm <- gg5(a1,f=c(1,2,1))
mm


###----L10function 4, eval = EVAL------------------------------------------
# User defined functions as arguments of basic functions

# Here is a user defined function
myF1 <- function(x, y=1){
cat("User entered x = ", x, ", y = ", y, "\n")

mean(x)*y
}
bb <- cbind(sample(1:33,10,replace = T),1:10)
apply(bb, 2, myF1)          # user function with default value of y
head(bb)
apply(bb, 2, myF1, y=2)      # User function with y = 2



###----L10birthdaysSource, eval = EVAL-------------------------------------
# Sourcing your own functions
dir()
source('../Resources/birthdays.r')
birthdays("Lisa")

