
## Lecture 2

library(tidyverse)
library(knitr)

###----L2Coercion,  warning=warn, eval = EVAL------------------------------
##Automatic variable conversion (Coercion)
#----------------------------------------------------------------------------------
A <- 11
B <- 7
C1 <- A + B
a <- c(A,B,C1)  # Concatenates or binds the arguments into a vector
a
str(a)     #  Very useful standard command in R for printing the structure of an object
d <- "Victoria"
t <- c(d,'Geoff', 'Lars')
t
t <- c(t,'Stig')
t
t <- c(t,A)
t
t <- c(1,2,"Eugenie")
str(t)
#----------------------------------------------------------------------------------


###----L2Vectors, warning=warn, eval = EVAL--------------------------------
##vector manipulations
#----------------------------------------------------------------------------------
length(a)
ss <- 5:22      # creates a vector, ss, whose elements are the sequence 5,6... to 22
ss
ss[6]
length(ss)
# and a little bit tricky:
sss <- ss[ss > 20]   # assigns those values of ss which are greater than 8 to a
# new vector sss
sss
sum(sss)
#-----------------------------------------------------------------------------------


###----L2VectorAddressing, warning=warn, eval = EVAL-----------------------

cc <- c(1, 4, 98, 17, 43, 172, 34, 67, 58, 143, 27, 72, 44, 88, 83)
cc
cc[c(3,5,8)]
ee <- c('Tom', 'Dick', 'Harry', 'Nils', 'Lars', 'Stig', 'Hans')
ee
eMharry <- ee[-3]
eMharry


###----L2DataFrames1, warning=warn, eval = EVAL----------------------------
people <- c('Lena', 'Solveig', 'Guinevere', 'Hans', 'Erik' )
byear <-  c(1982,      1976,      1949,      2001,   1967)
score <-  c( 22,        43,        87,        45,     60)
salary <- c(40,         47,        31,        NA,     75)
df <- data.frame(Name = people, BirthYear = byear, Score = score, Salary = salary)


###----L2df2, warning=warn, eval  = EVAL-----------------------------------
df$Name[3]    # List form - 3rd row of column 'Name'
df[2,3]       # Matrix form - 2nd row of 3rd column
df[,3]        # Row number omitted, assume ALL rows, i.e. this is equivalent to df$score
df[4:5,1:3]   # Selects the subset - rows 4 and 5, columns 1  to 3


###----L2df3, warning=warn, eval  = EVAL-----------------------------------
#Other basic functions -a reminder
summary(df)
names(df)
names(df)[3]
dim(df)
ncol(df)
nrow(df)
str(df)
str(people)
class(df)


###----L2Tibble1, warning=warn, eval  = EVAL-------------------------------
df <- as_tibble(df)
class(df)


###----L2Tibble2, warning=warn, eval  = EVAL-------------------------------
df <- as.data.frame(df)
class(df)


###----L2asCharacter, warning=warn, eval  = EVAL---------------------------
plotNumber <- 3
t1 <- paste('This is plot number', as.character(plotNumber), sep = " ")
t1


###----L2asNumeric, warning=warn, eval  = EVAL-----------------------------

chv <- '7'
17 + chv               # Does not work
17 + as.numeric(chv)


###----L2Kable1, eval =FALSE-----------------------------------------------
library(knitr)
kable(head(iris))
kable(head(iris, 1))


###----L2Kable2, eval =FALSE-----------------------------------------------
kable(head(iris), row.names=TRUE)


###----L2Kable3, eval =FALSE-----------------------------------------------
#create a 5 row datframe with variables with different distributions.
kable(data.frame(Rnorm = rnorm(5), Runif = runif(5), Rbeta = rbeta(5, .5, .5), RPoisson = rpois(5, 0.5)))



###----L2test-d, eval =FALSE-----------------------------------------------
kable(head(iris), align=c('l', 'c', 'r', 'l', 'r'))

