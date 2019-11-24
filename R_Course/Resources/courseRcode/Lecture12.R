
## ----Lecture12, include=FALSE--------------------------------------------
set_parent("../tidyRcourseBook.rnw")

## ----Dates1--------------------------------------------------------------
##Standard Dates (from the base )
#-----------------------------------------------------------------------------------
date()   # Character string for date - not so good for manipulation
str(date())
z <- Sys.time() # Standard POSIX time and date class - time stored as seconds from ?
z
startP1 <- as.POSIXct("1970-01-01 1:0:01")   # allowing for 1 hour ahead of 
# Greenwich (UTC)
startP1
as.numeric(startP1)
str(z)
z <- z+ 86400
z
#-----------------------------------------------------------------------------------

## ----Dates2--------------------------------------------------------------

mbd <- "9.05.1945"                      # Perhaps this came from a .csv file
as.Date(mbd, format = "%d.%m.%Y")
jbd <- "09/14/51"             # !!!!!!!!!!!!Careful!!!# From some other source
as.Date(jbd, format = "%m/%d/%y")
tbd <- "5-June-1978"                    # ...and a third one
tbD <- as.Date(tbd, format ="%e-%B-%Y")
str(tbD)

# And converting a Date back to some other character format:
strftime(tbD, format = "%d.%m.%Y" )
as.numeric(tbD)                      # the number is the number of days after....?

# ...and R's default 'origin'
as.numeric(as.Date("1-1-1970", format ="%e-%m-%Y"))

# Today's Date:
Sys.Date()
strftime(Sys.Date(), format = "%e-%B-%Y")   # specify a format


## ----L12lubridateEx1-----------------------------------------------------
library(lubridate)
dmy("8-May-1945")                               # It's VE day.
dmy("8:May:1945")
dmy("8/May/1945")
dmy("8/05-1945")
dmy("8-5:1945")
VE_day <- mdy("5-8:1945")
mdy("5-8:1945")
mdy("5-8:1945")
ymd("1945/May/8")
ymd("1945/05-08")
ymd(19450508)


## ----L12lubridateEx2-----------------------------------------------------
dmy_hm("16/02/2017 11:00")                                #(straight from one of my Excel spreadsheets)
dmy_hm("14/July/1977 21:45")                              # Does anyone know who was born at that time?
datetime <- dmy_hm("14/July/1977 21:45")
str(datetime)

## ----L12lubridateEx4-----------------------------------------------------
wday(today())      # Monday is day `1`
mday(today())
wday(datetime)                                            # What day of the week was she born?
month(VE_day)

## ----L12lubridateEx3-----------------------------------------------------
vignette("lubridate")

## ----L10DebugConditions--------------------------------------------------
x <- 6
warning("Jenny","Stewart",x < 10)
warning("Jenny ","Stewart ",x < 10)
message(x)


## ----T12discrimR, eval = EVAL--------------------------------------------
   # Computing discriminant for solution of quadratic equation
   discrim = function(x) {
     A = x[1]
     B = x[2]
     C = x[2]
     tf <- B^2 > 4*A*C
       return(tf)
   }
   
## ----T10example1, eval = EVAL--------------------------------------------
   library(knitr)
   # Four example coefficients
   x1 <- c(3,4,1)      #a = 3, b = 4,   c = 1
   x2 <- c(1,2,1)
   x3 <- c(1.1,4,3)
   x4 <- c(1,2,2)
   
   X <- as.matrix(cbind(x1,x2,x3,x4))  # Load the test values into the columns of X
   
   dd <- apply(X,2,discrim)            # test the four example sets
   xt <- t(X)                          # transpose the matrix
   colnames(xt) <- c("A", "B", "C")    # Name the matrix columns
   xt
   df1 <- data.frame(xt, Discrmt_gt_0 = as.logical(dd)) # Add a test result column
   kable(df1, align = 'lccr', digits = 1)
   


