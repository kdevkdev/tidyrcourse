
# ----Lecture11, include=FALSE--------------------------------------------
library(knitr)


# ----L11Loops1a, eval = EVAL---------------------------------------------
 ##For loops
 #------------------------------------------------------------------------------------
 # Throw 2 dice 10000 times
 n <- 100000
 s1 <- Sys.time()
 result <- numeric(n)                                                              #1.
 for (i in 1:n) {                                                                  #2.
   result[i] <- sum(sample(1:6,2,replace=T))                                       #3.
 }
 diff <- Sys.time() - s1
 diff
 df <- data.frame(result = result)
 ggplot(df) + geom_bar(aes(x = result), fill = "light blue")
 


 #----L11Loops1b, eval = EVAL---------------------------------------------
 # Throw 2 dice 100000 times
 n <- 100000
 s1 <- Sys.time()
 result <- numeric(n)                                                              #1.
 for (i in seq(length=n)) {                                                        #2.
   result[i] <- sum(sample(1:6,2,replace=T))                                       #3.
 }
 diff = Sys.time()-s1
 diff
 hist(result)
 df <- data.frame(result = result)
 ggplot(df) + geom_bar(aes(x = result), fill = "light blue", bins = 11)
 


 #----L11Loops2, eval = EVAL----------------------------------------------
 #   looping through a string sequence
 
 bP <- read.table(file="../Data/BackPain.csv",sep=",",header=TRUE)
 bP <- na.omit(bP)    #Remove all the rows with NA's
 str(bP)
 countries <- levels(bP$country)
 dfprint <- data.frame(Country = countries, RespondentNumbers= integer(6))        #1.
 for (ic in seq(countries)){                                                      #2.
   dfprint[ic,2]<- nrow(bP[bP$country == countries[ic],])                         #3.
 }
 # Now lets add the total numbers to the data frame. A little trickier because
 # it's a new level in our country factors
 totalNumbers <- nrow(bP)
 dfprint$Country <- factor(dfprint$Country, levels = c(levels(dfprint$Country),
                                                       "Total"))                  #4.
 extraRow <- data.frame(Country = as.factor("Total"),RespondentNumbers = totalNumbers)
 dfprint <- bind_rows(dfprint,extraRow)                                           #5.
 print(dfprint)                                                                   #6.
 
 #---------------------------------------------------------------------------------


 #----L11Loops3, eval = EVAL----------------------------------------------
 ##While loops
 # In This example we start after a miraculous sequence of three twelves (what's the
 #  probability?)
 # Our little script will tell us how many throws it takes to return an average
 # within eps of the
 # theoretical mean of 7.
 
 nThrows     <- 3                                                                    #1.
 totalThrows <- 36                                                                   #2.
 plotVec     <- c(NULL)                                                              #3.
 delta       <- 6.                                                                   #4.
 set.seed(8237)  #This ensures that your pseudo-random sampling is the same as mine!
 eps         <- 0.001  # Arbitrarily chosen tolerance
 while (abs(delta) > eps) {
    nThrows          <- nThrows + 1                                                  #5.
    totalThrows      <- totalThrows + sum(sample(1:6,2,replace=T))                   #6.
    meanThrows       <- totalThrows/nThrows
    delta            <- (meanThrows - 7)
    if(nThrows%%100 == 0) cat("After ",nThrows, " throws,  delta = ", delta, "\n")                                               #7.
    plotVec[nThrows] <- delta                                                        #8.
 }
 
 df <- data.frame(plotVec = plotVec, nthrows = 1:nThrows)
 ggplot(df)+geom_point(aes(x = nthrows, y = plotVec),
                       colour= "dark blue", size = 1) +
   xlab("No. of throws") +
   ylab("delta")
 
 cat("No. of throws to convergence: ", nThrows)
 if(delta==0)
 {
   cat("Converged exactly to true mean (7).[eps = ", eps)
 } else {
   options(digits = 8)
   cat("Converged to ",delta, "from true mean (7). eps = ", eps )
   options(digits = 7)
 }
 
 #------------------------------------------------------------------------------------


 #----L11Loops4, eval = EVAL----------------------------------------------
 ##Repeat loops
 #-----------------------------------------------------------------------------------
 #Generate an unusual sequence
 kk <- 1
 ii <- 1
 jj <- 4
 loops <- 0
 repeat{                                                                  #1.
   loops <- loops + 1
   kk <- kk + ii
   if (kk%%2 == 0 | kk%%5 == 0) next                                      #2.
   kk <- kk + jj
   cat(c('loops, kk', loops, kk), sep = c("     ","   ",'\n' ))
   if(kk > 50)break                                                       #3.
 }
 
 #----------------------------------------------------------------------------------


 #----L11if1, eval = EVAL-------------------------------------------------
 x <- 1
 if (x == 1){
   print(x)
 }


 #----L11if2, eval = EVAL-------------------------------------------------
 x <- 1
 if (x == 1){
   print("Disaster")
 } else {
   print(x)
 }


 #----L11if3, eval = EVAL-------------------------------------------------
 x <- 1
 if (x == 1) {
   print("Disaster")
 } else {
   print(x)
 }
   
  
  
 #  ----L11if4, eval = EVAL-------------------------------------------------
   x <- 1
   if (x == 1) {
     print("Disaster")
   } else if (x > 10){
      print(x)
   } else if (x < 10) {
     print("Have we got a logical problem here?")
   }
  
  
#   ----L11if5, eval = EVAL-------------------------------------------------
   x <- 10
   if (x == 1) {
     print("Disaster")
   } else if (x > 10){
      print(x)
   } else if (x < 10) {
     print("Have we got a logical problem here?")
   } else {
     print(" x is exactly equal to 10!")
   }
  
  
 #  ----L11switch, eval = EVAL----------------------------------------------
   x <- 2
   switch(x, 2+2, mean(1:10), rnorm(5))
   switch(2, 2+2, mean(1:10), rnorm(5))
   switch(6, 2+2, mean(1:10), rnorm(5))
   
  
  
 #  ----L11switchdefault, eval = EVAL---------------------------------------
    x <- 3
   if (x < 4 ){
     switch(x, 2+2, mean(1:10), rnorm(5))
   } else {
     print('Default value for switch')
   }
  
    