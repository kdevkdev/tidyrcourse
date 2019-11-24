tstart <- Sys.time()
m <- 100000  # samples
n <- 1000    # size of sample
mu <- 2 
sdev <- 2

# Old fashioned programmer's way (but still just as fast!)
df <- matrix(, nrow = n, ncol = m)
for(i in 1:m) {
 df[, i] = rnorm(n, mean = mu, sd = sdev)
}

# The 'R' way
##normData <- rnorm(m*n, mu, sdev)
#df <- matrix(normData, nrow = n, ncol = m)


df <- as.data.frame(df)
sampleMean <- sapply(df, mean)
samplesd <- sapply(df, sd)
cor1 <- cor(sampleMean, samplesd)
dfPlot <- data.frame(Mean=sampleMean, sd = samplesd)
ALPHA <- min(1, 1000/m)
ggplot(dfPlot) + geom_point(aes(x = Mean, y=sd), alpha = ALPHA)
a <- 2.05
b <- 1.95
PsdLTa <- sum(samplesd<a)/m
PmLTb <- sum(sampleMean<b)/m
prod1 <- PsdLTa*PmLTb
rhs <- sum(samplesd<a & sampleMean<b)/m
prod1
rhs
cat("m ,n ,mu, sdev:   ", m,n,mu,sdev, "\n")
cat("cor1              ", cor1, "\n")
cat("Product, Joint    ", prod1, rhs, "\n")
deltat <- Sys.time()- tstart
cat("Elapsed time:     ", deltat, " seconds")