# Reg_simul.r

rm(list=ls())

options(help_type="html")

source("c:/R/TGG_loessfit_func.r")
source("c:/R/TGG_modelresults_func.r")

#setwd("c:/data/CA_rainfall/")

library(car)        # for scatterplot.matrix() & av.plots() & levene.test
library(lattice)    # for densityplot()
library(nnet)       # for class.ind function
#library(Hmisc)      # for histbackback


#sink( "Reg_simul.out")   # write output to file

cat("Reg_simul.r         ", format(Sys.time(),"%A, %d %B %Y"),
    "\n===============================================================\n")

set.seed(15730023)

# set big N
N <- 10000

# simulate some data with p=5 covariates

x1 <- 5*rgamma(N, shape=1.0, rate=2)
summary(x1)
densityplot(~x1)
x1dens <- curve(dgamma(x, shape=1.0, rate=2) )

x2 <- rnorm(N, mean=1500, sd = 150)
summary(x2)
densityplot(~x2)

x3 <- runif(N, min = -7, max = 0)
summary(x3)
densityplot(~x3)

x4 <- 50*rgamma(N, shape=2.0, rate=1)
summary(x4)
densityplot(~x4)
x4dens <- curve(dgamma(x, shape=2.0, rate=1) )

x5 = x1*x2
summary(x5)
densityplot(~x5)



# create response variable mean function
b0 = 50; b1 = 2; b2 = .01; b3 = 1; b4 = .01; b5 = .0001

mu.y <- b0 + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5
summary(mu.y)
densityplot(mu.y)

# create some normally distributed errors
eps <- rnorm(N, mean = 0, sd = 0.20*diff(range(mu.y)) )

# generate the response variable
y <- mu.y + eps
summary(y)
densityplot(y)

################################################################################
oldpar <- par(mfrow=c(3,2), xaxs = 'r', 
          oma = c(3,1,2,0), mar = c(4, 4, 1, 1))
plot(density(x1), main="", ylab="density of x1")
plot(density(x2), main="", ylab="density of x2")
plot(density(x3), main="", ylab="density of x3")
plot(density(x4), main="", ylab="density of x4")
plot(density(x5), main="", ylab="density of x5")
plot(density(y), main="", ylab="density of y")

mtext(format(Sys.time(),"%A, %d %B %Y"), side = 1, outer=TRUE, line=2, cex=0.65, 
      col="blue", adj=0.99) 
mtext(paste("Densities of explanatory and response variable values"), 
  side = 3, outer=TRUE, line=0, cex=1.20, col="blue", adj=0.50)
mtext(paste("Reg-Simul"), side = 1, 
      outer=TRUE, line=2, cex=0.650, col="blue", adj=0.0)  
par(oldpar)


################################################################################

# construct the full data frame

alldata <- data.frame(y, x1, x2, x3, x4, x5)
head(alldata)
str(alldata)

cor(alldata)

# select a small sample
sampledata <- alldata[sort(sample( 1:dim(alldata)[1], 30) ), ]
head(sampledata)
true.mlr1 <- lm(y ~ x1+x2+x3+x4+x5, data = sampledata)
true.mlr1.mr <- modelresults(true.mlr1, "Y")
round( cov2cor( true.mlr1.mr[["varcov"]] ), 2)

# select a medium sized sample
sampledata <- alldata[sort(sample( 1:dim(alldata)[1], 200) ), ]
head(sampledata)
true.mlr1 <- lm(y ~ x1+x2+x3+x4+x5, data = sampledata)
true.mlr1.mr <- modelresults(true.mlr1, "Y")
round( cov2cor( true.mlr1.mr[["varcov"]] ), 2)

# select a large sample
sampledata <- alldata[sort(sample( 1:dim(alldata)[1], 1000) ), ]
head(sampledata)
true.mlr1 <- lm(y ~ x1+x2+x3+x4+x5, data = sampledata)
true.mlr1.mr <- modelresults(true.mlr1, "Y")
round( cov2cor( true.mlr1.mr[["varcov"]] ), 2)

# fit model without x5
sampledata <- alldata[sort(sample( 1:dim(alldata)[1], 1000) ), ]
#head(sampledata)
x5omit.mlr1 <- lm(y ~ x1+x2+x3+x4, data = sampledata)
x5omit.mlr1.mr <- modelresults(x5omit.mlr1, "Y")
round( cov2cor( x5omit.mlr1.mr[["varcov"]] ), 2)

# fit model without x1 & x5
sampledata <- alldata[sort(sample( 1:dim(alldata)[1], 1000) ), ]
#head(sampledata)
x1x5omit.mlr1 <- lm(y ~ x2+x3+x4, data = sampledata)
x1x5omit.mlr1.mr <- modelresults(x1x5omit.mlr1, "Y")
round( cov2cor( x1x5omit.mlr1.mr[["varcov"]] ), 2)

# fit model without x3
sampledata <- alldata[sort(sample( 1:dim(alldata)[1], 1000) ), ]
#head(sampledata)
x3omit.mlr1 <- lm(y ~ x1+x2+x4+x5, data = sampledata)
x3omit.mlr1.mr <- modelresults(x3omit.mlr1, "Y")
round( cov2cor( x3omit.mlr1.mr[["varcov"]] ), 2)


