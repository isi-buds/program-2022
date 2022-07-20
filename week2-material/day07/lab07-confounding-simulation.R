##
#####
##### ISI BUDS - 2022 : Simulation study to illustrate confounding
#####
##
#####
##### Simulation study to understand 
#####   1. Sampling distribution of an estimator
#####   2. Adjustment for confounding
#####   3. Adjustment for precision 
##### in linear regression models
#####
##
#####
##### Part a
#####
##
set.seed(12345)         ## Set a random seed to recreate results
##
##### Consider simulating data for one "study"
##
n <- 100                ## Sample size of 100
x1 <- rnorm(n,0,1)      ## Simulate a predictor of interest
beta0 <- 2              ## True intercept in the model
beta1 <- 1              ## True slope in the model
y <- beta0 + beta1*x1   ## Generate response without error
plot(x1,y)              ## Look at relationship between generated x1 and y
eps <- rnorm(n,0,1)     ## Error term
y <- y + eps            ## Add on the error term
plot(x1,y)              ## Observed relationship between x1 and y

##
##### Now use linear regression to estimate the relationship between x and y
#####   Note: Look at ??lm for more details on the lm() function
##
lmfit <- lm( y ~ x1 )
summary( lmfit )        ## How good are our true estimates?

##
##### True intercept is 2, and we estimate it to be 1.994
##### True slope is 1, and we estimate it to be 1.155
##
##### Theory also tells us that if we repeated this "study" over and over
##### again many times:
#####     1.  On average, these estimates would be equal to the true values
#####     2.  The standard error (i.e. standard deviation) of the estimates would 
#####         be approximately .100
#####
##


##
#####
##### Part b
#####
##
##
##### Let's check the theory and see if this is true. We will do this by
##### simulating many studies (1000), saving the estimates from each,
##### and looking at the sampling distribution
##
##### To do this, we can put the above in a for() loop!
##
rslt <- matrix(NA,1000,2) ## Create a matrix to store the estimates from each study (1000 x 2)
n <- 100                ## Sample size of 100
x1 <- rnorm(n,0,1)      ## Simulate a predictor of interest
beta0 <- 2              ## True intercept in the model
beta1 <- 1              ## True slope in the model
y <- beta0 + beta1*x1   ## Generate response without error
for( i in 1:1000){        ## i is the counter for each simulated study
  eps <- rnorm(n,0,1)
  y <- beta0 + beta1*x1 + eps
  lmfit <- lm( y ~ x1 )
  rslt[i,] <- lmfit$coefficients ## This will extract the estimates from the fit
}


##
#####
##### Part c
#####
##
##
##### Now let's look at the sampling distribution of the estimates
##
par(mfrow=c(1,2))
hist(rslt[,1], main="Estimate of Intercept")
hist(rslt[,2], main="Estimate of Slope")
par(mfrow=c(1,1))

## The above are the realized estimates if we had the ability to do the study many times
## It looks like they are centered at the true value... Let's check
mean(rslt[,1])
mean(rslt[,2])

##  Very close!  The estimates are "unbiased".

##  What about the standard deviation? Theory said they should both be about .10
sd(rslt[,1])
sd(rslt[,2])

## Again, very close!  This means that with a single sample, theory allows us
## to draw valid inference on the population parameters (the "truth")



##
#####
##### Part d
#####
##
##
##### Now let's look at adjusting for another variable
##### We will generate the data in the same way, but with 
##### the X's from a bivariate normal to allow for correlation between them
##
set.seed(12345)         ## Set a random seed to recreate results
library(MASS)           ## Load the MASS library to generate bivariate normal data
##
##### Consider simulating data for one "study"
##
n <- 100                ## Sample size of 100
X <- mvrnorm(n=100,     ## Simulate covariates with correlation .6
             mu=c(0, 0),
             Sigma=matrix(c(1, .6, .6, 1), ncol=2))    
x1 <- X[,1]
x2 <- X[,2]
beta0 <- 2              ## True intercept in the model
beta1 <- 1              ## True slope in the model for x1
beta2 <- 3              ## True slope in the model for x2
y <- beta0 + beta1*x1 + beta2*x2   ## Generate response without error
eps <- rnorm(n,0,1)     ## Error term
y <- y + eps            ## Add on the error term

##
##### Now use linear regression to estimate the relationship between x and y
##### first adjusting for x2
##
lmfit.adj <- lm( y ~ x1 + x2 )
summary( lmfit.adj )        ## How good are our true estimates?

## Again, these are very close...What if we fail to adjust for x2 (a confounder)?
lmfit.unadj <- lm( y ~ x1 )
summary( lmfit.unadj )        ## How good are our true estimates?

## Truth is 1 in the adjusted model, but estimate is 3.16!  This is the impact of
##  failing to adjust for the confounder...


##
#####
##### Part e
#####
##
##
######  Now let's simulate these data over a 1000 experiments and
######  see if these results are consistent across them...
##
rslt.unadj <- matrix(NA,1000,2) ## Create a matrix to store the estimates from each study (1000 x 2)
rslt.adj <- matrix(NA,1000,3) ## Create a matrix to store the estimates from each study (1000 x 2)
n <- 100                ## Sample size of 100
X <- mvrnorm(n=100,     ## Simulate covariates with correlation .6
             mu=c(0, 0),
             Sigma=matrix(c(1, .6, .6, 1), ncol=2))    
x1 <- X[,1]
x2 <- X[,2]
beta0 <- 2                        ## True intercept in the model
beta1 <- 1                        ## True slope in the model for x1
beta2 <- 3                        ## True slope in the model for x2
for( i in 1:1000){                ## i is the counter for each simulated study
  eps <- rnorm(n,0,1)
  y <- beta0 + beta1*x1 + +beta2*x2 + eps
  lmfit.unadj <- lm( y ~ x1 )
  lmfit.adj <- lm( y ~ x1 + x2 )
  rslt.unadj[i,] <- lmfit.unadj$coefficients ## This will extract the estimates from the fit
  rslt.adj[i,] <- lmfit.adj$coefficients ## This will extract the estimates from the fit
  }


##
#####
##### Part f
#####
##
##
##### Now let's look at the sampling distribution of the estimates
##
par(mfrow=c(1,2))
hist(rslt.unadj[,2], main="Estimate of Slope (unadjusted for X_2) ")
hist(rslt.adj[,2], main="Estimate of Slope (adjusted for X_2)")
par(mfrow=c(1,1))

##
##### Unadjusted estimates are centered around 3, while adjusted are at 1 (the truth)
##

##
#####
##### Part g
#####
##
##  Changing the association between X_2 and Y, and the 
##  correlation between X_1 and X_2 will have the following effects:
##    1. As the value of \beta_2 increases in magnitude, the bias will increase
##    2. As the value of the correlation goes to 1 in absolute value, the bias will increase
##

##
#####
##### Part h
#####
##
##### Now suppose the correlation between X_1 and X_2 is zero (independent)
##
set.seed(12345)         ## Set a random seed to recreate results
library(MASS)           ## Load the MASS library to generate bivariate normal data
##
##### Consider simulating data for one "study"
##
n <- 100                ## Sample size of 100
X <- mvrnorm(n=100,     ## Simulate covariates with correlation .6
             mu=c(0, 0),
             Sigma=matrix(c(1, 0, 0, 1), ncol=2))    
x1 <- X[,1]
x2 <- X[,2]
beta0 <- 2              ## True intercept in the model
beta1 <- 1              ## True slope in the model for x1
beta2 <- 3              ## True slope in the model for x2
y <- beta0 + beta1*x1 + beta2*x2   ## Generate response without error
eps <- rnorm(n,0,1)     ## Error term
y <- y + eps            ## Add on the error term

##
##### Now use linear regression to estimate the relationship between x and y
##### first adjusting for x2
##
lmfit.adj <- lm( y ~ x1 + x2 )
summary( lmfit.adj )        ## How good are our true estimates?

## Again, these are very close...What if we fail to adjust for x2 (a confounder)?
lmfit.unadj <- lm( y ~ x1 )
summary( lmfit.unadj )        ## How good are our true estimates?

## 
#####
##### Conclusions from this:
#####   1.  Both the unadjusted and adjusted models are unbiased
#####   2.  The standard error for \hat{\beta}_1 is smaller in the adjusted model. Why?
##
