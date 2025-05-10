library(readr)
library(rjags)

Leinhardt <- read_csv("Leinhardt.csv") # Mais rápido e com melhor formatação
head(Leinhardt)
Leinhardt <- as.data.frame(Leinhardt)
str(Leinhardt)
Leinhardt$region <- as.factor(Leinhardt$region)
Leinhardt$oil <- as.factor(Leinhardt$oil)

# We’ll start with a simple linear regression model that relates infant mortality 
# to per capita income.

plot(infant ~ income, data=Leinhardt)
hist(Leinhardt$infant)
hist(Leinhardt$income)  

Leinhardt$loginfant = log(Leinhardt$infant)
Leinhardt$logincome = log(Leinhardt$income)

plot(loginfant ~ logincome, data=Leinhardt)

# Since infant mortality and per capita income are positive and right-skewed 
# quantities, we consider modeling them on the logarithmic scale. 
# A linear model appears much more appropriate on this scale.
# Modeling 
# The reference Bayesian analysis (with a noninformative prior) is 
# available directly in R.

lmod = lm(loginfant ~ logincome, data=Leinhardt)
summary(lmod)

# Leson 7.3
# Model in JAGS
# Now we’ll fit this model in JAGS. A few countries have missing values, 
# and for simplicity, we will omit those.

dat = na.omit(Leinhardt)
library("rjags")

## Loading required package: coda
## Warning: package 'coda' was built under R version 3.3.2
## Linked to JAGS 4.2.0
## Loaded modules: basemod,bugs
mod1_string = " model {
    for (i in 1:n) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*log_income[i] 
    }
    
    for (i in 1:2) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(5/2.0, 5*10.0/2.0)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

set.seed(72)
data1_jags = list(y=dat$loginfant, n=nrow(dat), 
                  log_income=dat$logincome)
params1 = c("b", "sig")
inits1 = function() {
  inits = list("b"=rnorm(2,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}
mod1 = jags.model(textConnection(mod1_string), data=data1_jags, inits=inits1, n.chains=3)
update(mod1, 1000) # burn-in
mod1_sim = coda.samples(model=mod1,
                        variable.names=params1,
                        n.iter=5000)
mod1_csim = do.call(rbind, mod1_sim) # combine multiple chains

# Lesson 7.4
# MCMC convergence
# Before we check the inferences from the model, we should perform
# convergence diagnostics for our Markov chains.

plot(mod1_sim)
gelman.diag(mod1_sim)
autocorr.diag(mod1_sim)
autocorr.plot(mod1_sim)
effectiveSize(mod1_sim)
summary(mod1_sim)

# Don’t forget that these results are for a regression model relating 
# the logarithm of infant mortality to the logarithm of income.

# Residual checks
# Checking residuals (the difference between the response and the
# model’s prediction for that value) is important with linear models 
# since residuals can reveal violations of the assumptions we made to
# specify the model. In particular, we are looking for any sign that the
# model is not linear, normally distributed, or that the observations are 
# not independent (conditional on covariates).

# First, let’s look at what would have happened if we fit the 
# reference linear model to the un-transformed variables.

lmod0 = lm(infant ~ income, data=Leinhardt)
plot(resid(lmod0)) # to check independence (looks okay)
plot(predict(lmod0), resid(lmod0)) # to check for linearity, constant variance (looks bad)
qqnorm(resid(lmod0)) # to check Normality assumption (we want this to be a straight line)

# Now let’s return to our model fit to the log-transformed variables.
# In a Bayesian model, we have distributions for residuals, but we’ll 
# simplify and look only at the residuals evaluated at the 
# posterior mean of the parameters.
X = cbind(rep(1.0, data1_jags$n), data1_jags$log_income)
head(X)
(pm_params1 = colMeans(mod1_csim)) # posterior mean

yhat1 = drop(X %*% pm_params1[1:2])
resid1 = data1_jags$y - yhat1
plot(resid1) # against data index

plot(yhat1, resid1) # against predicted values

qqnorm(resid1) # checking normality of residuals
plot(predict(lmod), resid(lmod)) # to compare with reference linear model

rownames(dat)[order(resid1, decreasing=TRUE)[1:5]] # which countries have the largest positive residuals?



###############################################################################

#Lesson 7.5
#In the previous segment, we saw two outliers in the model relating the logarithm
#of infant mortality to the logarithm of income. Here we will discuss options for
#when we conclude that these outliers belong in the data set.

#Additional covariates
#The first approach is to look for additional covariates that may be able to 
#explain the outliers. For example, there could be a number of variables that 
#provide information about infant mortality above and beyond what income provides.

#Looking back at our data, there are two variables we haven’t used yet: region
#and oil. The oil variable indicates oil-exporting countries. Both Saudi Arabia
#and Libya are oil-exporting countries, so perhaps this might explain part of 
#the anomaly.


library("rjags")

mod2_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*log_income[i] + b[3]*is_oil[i]
    }
    
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(5/2.0, 5*10.0/2.0)
    sig = sqrt( 1.0 / prec )
} "


set.seed(73)
data2_jags = list(y=dat$loginfant, log_income=dat$logincome,
                  is_oil=as.numeric(dat$oil=="yes"))
data2_jags$is_oil

params2 = c("b", "sig")

inits2 = function() {
  inits = list("b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod2 = jags.model(textConnection(mod2_string), data=data2_jags, inits=inits2, n.chains=3)
update(mod2, 1e3) # burn-in

mod2_sim = coda.samples(model=mod2,
                        variable.names=params2,
                        n.iter=5e3)

mod2_csim = as.mcmc(do.call(rbind, mod2_sim)) # combine multiple chains

plot(mod2_sim)

gelman.diag(mod2_sim)
autocorr.diag(mod2_sim)

autocorr.plot(mod2_sim)


effectiveSize(mod2_sim)

summary(mod2_sim)

