library(tidyverse)
library(rjags)
Leinhardt <- read.csv("Leinhardt.csv", header = TRUE)
head(Leinhardt)

dat <- na.omit(Leinhardt)

dat$logincome <- log(dat$income)
dat$loginfant <- log(dat$infant)
dat$region <- as.factor(dat$region)
str(dat)

mod_string <-" model {
  for (i in 1:length(y)) {
    y[i] ~ dnorm(mu[i], prec)
    mu[i] = a[region[i]] + b[1]*log_income[i] + b[2]*is_oil[i]
  }
  for (j in 1:max(region)) {
    a[j] ~ dnorm(a0, prec_a)
  } 
  a0 ~ dnorm(0, 1/1e6)
  prec_a ~ dgamma(1/2, 1*10/2)
  tau = sqrt(1/prec_a)
  
  for (j in 1:2){
    b[j] ~ dnorm(0, 1/1e6)
  }
  prec ~ dgamma(5/2, 5*10/2)
  sig = sqrt(1/prec)
}"

set.seed(116)
data_jags = list(y=dat$loginfant,
                 log_income=dat$logincome,
                 is_oil=as.numeric(dat$oil=="yes"),
                 region=as.numeric(dat$region))
data_jags
data_jags$is_oil
table(data_jags$is_oil, data_jags$region)

params <- c("a0", "a", "b", "sig", "tau")


mod <- jags.model(textConnection(mod_string),
                  dat = data_jags,
                  n.chains = 3)

update(mod, 1000)

mod_sim <- coda.samples(model = mod,
                        variable.names = params,
                        n.iter = 10000,
                        )

mod_csim = as.mcmc(do.call(rbind, mod_sim))

plot(mod_sim, ask = TRUE)

gelman.diag(mod_sim)

autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize((mod_sim))

dic.samples(mod, n.iter = 1e5)
summary(mod_sim)
