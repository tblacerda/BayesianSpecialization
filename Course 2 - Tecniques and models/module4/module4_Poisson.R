library("COUNT")
data("badhealth")

####
?badhealth
data(badhealth)
glmbadp <- glm(numvisit ~ badh + age, family=poisson, data=badhealth)
summary(glmbadp)
exp(coef(glmbadp))
library(MASS)
glmbadnb <- glm.nb(numvisit ~ badh + age, data=badhealth)
summary(glmbadnb)
exp(coef(glmbadnb))
head(badhealth)
####

any(is.na(badhealth))

hist(badhealth$numvisit, breaks = 20,
     main = "Histogram of Number of Visits",
     xlab = "Number of Visits",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")

#há algum valor de visitas = 0 ? nao faz sentido ter
min(badhealth$numvisit)
sum(badhealth$numvisit == 0)

plot(jitter(log(numvisit)) ~jitter(age), data = badhealth,
     subset=badh==0 & numvisit>0, xlab="Age", ylab = "Log(visits)")

points(jitter(log(numvisit)) ~jitter(age), data = badhealth,
     subset=badh==1 & numvisit>0, xlab="Age", ylab = "Log(visits)", col = "red")

####
library(rjags)
library(coda)

mod_string = "model {
  for (i in 1:length(numvisit)){
    numvisit[i] ~ dpois(lam[i])
    log(lam[i]) <- int + b_badh*badh[i] + b_age*age[i]+ b_intx*age[i]*badh[i]
  }
  int ~ dnorm(0, 1/1e6)
  b_badh ~ dnorm(0, 1/1e4)
  b_age ~ dnorm(0, 1/1e4)
  b_intx ~ dnorm(0, 1/1e4)
}"

set.seed(102)

data_jags = as.list(badhealth)
str(data_jags)
params = c("int", "b_badh", "b_age", "b_intx")
mod = jags.model(textConnection(mod_string),
                 data = data_jags,
                 n.chains = 3,
                 n.adapt = 1000,)

update(mod, 1000)

mod_sim = coda.samples(model = mod,
                       variable.names = params,
                       n.iter = 10000,
                     )

### combina todas as 3 cadeias em uma so. um append
mod_csim = as.mcmc(do.call(rbind, mod_sim))

### CONVERGENCE DIAGNOSTICS
plot(mod_sim)
gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

### COMPUTE DIC
dic=dic.samples(mod, n.iter = 10000)
dic
5634
### RESIDUALS
X= as.matrix(badhealth[,-1])
X = cbind(X, with(badhealth, badh*age))
tail(X)

pmed_coef = apply(mod_csim, 2, median)
pmed_coef

llam_hat = pmed_coef["int"] + X %*% pmed_coef[c("b_badh", "b_age", "b_intx")]
lam_hat = exp(llam_hat)
resid = badhealth$numvisit - lam_hat
plot(resid)

summary(mod_sim)





mod_string = "model {
  for (i in 1:length(numvisit)){
    numvisit[i] ~ dpois(lam[i])
    log(lam[i]) <- int + b_badh*badh[i] + b_age*age[i]
  }
  int ~ dnorm(0, 1/1e6)
  b_badh ~ dnorm(0, 1/1e4)
  b_age ~ dnorm(0, 1/1e4)
}"

set.seed(102)

data_jags = as.list(badhealth)
str(data_jags)
params = c("int", "b_badh", "b_age")
mod = jags.model(textConnection(mod_string),
                 data = data_jags,
                 n.chains = 3,
                 n.adapt = 1000,)

update(mod, 1000)

mod_sim = coda.samples(model = mod,
                       variable.names = params,
                       n.iter = 10000,
)

### combina todas as 3 cadeias em uma so. um append
mod_csim = as.mcmc(do.call(rbind, mod_sim))

### COMPUTE DIC
dic=dic.samples(mod, n.iter = 10000)
dic
