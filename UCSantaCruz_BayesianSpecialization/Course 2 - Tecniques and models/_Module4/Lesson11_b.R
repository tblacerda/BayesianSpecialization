library("MASS")
data("OME")

dat = subset(OME, OME != "N/A")
dat$OME = factor(dat$OME) # relabel OME
dat$ID = as.numeric(factor(dat$ID)) # relabel ID so there are no gaps in numbers (they now go from 1 to 63)

## Original reference model and covariate matrix
mod_glm = glm(Correct/Trials ~ ID + Age + OME + Loud + Noise, data=dat, weights=Trials, family="binomial")
X = model.matrix(mod_glm)[,-1]

## Original model (that needs to be extended)
mod_string = " model {
	for (i in 1:length(y)) {
		y[i] ~ dbin(phi[i], n[i])
		logit(phi[i]) = alpha[ID[i]] + b0  + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
	}
	
	b0 ~ dnorm(0.0, 1.0/5.0^2)
	for (j in 1:4) {
		b[j] ~ dnorm(0.0, 1.0/4.0^2)
	}

  for (k in 1:63) {
    alpha[k] ~ dnorm(mu, tau2^-2)
    }

  mu ~ dnorm(0.0, 1.0/100)
  tau2 ~ dgamma(0.5, 0.5)
  tau <- sqrt(tau2)

} "
data_jags
data_jags = as.list(as.data.frame(X))
data_jags$y = dat$Correct
data_jags$n = dat$Trials
data_jags$ID = dat$ID

library(rjags)
jags = jags.model(textConnection(mod_string), data = data_jags, n.chains = 3, n.adapt = 1000)
update(jags, 1000) # burn-in
library(coda)
# Agora use `coda.samples()` diretamente:
coda_samples <- coda.samples(jags, variable.names = c("b0", "b", "mu", "tau"), n.iter = 1000)



# diagnostico de convergencia
traceplot(coda_samples, ask=TRUE)

# Isso já é um mcmc.list:
summary(coda_samples)
plot(coda_samples)
HPDinterval(coda_samples)
raftery.diag(coda_samples) #para saber a quantidade de samples 

gelman.diag(coda_samples)

autocorr.plot(coda_samples)
