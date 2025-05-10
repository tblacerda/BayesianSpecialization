library("rjags")

dat = read.table(file="cookies.txt", header=TRUE)

mod_string =" model {
  for (i in 1:length(chips)) {
    chips[i] ~ dpois(lam[location[i]])
  }
  for (j in 1:max(location)) {
    lam[j] ~ dgamma(alpha, beta)
  }
  
  mu ~ dgamma(2, 1/5)
  sig ~ dexp(1)
  
  alpha <- mu^2 / sig^2
  beta <- mu / sig^2
}
"

set.seed(113)
data_jags <- as.list(dat)
params <- c("lam", "mu", "sig")

mod <- jags.model(textConnection(mod_string),
                  data = data_jags,
                  n.chains=3)
update(mod, 1e3)

mod_sim <- coda.samples(model=mod,
                        variable.names=params,
                        n.iter=1e4)
modc_csim <- as.mcmc(do.call(rbind, mod_sim))


plot(modc_csim, ask=FALSE)

dic = dic.samples(mod, n.iter=1e3)

summary(mod_sim)
