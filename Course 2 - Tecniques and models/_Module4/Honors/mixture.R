library(tidyverse)
library(rjags)

dat = read.csv("mixture.csv", header = FALSE)

head(dat)
y = dat$V1
hist(y)
plot(density(y))

mod_string =" model {
  for (i in 1:length(y)) {
    y[i] ~ dnorm(mu[z[i]], prec)
    z[i] ~ dcat(omega)
  }
  mu[1] ~ dnorm(-1, 1/100)
  mu[2] ~ dnorm (1 , 1/100) T(mu[1],)
  prec ~ dgamma(1/2,  1/2)
  sig = sqrt(1.0/prec)
  
  omega ~ ddirch(c(1,1))
  
}"

set.seed(11)
data_jags = list(y=y)

params = c("mu", "sig", "omega", "z[1]", "z[31]", "z[49]", "z[6]")

mod <- jags.model(textConnection(mod_string),
                  data = data_jags,
                  n.chains = 3,
                  n.adapt = 1000,
                  )

update(mod, 1000)

mod_sim <- coda.samples(model = mod,
                        variable.names = params,
                        n.iter = 10000,
                        )

mod_csim <- as.mcmc(do.call(rbind, mod_sim))


plot(mod_csim, ask = TRUE)

summary(mod_csim)

par(mfrow = c(2, 2))
densplot(mod_csim[,c("z[1]", "z[31]", "z[49]", "z[6]")], ask = FALSE)
