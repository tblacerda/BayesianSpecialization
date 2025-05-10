# 1. Specify the model
# In this step, we give JAGS the hierarchical structure of the model, 
# assigning distributions to the data (the likelihood) and parameters (priors).
# The syntax for this step is very similar to R, but there are some key differences.

library("rjags")
mod_string = " model {
for (i in 1:n) {
y[i] ~ dnorm(mu, 10/sig2)
}
mu ~ dt(0.0, 1.0/1.0, 1.0) # location, inverse scale, degrees of freedom
sig2 =0.5
} "

# 2. Set up the model
set.seed(50)
y = c(-0.2, -1.5, -5.3, 0.3, -0.8, -2.2)
n = length(y)

data_jags = list(y=y, n=n)
params = c("mu")

inits = function() {
  inits = list("mu"=1.0)
} # optional (and fixed)

mod = jags.model(textConnection(mod_string), data=data_jags, inits=inits)

#3. Run the MCMC sampler
update(mod, 500) # burn-in

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=1000)

#4. Post processing
summary(mod_sim)

library("coda")
plot(mod_sim)
