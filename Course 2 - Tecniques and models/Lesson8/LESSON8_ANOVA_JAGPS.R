data("PlantGrowth")
?PlantGrowth


head(PlantGrowth)
str(PlantGrowth)

boxplot(weight ~ group, data = PlantGrowth, 
        xlab = "Group", ylab = "Weight", 
        main = "Boxplot of Weight by Group",
        col = c("red", "green", "blue"))

lmod = lm(weight ~ group, data = PlantGrowth)
summary(lmod)
anova(lmod)

mod_string = "model {
  for (i in 1:length(y)) {
    y[i] ~ dnorm(mu[grp[i]], prec)
  }
  
  for (j in 1:3) {
    mu[j] ~ dnorm(0, 1/1.0e6)
  }
  prec ~ dgamma(5/2.0, 5*1.0/2)
  sig = sqrt (1 / prec)
    
}"

set.seed(82)
library(rjags)
data_jags = list(y = PlantGrowth$weight, 
           grp = as.numeric(PlantGrowth$group))

params = c("mu", "sig")

inits = function() {
  list("mu"=rnorm(3, 0, 100), "prec" = rgamma(1, 1, 1))
}

mod = jags.model(textConnection(mod_string),
                 data = data_jags,
                 inits = inits,
                 n.chains = 3,
                 n.adapt = 1000
)
update(mod, 1000)


mod_sim = coda.samples(model = mod,
                       variable.names = params,
                       n.iter = 10000,
                       thin = 10
)

mod_csim = as.mcmc(do.call(rbind, mod_sim))

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
effectiveSize(mod_sim)

pm_params = (colMeans(mod_csim))

pm_params

data_jags$grp
yhat = pm_params[1:3][data_jags$grp]
yhat
resid = data_jags$y - yhat
plot(resid)
plot(resid ~ yhat, 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")


summary(mod_sim)
HPDinterval(mod_csim, prob = 0.95)

mean(mod_csim[,3] > mod_csim[,1]) #explain this
mean(mod_csim[,3] > 1.1* mod_csim[,1])



library(coda)

# supondo que mod_sim seja seu mcmc.list de mu[1], mu[2], mu[3]
mat = do.call(rbind, mod_sim)       # empilha as cadeias
diff = mat[, "mu[3]"] - mat[, "mu[1]"]  # vetor de amostras de μ3−μ1

diff_mcmc = mcmc(diff)              # converte em objeto mcmc
hpd_diff = HPDinterval(diff_mcmc, prob = 0.95)
print(hpd_diff)


################################################################################

data("PlantGrowth")

mod_string2 = "
model {
  for (i in 1:length(y)) {
    y[i] ~ dnorm(mu[grp[i]], prec[grp[i]])
  }

  for (j in 1:3) {
    mu[j]   ~ dnorm(0,       1.0E-6)
    prec[j] ~ dgamma(2.5,    2.5)
    sig[j]  <- sqrt(1 / prec[j])
  }
}
"


set.seed(82)
library(rjags)
data_jags2 = list(y = PlantGrowth$weight, 
                 grp = as.numeric(PlantGrowth$group))

params2 = c("mu", "sig")



mod2 = jags.model(textConnection(mod_string2),
                 data = data_jags,
                 n.chains = 3,
                 n.adapt = 1000
)
update(mod2, 1000)


mod_sim2 = coda.samples(model = mod2,
                       variable.names = params2,
                       n.iter = 10000,
                       thin = 10
)

mod_csim2 = as.mcmc(do.call(rbind, mod_sim2))


dic.samples(mod2, n.iter=1e3)
dic.samples(mod, n.iter=1e3)



summary(mod2)
summary(mod_sim)
gelman.diag(mod_sim2)
autocorr.diag(mod_sim2)
effectiveSize(mod_sim2)

pm_params = (colMeans(mod_csim))
pm_params

#################################################################################

mod_cm = lm(weight ~ -1 + group, data=PlantGrowth)
summary(mod_cm)