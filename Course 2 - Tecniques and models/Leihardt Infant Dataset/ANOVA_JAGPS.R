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

plot(mod_sim)

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

mean(mod_csim[,3] > mod_csim[,1])
mean(mod_csim[,3] > 1.1* mod_csim[,1])
