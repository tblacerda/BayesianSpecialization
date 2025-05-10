library(tidyverse)
dat <- read.csv(file="pctgrowth.csv", header=TRUE)
head(dat)
str(dat)
boxplot(y ~grp, data=dat)
means_anova = tapply(dat$y, INDEX=dat$grp, FUN=mean)
means_anova


plot(means_anova)
points(means_theta, col="red") ## where means_theta are the posterior point estimates for the industry means.


# instale se necessário:
# install.packages(c("rjags","coda"))

library(rjags)
library(coda)

# 2.1. Leia seus dados
y    <- dat$y
grp  <- dat$grp
N    <- length(y)
J    <- length(unique(grp))

# 2.2. Prepare lista para JAGS
data_jags <- list(
  y    = y,
  grp  = grp,
  N    = N,
  J    = J
)

# 2.3. Quais parâmetros vamos monitorar?
params <- c("theta", "mu", "sigma", "tau_between")

# 2.4. Inicialização (opcional)
inits <- function() {
  list(
    mu         = mean(y),
    theta      = tapply(y, grp, mean),
    tau.y      = 1 / var(y),
    tau.theta  = 1 / var(tapply(y, grp, mean))
  )
}

mod_string <- "
model {
  # 1. Likelihood
  for (i in 1:N) {
    y[i] ~ dnorm(theta[grp[i]], tau.y)
  }

  # 2. Priors on group means (hierarquia)
  for (j in 1:J) {
    theta[j] ~ dnorm(mu, tau.theta)
  }

  # 3. Hyperpriors
  mu        ~ dnorm(0.0,    1.0E-4)    # N(0, 10^2)
  tau.theta ~ dgamma(0.01,  0.01)      # equivalente a half-Cauchy-ish
  tau.y     ~ dgamma(0.01,  0.01)

  # 4. Convert precisions to sds (opcional, para monitorar)
  sigma       <- 1 / sqrt(tau.y)
  tau_between <- 1 / sqrt(tau.theta)
}

"

# 2.5. Compile o modelo
m <- jags.model(
  textConnection(mod_string),
  data    = data_jags,
  inits   = inits,
  n.chains= 3,
  n.adapt = 1000
)

# 2.6. Burn‐in
update(m, n.iter = 5000)

# 2.7. Amostragem
samps <- coda.samples(
  model     = m,
  variable.names = params,
  n.iter    = 10000,
  thin      = 10
)


# combine cadeias
samps_combined <- as.mcmc(do.call(rbind, samps))

# extrai apenas as thetas
theta_draws <- samps_combined[, grep("^theta\\[", colnames(samps_combined))]

# estime a média a posteriori de cada theta[j]
post_means <- colMeans(theta_draws)

# formate num data.frame para visualização
results <- data.frame(
  industry = 1:J,
  post_mean = post_means
)

print(results)
means_anova = tapply(dat$y, INDEX=dat$grp, FUN=mean)
means_anova

plot(means_anova)
points(results, col="red")
