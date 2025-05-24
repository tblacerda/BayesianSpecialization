library(tidyverse)
library(rjags)
library(COUNT)

# load data
dat <- read.csv("callers.csv")
head(dat)
str(dat)
hist(dat$calls, breaks=20)
# fit a Poisson model

library(rjags)



# Modelo JAGS (similar ao seu exemplo, mas com suas variáveis)
mod_string <- " model {
  for (i in 1:length(calls)) {
    calls[i] ~ dpois(lam[i])
    log(lam[i]) <- int + b_days*days_active[i] + b_group*isgroup2[i] + b_age*age[i]
  }

  # Priors (distribuições a priori)
  int ~ dnorm(0, 1.0/100)       # Intercepto
  b_days ~ dnorm(0, 1.0/100)    # Coef. days_active
  b_group ~ dnorm(0, 1.0/100)   # Coef. isgroup2
  b_age ~ dnorm(0, 1.0/100)     # Coef. age
} "

# Preparar os dados para JAGS
data_jags <- list(
  calls = dat$calls,
  days_active = dat$days_active,
  isgroup2 = dat$isgroup2,
  age = dat$age
)

# Inicializar o modelo
mod <- jags.model(
  textConnection(mod_string),
  data = data_jags,
  n.chains = 3  # Número de cadeias MCMC
)

# Amostrar da posterior
params <- c("int", "b_days", "b_group", "b_age")
update(mod, 1000)  # Burn-in
samples <- coda.samples(
  mod,
  variable.names = params,
  n.iter = 5000
)

# Resumo das estimativas
summary(samples)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergence diagnostics
plot(mod_sim)
gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)
## compute DIC
dic = dic.samples(mod, n.iter=1e3)
dic
# plotar os resultados
library(bayesplot)
library(ggplot2)

# Plotar os resultados
mcmc_trace(mod_sim, facet_args = list(ncol = 2)) +
  ggtitle("Traceplot dos parâmetros do modelo") +
  theme_minimal()


# respondendo a pergunta:
# use your posterior samples to simulate predictions of the number of calls by 
# a new 29 year old customer from Group 2 whose account is active for 30 days.
# What is the probability that this new customer calls at least three times during
# this period? Round your answer to two decimal places.

# Converter as amostras em matriz
samples_mat <- as.matrix(samples)
head(samples_mat)  # Verifique os parâmetros: int, b_days, b_group, b_age

# Dados do novo cliente
new_age <- 29
new_group <- 1   # Grupo 2 é codificado como 1 (assumindo isgroup2 = 1 para Group 2)
new_days <- 30

# Calcular log(lam) para cada amostra posterior
log_lam_new <- samples_mat[, "int"] + 
  samples_mat[, "b_days"] * new_days + 
  samples_mat[, "b_group"] * new_group + 
  samples_mat[, "b_age"] * new_age

# Calcular lam (inverso do link log)
lam_new <- exp(log_lam_new)

set.seed(123)  # Para reprodutibilidade
calls_new <- rpois(n = length(lam_new), lambda = lam_new)

prob_at_least_3 <- mean(calls_new >= 3)
round(prob_at_least_3, 2)  # Arredonde para 2 casas decimais