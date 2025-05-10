##

B0 <- 1.5
B1 <- -0.3
B2 <- 1
x1 <- 0.8
x2 <- 1.2

LogE <- B0 + B1 * x1 + B2 * x2
LogE
exp(LogE)

###
# parâmetros
lambda <- 15 * 2   # 15 chamadas/hora × 2 horas = 30

# probabilidade de até 21 chamadas
prob <- ppois(21, lambda)

# mostrar com duas casas decimais
round(prob, 2)

library(tidyverse)
dat <- read.csv(file="callers.csv", header=TRUE)
head(dat)

dat <- dat %>% 
  mutate(isgroup2 = as.factor(isgroup2))

str(dat)

dat %>% 
  ggplot(aes(x=isgroup2, y=calls))+
           geom_boxplot() +
    labs(title="Boxplot of Calls by Group",
         x="Group",
         y="Number of Calls")


dat %>% 
  ggplot(aes(x=isgroup2, y=calls/days_active))+
  geom_boxplot() +
  labs(title="Boxplot of Calls by Group",
       x="Group",
       y="Number of Calls")



library(rjags)
library(coda)

mod_string = "model {
	for (i in 1:length(calls)) {
		calls[i] ~ dpois( days_active[i] * lam[i] )
		log(lam[i]) = b0 + b1*age[i] + b2*isgroup2[i]
	}
  b0 ~ dnorm(0, 1/100)
  b1 ~ dnorm(0, 1/1e4)
  b2 ~ dnorm(0, 1/1e4)
}"

set.seed(102)

data_jags = as.list(dat)
str(data_jags)
params = c("b0", "b1", "b2")
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
mod_csim

# extrai o vetor de amostras de b2
b2_samples <- as.matrix(mod_csim)[, "b2"]

# calcula P(b2 > 0) como proporção de amostras positivas
prob_b2_pos <- mean(b2_samples > 0)

# imprime com 3 casas decimais
round(prob_b2_pos, 3)