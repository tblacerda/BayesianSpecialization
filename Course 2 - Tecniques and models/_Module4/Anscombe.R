library(readr)
library(rjags)
library(car)  # load the 'car' package
install.package

data <- read_csv("Leinhardt.csv") # Mais rápido e com melhor formatação

head(data)  # look at the first few lines of the data
str(data)
#pairs(data)  # scatter plots for each pair of variables

#pairs(Leinhardt)  # scatter plots for each pair of variables)
# Converter colunas 'region' e 'oil' para fatores
data$region <- as.factor(data$region)
data$oil <- as.factor(data$oil)

# Ou selecionar colunas numéricas automaticamente
numeric_cols <- sapply(data, is.numeric)
pairs(data[, numeric_cols])

plot(data$income, data$infant, col = as.factor(data$region), pch = 19)
legend("topright", legend = levels(as.factor(data$region)), fill = 1:4)


plot(infant ~ income, data = data, col = as.factor(region), pch = 19,
     xlab = "Income", ylab = "Infant Mortality", main = "Infant Mortality vs Income")

hist(data$income, breaks = 20, main = "Histogram of Income", xlab = "Income", col = "lightblue")
hist(data$infant, breaks = 20, main = "Histogram of Infant Mortality", xlab = "Infant Mortality", col = "lightgreen")


data$log_income <- log(data$income)
data$log_infant <- log(data$infant)
data
plot(data$log_income, data$log_infant, col = as.factor(data$region), pch = 19,
     xlab = "Log(Income)", ylab = "Log(Infant Mortality)", main = "Log(Infant Mortality) vs Log(Income)")



data
lmod2 = lm(infant ~ income, data = data)

lmod2
### Modeling
lmod = lm(log_infant ~ log_income, data = data)
summary(lmod)
dat = na.omit(data)


mod1_string = "model {
  for (i in 1:n) {
  y[i] ~ dnorm(mu[i], prec)
    mu[i] = b[1] + b[2]*log_income[i]
  }
  
  for (j in 1:2) {
    b[j] ~ dnorm(0.0, 1.0/1.0e6)
  }
  
  prec ~ dgamma(5 / 2, 5 * 10 / 2)
  sig2 = 1 / prec
  sig = sqrt(sig2)
  }"

# Prepare data for JAGS
set.seed(72)
data1_jags = list(y = dat$log_infant,
                  log_income = dat$log_income,
                  n = nrow(dat)) # number of observations

params1= c("b","sig")

inits1 = function() {
  inits = list("b" = rnorm(2, 0, 100), "prec" = rgamma(1, 1, 1))
  
}

mod1 = jags.model(textConnection(mod1_string),
                data = data1_jags,
                inits = inits1,
                n.chains = 3,
                n.adapt = 1000)

update(mod1, 1000)

mod1_sim = coda.samples(model=mod1, variable.names = params1, n.iter=5e3)

mod1_csim = do.call(rbind, mod1_sim)
mod1_csim

# Model checking - CONVERGENCE

plot(mod1_sim)
gelman.diag(mod1_sim) #deve ser proximo a 1
autocorr.diag(mod1_sim) # valores altos nos LAGs iniciais é bom
effectiveSize(mod1_sim) # deve ser maior que 1000
summary(mod1_sim)


# Model checking - RESIDUALS
lmod0 = lm(infant ~ income, data = data)
plot(resid(lmod0)) # melhor é nao ter nenhum padrao
plot(predict(lmod0), resid(lmod0)) # melhor é nao ter nenhum padrao.
qqnorm(resid(lmod0))

X = cbind(rep(1, data_jags$n), data_jags$log_income)
pm_params1 = colMeans(mod1_csim)
y_hat1 =drop(X %*% pm_params1[1:2])
resid1 = data_jags$y - y_hat1
plot(y_hat1, resid1)
qqnorm(resid1)

head(rownames(dat)[order(resid1, decreasing = TRUE)])

