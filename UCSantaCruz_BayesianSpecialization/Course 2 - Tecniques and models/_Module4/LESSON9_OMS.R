library("MASS")
data("OME")
?OME # background on the data
head(OME)

any(is.na(OME)) # check for missing values
dat = subset(OME, OME != "N/A") # manually remove OME missing values identified with "N/A"
dat$OME = factor(dat$OME)
str(dat)

plot(dat$Age, dat$Correct / dat$Trials )
plot(dat$OME, dat$Correct / dat$Trials )
plot(dat$Loud, dat$Correct / dat$Trials )
plot(dat$Noise, dat$Correct / dat$Trials )


library(ggplot2)

ggplot(OME, aes(x = Loud, y = Correct / Trials, color = Noise)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ OME) +
  labs(title = "Proporção de acertos por nível de ruído e intensidade sonora",
       x = "Intensidade do estímulo (dB)",
       y = "Proporção de acertos")

mod_glm = glm(Correct/Trials ~ Age + OME + Loud + Noise, data=dat, weights=Trials, family="binomial")
summary(mod_glm)

plot(residuals(mod_glm, type="deviance"))
plot(fitted(mod_glm), dat$Correct/dat$Trials)

X = model.matrix(mod_glm)[,-1] # -1 removes the column of 1s for the intercept
head(X)



mod_string = " model {
	for (i in 1:length(y)) {
		y[i] ~ dbin(phi[i], n[i])
		logit(phi[i]) = b0 + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
	}
	
	b0 ~ dnorm(0.0, 1.0/5.0^2)
	for (j in 1:4) {
		b[j] ~ dnorm(0.0, 1.0/4.0^2)
	}
	
} "

data_jags = as.list(as.data.frame(X))
data_jags$y = dat$Correct # this will not work if there are missing values in dat (because they would be ignored by model.matrix). Always make sure that the data are accurately pre-processed for JAGS.
data_jags$n = dat$Trials
str(data_jags) # make sure that all variables have the same number of observations (712).

library(rjags)
jags = jags.model(textConnection(mod_string), data = data_jags, n.chains = 3, n.adapt = 1000)
update(jags, 1000) # burn-in
samp = jags.samples(jags, c("b0", "b"), n.iter = 10000)
library(coda)


# Agora use `coda.samples()` diretamente:
coda_samples <- coda.samples(jags, variable.names = c("b0", "b"), n.iter = 10000)

# Isso já é um mcmc.list:
summary(coda_samples)
plot(coda_samples)
HPDinterval(coda_samples)
raftery.diag(coda_samples) #para saber a quantidade de samples 


# Extrair as amostras do parâmetro b[2] de cada cadeia
b2_samples <- samp$b[2,,]  # b[2] é o coeficiente para OME low

# Calcular a probabilidade de b[2] ser negativo
prob_b2_negative <- mean(b2_samples < 0)

# Exibir a probabilidade
prob_b2_negative


# Assuming samp is the list of posterior samples for coefficients


### QUESTAO 7

# Using the posterior mean estimates of the model coefficients, 
# create a point estimate of the probability of correct responses
# for a child of age 60 months, with high OME, using a coherent 
# stimulus of 50 decibels. Round your answer to two decimal places.


# Extract the posterior means for the coefficients
b0_mean <- mean(samp$b0)
b1_mean <- mean(samp$b[1,,])  # Coefficient for Age
b2_mean <- mean(samp$b[2,,])  # Coefficient for OME low
b3_mean <- mean(samp$b[3,,])  # Coefficient for Loud
b4_mean <- mean(samp$b[4,,])  # Coefficient for Noise incoherent


# Define the input values for the child
Age <- 60
OMElow <- 0  # high OME
Loud <- 50
Noiseincoherent <- 0  # coherent stimulus

# Calculate the linear predictor xb
xb <- b0_mean + b1_mean * Age + b2_mean * OMElow + b3_mean * Loud + b4_mean * Noiseincoherent

# Apply the inverse logit function to calculate the probability
probability <- 1 / (1 + exp(-xb))

# Round the probability to 2 decimal places
round(probability, 2)

##### QUESTAO 8
# Use the posterior mean estimates of the model coefficients to create point
# estimates of the probability of correct responses for each observation
# in the original data. To do this, follow the steps outlined in the lesson 
# to create a vector of these probabilities called phat

# Step 2: Calculate the linear predictor (xb) for each observation in the data
# Create a matrix for the variables (excluding intercept)
X_data <- cbind(1, dat$Age, ifelse(dat$OME == "low", 1, 0), dat$Loud, ifelse(dat$Noise == "incoherent", 1, 0))

# Calculate the linear predictor for each observation
xb <- X_data %*% c(b0_mean, b1_mean, b2_mean, b3_mean, b4_mean)

# Step 3: Apply the inverse logit function to get the predicted probabilities (phat)
phat <- 1 / (1 + exp(-xb))
(tab0.7 = table(phat > 0.7, (dat$Correct / dat$Trials) > 0.7))
sum(diag(tab0.7)) / sum(tab0.7)
