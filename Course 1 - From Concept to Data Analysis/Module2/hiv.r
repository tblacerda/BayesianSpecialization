library(ggplot2)

p_HIV_given_n_positives <- function(p_HIV, p_pos_given_HIV, p_neg_given_no_HIV, n) {
  # Calculate false positive rate (1 - specificity)
  fp_rate <- 1 - p_neg_given_no_HIV
  p_no_HIV <- 1 - p_HIV
  
  # Initial probability after first positive test
  p_HIV_given_pos <- (p_pos_given_HIV * p_HIV) / (p_pos_given_HIV * p_HIV + fp_rate * p_no_HIV)
  
  # Update probability for each subsequent positive test
  if (n > 1) {
    for (i in 2:n) {
      numerator <- p_pos_given_HIV * p_HIV_given_pos
      denominator <- numerator + fp_rate * (1 - p_HIV_given_pos)
      p_HIV_given_pos <- numerator / denominator
    }
  }
  
  return(p_HIV_given_pos)
}

# Parameters
p_pos_given_HIV <- 0.977   # Sensitivity
p_neg_given_no_HIV <- 0.926  # Specificity
p_HIV <- 0.0026           # Prevalence

# Calculate for different numbers of tests
n_values <- c(1, 2, 3, 5, 10)
results <- sapply(n_values, function(n) p_HIV_given_n_positives(p_HIV, p_pos_given_HIV, p_neg_given_no_HIV, n))

# Display results
df <-data.frame(n_tests = n_values, probability = results)


# Generate the plot
ggplot(df, aes(x = n_tests, y = probability)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "red", size = 3) +
  labs(title = "Probability of Having HIV Given n Positive Tests",
       x = "Number of Consecutive Positive Tests",
       y = "P(HIV | n Positive Tests)") +
  theme_minimal()


qnorm(p=0.975, mean=0, sd=1)





# Posterior mean and variance
posterior_mean <- 96.17
posterior_variance <- 0.042

# Calculate the standard deviation
posterior_sd <- sqrt(posterior_variance)

# Calculate the upper end of the 95% credible interval
upper_end <- qnorm(0.975, mean = posterior_mean, sd = posterior_sd)

# Print the result
upper_end




set.seed(123)  # For reproducibility
a <- 3
b <- 200
gamma_draws <- rgamma(1000, shape = a, rate = b)  # Draw from Gamma(a, b)
inv_gamma_draws <- 1 / gamma_draws  # Take reciprocals to get Inverse-Gamma samples

# Plot histogram to visualize
hist(inv_gamma_draws, breaks = 30, main = "Inverse-Gamma(3, 200) Samples", col = "lightblue")




set.seed(123)  # For reproducibility

# Given parameters
m <- 500
w <- 0.1
a <- 3
b <- 200

# Simulate 1000 draws from the Inverse-Gamma(3, 200) for sigma^2
gamma_draws <- rgamma(1000, shape = a, rate = b)  # Gamma(a, b)
sigma2_draws <- 1 / gamma_draws  # Convert to Inverse-Gamma

# Simulate 1000 draws from the prior Normal(m, sigma^2 / w)
mu_prior_draws <- rnorm(1000, mean = m, sd = sqrt(sigma2_draws / w))

mean(mu_prior_draws)
# Plot histogram to visualize the prior mean distribution
hist(mu_prior_draws, breaks = 30, main = "Prior Distribution of μ", col = "lightblue")


z <- rgamma(1000, shape=16.5, rate=6022.9)
sig2 <- 1/z
mu <- rnorm(1000, mean=609.3, sd=sqrt(sig2/27.1))

mu
mean(mu)
quantile(x=mu, probs=c(0.025, 0.975))


sum( muA > muB ) / 1000


# Define the range of theta values
theta <- seq(0, 1, length.out = 1000)

# Calculate the density of the Beta(1/2, 1/2) distribution
density <- dbeta(theta, shape1 = 1/2, shape2 = 1/2)

# Plot the Beta(1/2, 1/2) distribution
curve(dbeta(x, shape1 = 1/2, shape2 = 1/2), from = 0, to = 1,
      xlab = expression(theta), ylab = "Density",
      main = "Beta(1/2, 1/2) Distribution", col = "blue", lwd = 2)




# Load necessary library
library(ggplot2)

# Specify the URL
url <- "http://www.stat.ufl.edu/~winner/data/pgalpga2008.dat"

# Read the data from the URL
data <- read.table(url, header = FALSE)

# Assign column names based on the description
colnames(data) <- c("AvgDriveDistance", "PercentAccuracy", "Gender")

# Separate data for female and male golfers
female_golfers <- subset(data, Gender == 1, select = c(AvgDriveDistance, PercentAccuracy))
male_golfers <- subset(data, Gender == 2, select = c(AvgDriveDistance, PercentAccuracy))

# Create scatter plot for female golfers
ggplot(female_golfers, aes(x = AvgDriveDistance, y = PercentAccuracy)) +
  geom_point() +
  labs(title = "Female Golfers: Average Drive Distance vs. Percent Accuracy",
       x = "Average Drive Distance (yards)",
       y = "Percent Accuracy")

# Create scatter plot for male golfers
ggplot(male_golfers, aes(x = AvgDriveDistance, y = PercentAccuracy)) +
  geom_point() +
  labs(title = "Male Golfers: Average Drive Distance vs. Percent Accuracy",
       x = "Average Drive Distance (yards)",
       y = "Percent Accuracy")


# Load necessary library
library(MASS)  # For Bayesian linear regression

# Assuming 'female_golfers' is already created from previous steps
# female_golfers <- subset(data, Gender == 1, select = c(AvgDriveDistance, PercentAccuracy))

# Fit the Bayesian linear regression model
model <- lm(PercentAccuracy ~ AvgDriveDistance, data = female_golfers)

# Summary of the model to get the coefficients
summary(model)

# Extract the slope parameter (b1)
slope <- coef(model)["AvgDriveDistance"]

# Round the slope to two decimal places
slope_rounded <- round(slope, 2)

# Print the rounded slope
slope_rounded



# Assuming 'model' is the linear regression model fitted earlier
# model <- lm(PercentAccuracy ~ AvgDriveDistance, data = female_golfers)

# Extract the intercept (b0) and slope (b1) from the model
intercept <- coef(model)["(Intercept)"]
slope <- coef(model)["AvgDriveDistance"]

# Define the new driving distance
new_distance <- 260

# Calculate the predicted accuracy
predicted_accuracy <- intercept + slope * new_distance

# Round the predicted accuracy to one decimal place
predicted_accuracy_rounded <- round(predicted_accuracy, 1)

# Print the rounded predicted accuracy
predicted_accuracy_rounded






# Load necessary library
library(dplyr)

# Specify the URL
url <- "http://www.stat.ufl.edu/~winner/data/pgalpga2008.dat"

# Read the data from the URL
data <- read.table(url, header = FALSE)

# Assign column names based on the description
colnames(data) <- c("AvgDriveDistance", "PercentAccuracy", "Gender")

# Modify the Gender column: 0 for female (1), 1 for male (2)
data <- data %>%
  mutate(Gender = ifelse(Gender == 1, 0, 1))

# Fit the multiple regression model
model <- lm(PercentAccuracy ~ AvgDriveDistance + Gender, data = data)

# Summary of the model to get the coefficients
summary(model)

# Extract the intercept (b0)
intercept <- coef(model)["(Intercept)"]

# Round the intercept to the nearest whole number
intercept_rounded <- round(intercept)

# Print the rounded intercept
intercept_rounded


plot(fitted(mod), residuals(mod))
