

# Laura keeps record of her loan applications and performs a Bayesian analysis of her success rate θ. 
# Her analysis yields a Beta(5, 3) posterior distribution for θ. 
# The posterior mean for θ is equal to 5 / (5 + 3) = 0.625. 
# However, Laura likes to think in terms of the odds of succeeding, defined as θ / (1 - θ), 
# the probability of success divided by the probability of failure. 
# 
# Use R to simulate a large number of samples (more than 10,000) from the posterior distribution for θ 
# and use these samples to approximate the posterior mean for Laura's odds of success (E(θ / (1 - θ))).

set.seed(123)  # For reproducibility
theta_samples <- rbeta(1000000, 5, 3)  # Simulate 1,000,000 samples from Beta(5,3)
odds <- theta_samples / (1 - theta_samples)  # Compute odds for each sample
mean_odds <- mean(odds)  # Approximate posterior mean of the odds
alpha <- mean(odds>1)
alpha
print(mean_odds)

# I want to know the probability of the odds being greater than 1
odds_gt_1 <- sum(odds > 1) / length(odds)
print(odds_gt_1)



n_samples <- 1000000  # Use a large number of samples for accuracy
samples <- rnorm(n_samples, mean = 0, sd = 1)  # Generate samples from N(0, 1)
quantile_0.3 <- quantile(samples, probs = 0.3)  # Compute the 0.3 quantile
print(quantile_0.3)

help(rbeta) # r random number generation, beta distribution
