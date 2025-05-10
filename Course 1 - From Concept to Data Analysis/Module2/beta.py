import numpy as np
from scipy.stats import beta
from scipy.stats import gamma

import matplotlib.pyplot as plt

def plot_beta(alpha, beta_param):
    x = np.linspace(0, 1, 100)
    y = beta.pdf(x, alpha, beta_param)
    
    plt.figure(figsize=(8, 6))
    plt.plot(x, y, label=f'Beta({alpha}, {beta_param})')
    plt.title('Beta Distribution')
    plt.xlabel('x')
    plt.ylabel('Probability Density')
    plt.legend()
    plt.grid(True)
    plt.show()

# Example usage:
plot_beta(67,6)



import scipy.stats as stats

# Prior parameters (assuming an uninformative prior, Beta(1,1), a uniform distribution)
alpha_prior = 1
beta_prior = 1

# Observed data: 4 Tails in 4 trials
x = 0  # Number of successes (Tails)
n = 4  # Total trials

# Posterior parameters
alpha_post = alpha_prior + x
beta_post = beta_prior + (n - x)

# Compute P(θ < 0.5) using the CDF of Beta distribution
posterior_prob = stats.beta.cdf(0.5, alpha_post, beta_post)

print(f"Posterior probability P(θ < 0.5 | Data) = {posterior_prob:.4f}")


def plot_gamma(alpha, beta_param):
    x = np.linspace(0, 20, 100)
    y = gamma.pdf(x, alpha, scale=1/beta_param)
    
    plt.figure(figsize=(8, 6))
    plt.plot(x, y, label=f'Gamma({alpha}, {beta_param})')
    plt.title('Gamma Distribution')
    plt.xlabel('x')
    plt.ylabel('Probability Density')
    plt.legend()
    plt.grid(True)
    plt.show()

# Example usage:
plot_gamma(67, 6)

def gamma_credible_interval(alpha, beta_param, credibility=0.90):
    lower_bound = gamma.ppf((1 - credibility) / 2, alpha, scale=1/beta_param)
    upper_bound = gamma.ppf(1 - (1 - credibility) / 2, alpha, scale=1/beta_param)
    return lower_bound, upper_bound

# Example usage:
alpha = 67
beta_param = 6
lower, upper = gamma_credible_interval(alpha, beta_param)
print(f"90% credible interval for λ: [{lower:.4f}, {upper:.4f}]")