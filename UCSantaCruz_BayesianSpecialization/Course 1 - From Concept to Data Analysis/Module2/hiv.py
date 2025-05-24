import pandas as pd
import matplotlib.pyplot as plt

def p_HIV_given_n_positives(p_HIV, p_pos_given_HIV, p_neg_given_no_HIV, n):
    # Calculate false positive rate (1 - specificity)
    fp_rate = 1 - p_neg_given_no_HIV
    p_no_HIV = 1 - p_HIV
    
    # Initial probability after first positive test
    p_HIV_given_pos = (p_pos_given_HIV * p_HIV) / (p_pos_given_HIV * p_HIV + fp_rate * p_no_HIV)
    
    # Update probability for each subsequent positive test
    if n > 1:
        for i in range(2, n + 1):
            numerator = p_pos_given_HIV * p_HIV_given_pos
            denominator = numerator + fp_rate * (1 - p_HIV_given_pos)
            p_HIV_given_pos = numerator / denominator
    
    return p_HIV_given_pos

    # Parameters
    p_pos_given_HIV = 0.977   # Sensitivity
    p_neg_given_no_HIV = 0.926  # Specificity
    p_HIV = {
        'general_population': 0.0026,
        'risk_group': 0.05
    }

    # Calculate for different numbers of tests
    n_values = [1, 2, 3, 4, 5]
    results_general = [p_HIV_given_n_positives(p_HIV['general_population'], p_pos_given_HIV, p_neg_given_no_HIV, n) for n in n_values]
    results_risk = [p_HIV_given_n_positives(p_HIV['risk_group'], p_pos_given_HIV, p_neg_given_no_HIV, n) for n in n_values]

    # Display results
    df_general = pd.DataFrame({'n_tests': n_values, 'probability': results_general})
    df_risk = pd.DataFrame({'n_tests': n_values, 'probability': results_risk})
    # Generate the plot
    plt.plot(df_general['n_tests'], df_general['probability'], color='blue', linewidth=1.2, label='General Population')
    plt.plot(df_risk['n_tests'], df_risk['probability'], color='green', linewidth=1.2, label='Risk Group')
    plt.scatter(df_general['n_tests'], df_general['probability'], color='red', s=30)
    plt.scatter(df_risk['n_tests'], df_risk['probability'], color='orange', s=30)
    plt.title("Probability of Having HIV Given n Positive Tests")
    plt.xlabel("Number of Consecutive Positive Tests")
    plt.ylabel("P(HIV | n Positive Tests)")
    plt.legend()
    plt.grid(True)
    plt.xticks(df_general['n_tests'])  # Adjust X-axis to show only integer values
    plt.show()



import numpy as np
import matplotlib.pyplot as plt
import scipy.stats as stats

# Parameters for Beta(1,5) distribution
alpha = 1
beta = 5

# Define the range of theta values
theta = np.linspace(0, 1, 100)

# Compute Beta PDF
posterior_pdf = stats.beta.pdf(theta, alpha, beta)

# Plot the posterior distribution
plt.figure(figsize=(8, 5))
plt.plot(theta, posterior_pdf, label=r'$\text{Beta}(1,5)$', color='blue', linewidth=2)
plt.fill_between(theta, posterior_pdf, color='blue', alpha=0.3)
plt.xlabel(r'$\theta$')
plt.ylabel('Density')
plt.title('Posterior Distribution of $\\theta$')
plt.legend()
plt.grid(True)
plt.show()

