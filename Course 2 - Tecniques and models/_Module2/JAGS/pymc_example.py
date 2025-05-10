import pymc as pm
import numpy as np
import arviz as az

# 1. Especificar o modelo
with pm.Model() as model:
    # Dados observados
    y = np.array([1.2, 1.4, -0.5, 0.3, 0.9, 2.3, 1.0, 0.1, 1.3, 1.9])
    n = len(y)
    
    # Priors
    mu = pm.StudentT('mu', mu=0, sigma=1, nu=1)
    sigma = 1.0  # Variância fixa conforme o modelo original
    
    # Verossimilhança
    y_obs = pm.Normal('y_obs', mu=mu, sigma=sigma, observed=y)

    # 2. Executar o amostrador MCMC
    trace = pm.sample(10000, tune=1000, return_inferencedata=True, progressbar=True)  

# 3. Pós-processamento
print(az.summary(trace, var_names=['mu']))
az.plot_trace(trace, var_names=['mu'])
