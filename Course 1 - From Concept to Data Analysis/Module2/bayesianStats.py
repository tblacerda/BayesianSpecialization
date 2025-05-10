import numpy as np
from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import Matern
from scipy.stats import norm
from scipy.optimize import minimize
from sklearn.model_selection import train_test_split

def maximize_users(user_function, n_iter=30, init_points=2):
    """Bayesian optimization with Gaussian Process and Expected Improvement."""
    t_bounds = (-2, 15)
    a_bounds = (-30, 30)
    bounds = np.array([t_bounds, a_bounds])
    
    # Latin Hypercube Sampling for initial points
    X = np.column_stack((
        np.random.uniform(t_bounds[0], t_bounds[1], init_points),
        np.random.uniform(a_bounds[0], a_bounds[1], init_points)
    ))
    X, _ = train_test_split(X, train_size=init_points, random_state=42)
    
    y = np.array([user_function(t, a) for t, a in X])
    evaluated_points = set(map(tuple, X))
    
    # Kernel with optimized hyperparameters
    kernel = Matern(nu=2.5)
    gp = GaussianProcessRegressor(kernel=kernel, alpha=1e-6, n_restarts_optimizer=5, normalize_y=True)
    gp.fit(X, y)
    
    def expected_improvement(x):
        """Vectorized Expected Improvement."""
        x = np.atleast_2d(x)
        mu, sigma = gp.predict(x, return_std=True)
        mu = mu.reshape(-1, 1)
        sigma = sigma.reshape(-1, 1)
        y_max = np.max(y)
        improvement = mu - y_max
        Z = improvement / sigma
        ei = improvement * norm.cdf(Z) + sigma * norm.pdf(Z)
        return -ei  # Negative for minimization
    
    # Bayesian optimization loop
    for _ in range(n_iter):
        # Find candidate with the highest EI using gradient-based optimization
        best_x = None
        best_ei = np.inf  # We minimize -EI
        
        # Multi-start optimization to avoid local minima
        for _ in range(5):  # 5 random restarts
            x0 = np.array([
                np.random.uniform(t_bounds[0], t_bounds[1]),
                np.random.uniform(a_bounds[0], a_bounds[1])
            ])
            res = minimize(expected_improvement, x0, bounds=bounds, method='L-BFGS-B')
            if res.fun < best_ei and not any(np.allclose(res.x, x, atol=1e-3) for x in evaluated_points):
                best_ei = res.fun
                best_x = res.x
        
        if best_x is None:
            continue  # Fallback to random sampling if all candidates are duplicates
        
        # Evaluate and update
        new_y = user_function(best_x[0], best_x[1])
        X = np.vstack([X, best_x])
        y = np.append(y, new_y)
        evaluated_points.add(tuple(best_x))
        gp.fit(X, y)
    
    idx_best = np.argmax(y)
    return (X[idx_best, 0], X[idx_best, 1]), y[idx_best]



if __name__ == "__main__":
    counter = 0
    # Test function with maximum at (7, 5)
    def expensive_probe(t, a):
        global counter
        counter += 1
        print(f"Probe {counter}: t={t}, a={a}")

        return -((t - 7)**2 + (a - 5)**2) + 100
    
    best_pos, best_val = maximize_users(expensive_probe)
    print(f"Optimal Position: {best_pos}, Value: {best_val}")