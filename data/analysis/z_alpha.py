from scipy.stats import norm

alpha = 1 - 0.95
z_alpha = norm.ppf(1 - alpha / 2)
print("z_alpha =", z_alpha)
z_alpha_squared = z_alpha ** 2
print("z_alpha_squared =", z_alpha_squared)