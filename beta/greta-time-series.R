# ar1 greta
library(greta)

# function to simulate fake data from the process, with known parameters
fake_data <- function (time = 100,
                       phi = 0,
                       mu_omega = 0,
                       sigma_1 = 0.5,
                       sigma_2 = 0.01) {

  # empty vectors
  omega <- y <- rep(0, 10)

  # initial conditions
  omega[1] <- rnorm(1, mu_omega, 1)
  y[1] <- 0

  # iterate state
  for (t in 2:time) {
    omega[t] <- rnorm(1, mu_omega + phi * (omega[t-1] - mu_omega), sigma_2)
    y[t] <- rnorm(1, exp(omega[t]) * y[t-1], sigma_1)
  }

  # return time series
  y

}

#plot
set.seed(2017-05-03)
y <- fake_data()
plot(y, type = 'l')


time <- length(y)

# matrix of absolute difference in time between observations
time_diff <- abs(outer(2:time, 2:time, `-`))

# covariance function of the ar1 process, with marginal variance parameter 1
ar1_cov <- function (phi, time_diff)
  (phi ^ time_diff) / (1 - phi ^ 2)

# define parameters and their priors
sigma_1 = beta(1, 1)
sigma_2 = beta(1, 1)
mu_omega = normal(0, 1)
phi = normal(0, 1)

# # iterate the process (zero-mean, variance 1, ar1 process)
# omega_raw <- zeros(time)
# omega_raw[1] = normal(0, 1)
# for (t in 2:time)
#   omega_raw[t] = normal(phi * omega_raw[t-1], 1)

# Gaussian process version
omega_raw = multivariate_normal(zeros(time - 1), ar1_cov(phi, time_diff))

# calculate the likelihood in vector fashion
rho <- exp(mu_omega + omega_raw * sigma_2)
likelihood(y[-1]) = normal(t(rho) * y[-time], sigma_1)

model <- define_model(mu_omega, phi, sigma_1, sigma_2)
draws <- mcmc(model, n_samples = 5000)
MCMCvis::MCMCplot(draws)
