library(forecast)
library(fpp2)
library(atsar)

# MONTE CARLO SETUP ------------------
set.seed(1788)
N <- 200
# set up parameters and simulate
sigma = .7
ar = c(1.2, -.6)
ma = c(-0.5, .9)
mu = .3
y <- mu + arima.sim(model = list(order = c(2,0,2), ar = ar, ma = ma), n = N, rand.gen = function(n) rnorm(n, 0, sigma))

# FIT MODELS ------------------
# arima model with auto.arima
arma_fit <- forecast::auto.arima(y)

# using bayesian model
stan_fit <- atsar::fit_stan(y=y, x=1:length(y), model_name = 'arma22')
rstan:::stan_plot(stan_fit)
# rstan:::pairs.stanfit(stan_fit, pars = names(stan_fit) %>% head(-1))

# extract coefs
stan_coefs <- rstan::extract(stan_fit) %>%
  head(-1) %>%
  sapply(., median) # extract the median value

# BOOT STRAP CI -----------------------
# bayesian arima sims
nsims <- 1000
stan_sims <- sapply(1:nsims, function(i) {
  stan_coefs['mu'] + arima.sim(
    model = list(
      order = c(2,0,2),
      ar = stan_coefs[5:6],
      ma = stan_coefs[3:4]),
    n = length(y),
    rand.gen = function(x) rnorm(x, 0, stan_coefs['sigma'])
  )
})
stan_ybar <- apply(stan_sims, 1, mean)

# auto.arima sims
arma_sims <- sapply(1:nsims, function(i) {
  arma_fit$coef['intercept'] +
    arima.sim(
      model = list(
        order = c(2,0,2),
        ar = arma_fit$coef[1:2],
        ma = arma_fit$coef[3:4]),
      n = length(y),
      rand.gen = function(x) rnorm(x, 0, sqrt(arma_fit$sigma2)))
})
arima_ybar <- apply(arma_sims, 1, mean)

# PLOT -------------------
ymin = min(c(arma_sims, stan_sims, y))
ymax = max(c(arma_sims, stan_sims, y))
plot(stan_ybar, ylim = c(ymin, ymax), type = 'n')

# plot stan sims
for(i in 1:nsims) {
  lines(y = stan_sims[, i], x = 1:nrow(stan_sims), lty = 1, col = scales::alpha('blue', .01))
}

# plot arima sims
for(i in 1:nsims) {
  lines(y = arma_sims[, i], x = 1:nrow(arma_sims), lty = 1, col = scales::alpha('red', .01))
}

# overlay expected outcomes
lines(y = arima_ybar, x = 1:length(y), lwd = 2, lty = 3, col = 'red')
lines(y = stan_ybar,  x = 1:length(y), lwd = 2, lty = 3, col = 'blue')

# plot original data
lines(y = y, x = 1:nrow(stan_sims))
