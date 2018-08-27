data {
  int<lower=1> N;
  real y[N];
}
parameters {
  real mu;              // mean term
  real<lower=-1,upper=1> phi;             // autoregression coeff
  real<lower=-1,upper=1> theta;           // moving avg coeff
  real<lower=0> sigma;  // noise scale
}
model {
  vector[N] nu;         // prediction for time t
  vector[N] err;        // error for time t
  nu[1]  = mu + phi * mu;   // assume err[0] == 0
  err[1] = y[1] - nu[1];
  for (t in 2:N) {
    nu[t]  = mu + phi * y[t-1] + theta * err[t-1];
    err[t] = y[t] - nu[t];
  }

  // priors
  mu ~ normal(0,10);
  phi ~ normal(0,2);
  theta ~ normal(0,2);
  sigma ~ cauchy(0,5);

  // likelihood
  err ~ normal(0,sigma);
}

