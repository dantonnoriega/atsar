data {
  int<lower=3> N;  // number of observations
  vector[N] y;     // observation at time N
}

// constraints follow "Understanding constants in R" of https://otexts.org/fpp2/arima-r.html
parameters {
  real mu;              // mean
  real<lower=0> sigma;  // error scale
  real<lower=-1, upper=1> phi2; // phi2 coefficients
  real<upper=(1-phi2)> phi1;      // phi1 coefficients
  real<lower=-1, upper=1> theta2; // theta2 coefficients
  real<lower=(-1-theta2), upper=(1+theta2)> theta1;   // theta1 coefficients
}

transformed parameters {
  vector[N] err;    // error terms
  vector[N] nu;    // error terms

  nu[1]  = mu*(1 - phi1 - phi2);
  err[1] = y[1] - nu[1];
  nu[2]  = mu * (1 - phi1 - phi2) + phi1 * y[1] + theta1 * err[1];
  err[2] = y[2] - nu[2];

  for (t in 3:N) {
    nu[t] = mu * (1 - phi1 - phi2) +
            phi1 * y[t-1] +
            phi2 * y[t-2] +
            theta1 * err[t-1] +
            theta2 * err[t-2];
    err[t] = y[t] - nu[t];
  }
}

model {
  mu    ~ normal(0,10);
  phi2   ~ normal(0,3);
  phi1   ~ normal(0,3);
  theta2 ~ normal(0,3);
  theta1 ~ normal(0,3);
  sigma ~ cauchy(0,5);

  for (t in 3:N) {
    y[t] ~ normal(mu * (1 - phi1 - phi2) +
                  phi1 * y[t-1] +
                  phi2 * y[t-2] +
                  theta1 * err[t-1] +
                  theta2 * err[t-2],
                  sigma);
  }
}
