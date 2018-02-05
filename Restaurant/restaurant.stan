parameters {
  real<lower=0,upper=100> x;
}
model {
  target += (5000/x^2)*(x-11);
}

