data {
  int J;
  int n[J];
  vector[J] x;
  int y[J];
  real r;
  real R;
}
parameters {
  real<lower=0> sigma;
}
model {
  vector[J] p = 2*Phi(asin((R-r) ./ x) / sigma) - 1;
  y ~ binomial(n, p);
}
generated quantities {
  real sigma_degrees;
  sigma_degrees = (180/pi())*sigma;
}

