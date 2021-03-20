data {
  int N;
  vector[N] x;
  int y[N];
}
parameters {
  real a;
  real b;
}
model {
  y ~ bernoulli_logit(a + b*x);
}
