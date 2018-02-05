data {
  int N;
  vector[2] y[N];
}
parameters {
  vector[2] mu;
  cov_matrix[2] Sigma;
  vector<lower=-0.5, upper=0.5>[2] round_err[N];
}
transformed parameters {
  vector[2] z[N];
  for (n in 1:N) {
    z[n] = y[n] - round_err[n];
  }
}
model {
  z ~ multi_normal(mu, Sigma);
}
