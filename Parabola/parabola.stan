parameters {
  real x;
}
model {
  target += 15 + 10*x - 2*x^2;
}
