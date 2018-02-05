setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/Restaurant")

library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

resto <- stan_model("restaurant.stan")
fit <- optimizing(resto)
print(fit)

