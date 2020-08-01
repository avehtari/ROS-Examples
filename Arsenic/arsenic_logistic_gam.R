fit_8s <- stan_gamm4(y ~ s(dist100) + s(arsenic) + s(dist100, educ4),
                     family=binomial(), data = wells,
                     control = list(max_treedepth=10))
(loo8s <- loo(fit_8s, save_psis=TRUE))
#' Compare models
loo_compare(loo8, loo8s)
loo_compare(loo8a, loo8s)
