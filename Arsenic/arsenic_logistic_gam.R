fit_8s <- stan_gamm4(y ~ s(dist100) + s(arsenic) + s(dist100, educ4),
                     family = binomial(link="logit"), data = wells)
(loo8s <- loo(fit_8s, save_psis=TRUE))
#' Compare models
compare_models(loo8, loo8s)
compare_models(loo8a, loo8s)
