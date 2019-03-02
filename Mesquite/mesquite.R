#' ---
#' title: "Regression and Other Stories: Mesquite"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Predicting the yields of mesquite bushes
#' 
#' -------------
#' 

#+ include=FALSE
# switch this to TRUE to save figures in separate files
savefigs <- FALSE

#' **Load packages**
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library("rprojroot")
root<-has_dirname("RAOS-Examples")$make_fix_file()
library("rstanarm")
options(mc.cores = parallel::detectCores())
library("loo")
library("ggplot2")
library("bayesplot")
theme_set(bayesplot::theme_default(base_family = "sans"))
color_scheme_set(scheme = "gray")
library("foreign")

#' **Load data**
mesquite <- read.table(root("Mesquite/data","mesquite.dat"), header=TRUE)
summary(mesquite)

#' **Regress `weight` on all of the predictors**
fit_1 <- stan_glm(weight ~ diam1 + diam2 + canopy_height + total_height +
                      density + group, data=mesquite)
print(fit_1)
(loo_1 <- loo(fit_1))
#+ results='hide'
kfold_1 <- kfold(fit_1, K=10)
#+
kfold_1

#' **Regress `log(weight)` on all of the log transformed predictors**
fit_2 <- stan_glm(formula = log(weight) ~ log(diam1) + log(diam2) +
                      log(canopy_height) + log(total_height) + log(density) +
                      group, data=mesquite)
(loo_2 <- loo(fit_2))

#' **Jacobian correction to make the models comparable**
loo_2_with_jacobian <- loo_2
loo_2_with_jacobian$pointwise[,1] <- loo_2_with_jacobian$pointwise[,1]-
                                     log(mesquite$weight)
(elpd_loo_2_with_jacobian <- sum(loo_2_with_jacobian$pointwise[,1]))
#' `compare_models` checks that the target data is same, but `compare`
#' trusts that we know what we are comparing
compare(kfold_1, loo_2_with_jacobian)

#' **Posterior predictive checking for model in original scale**
yrep_1 <- posterior_predict(fit_1)
n_sims <- nrow(yrep_1)
subset <- sample(n_sims, 100)
ppc_1 <- ppc_dens_overlay(mesquite$weight, yrep_1[subset,])
#' **Posterior predictive checking for model in log scale**
yrep_2 <- posterior_predict(fit_2)
ppc_2 <- ppc_dens_overlay(log(mesquite$weight), yrep_2[subset,])
bpg <- bayesplot_grid(
  ppc_1, ppc_2,
  grid_args = list(ncol = 2),
  titles = c("Model for weight", "Model for log(weight)")
)
#' **Display posterior predictive checking plots**
bpg
#+ eval=FALSE, include=FALSE
if (savefigs)
    ggsave(root("Mesquite/figs","mesquite_ppc.pdf"), bpg, height=3, width=9)

#' **Plot marginal posteriors**
#+ fig.height=3, fig.width=6
mcmc_areas(as.matrix(fit_2), regex_pars = "^log|^gro")
#+ eval=FALSE, include=FALSE
if (savefigs)
    ggsave(root("Mesquite/figs","mesquite_areas.pdf"), height=3.5, width=5)

#' **Plot joint marginal posterior for log(canopy_height) and log(total_height)
mcmc_scatter(as.matrix(fit_2), pars = c("log(canopy_height)","log(total_height)"), size = 1) +
    geom_vline(xintercept=0) +
    geom_hline(yintercept=0) +
    labs(x="coef of log(canopy_height)", y="coef of log(total_height)")
#+ eval=FALSE, include=FALSE
if (savefigs)
    ggsave(root("Mesquite/figs","mesquite_scatter.pdf"), height=3.5, width=5)

#' **Additional transformed variables**
mesquite$canopy_volume <- mesquite$diam1 * mesquite$diam2 * mesquite$canopy_height
mesquite$canopy_area <- mesquite$diam1 * mesquite$diam2
mesquite$canopy_shape <- mesquite$diam1 / mesquite$diam2

#' **A model with just the new canopy volume variable**
fit_3 <- stan_glm(log(weight) ~ log(canopy_volume), data=mesquite)
print(fit_3)
loo_3 <- loo(fit_3)
compare_models(loo_2, loo_3)

#' **Compare also LOO-R^2**
looR2 <- function(fit) {
    y <- get_y(fit)
    ypred <- posterior_linpred(fit)
    ll <- log_lik(fit)
    r_eff <- relative_eff(exp(ll), chain_id = rep(1:4, each = 1000))
    psis_object <- psis(log_ratios = -ll, r_eff = r_eff)
    ypredloo <- E_loo(ypred, psis_object, log_ratios = -ll)$value
    eloo <- ypredloo-y
    return(1-var(eloo)/var(y))
}
round(looR2(fit_2),2)
round(looR2(fit_3),2)

#' **Compare Bayesian R^2**
round(median(bayes_R2(fit_2)),2)
round(median(bayes_R2(fit_3)),2)


#' **Add canopy area and canopy shape**
fit_4 <- stan_glm(formula = log(weight) ~ log(canopy_volume) +
                      log(canopy_area) + log(canopy_shape) +
                      log(total_height) + log(density) + group,
                  data=mesquite)
print(fit_4)
(loo_4 <- loo(fit_4))
compare_models(loo_2, loo_4)
round(looR2(fit_4),2)
round(median(bayes_R2(fit_4)),2)

#' **Plot Bayesian R^2**
#+ results='hide', fig.height=3, fig.width=6
mcmc_hist(data.frame(bayes_R2(fit_4)), binwidth=0.005)+
  xlab('Bayesian R^2') + scale_y_continuous(breaks=NULL)

#' **Plot marginals**
#+ fig.height=3, fig.width=6
mcmc_areas(as.matrix(fit_4))

#' **Plot pairwise joint marginals**<br>
#' Strong collinearity between canopy volume and canopy area is obvious
#+ fig.width=8, fig.height=8
mcmc_pairs(as.matrix(fit_4), size = 1, pars = c("log(canopy_volume)","log(canopy_area)",
                                                "log(canopy_shape)","log(total_height)","log(density)"))

#' **A model with canopy volume and canopy shape**
fit_5 <- stan_glm(log(weight) ~ log(canopy_volume) + log(canopy_shape) +
    group, data=mesquite)
(loo_5 <- loo(fit_5))
compare_models(loo_4, loo_5)
round(looR2(fit_5),2)
round(median(bayes_R2(fit_5)),2)

#' **A model in a previous edition**
fit_6 <- stan_glm(log(weight) ~ log(canopy_volume) + log(canopy_area) +
    group, data=mesquite)
(loo_6 <- loo(fit_6))
compare_models(loo_5, loo_6)
round(looR2(fit_6),2)
round(median(bayes_R2(fit_6)),2)
