#' ---
#' title: "Regression and Other Stories: Mesquite"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     theme: readable
#'     toc: true
#'     toc_depth: 2
#'     toc_float: true
#'     code_download: true
#' ---

#' Predicting the yields of mesquite bushes. See Chapter 12 in
#' Regression and Other Stories.
#' 
#' -------------
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(message=FALSE, error=FALSE, warning=FALSE, comment=NA)
# switch this to TRUE to save figures in separate files
savefigs <- FALSE

#' #### Load packages
library("rprojroot")
root<-has_file(".ROS-Examples-root")$make_fix_file()
library("foreign")
library("rstanarm")
library("loo")
library("ggplot2")
library("bayesplot")
theme_set(bayesplot::theme_default(base_family = "sans"))
#+ eval=FALSE, include=FALSE
# grayscale figures for the book
if (savefigs) color_scheme_set(scheme = "gray")

#' Set random seed for reproducability
SEED <- 4587

#' #### Load data
mesquite <- read.table(root("Mesquite/data","mesquite.dat"), header=TRUE)
head(mesquite)

#' ## Predict weight given all the predictors
fit_1 <- stan_glm(weight ~ diam1 + diam2 + canopy_height + total_height +
                    density + group, data=mesquite, seed=SEED, refresh=0)
print(fit_1)
(loo_1 <- loo(fit_1))
#' We get warnings about high Pareto k values, which indicates that
#' the importance sampling approximation used in loo is in this case
#' unreliable. We thus use more robust K-fold-CV.
kfold_1 <- kfold(fit_1, K=10)
kfold_1

#' ## Predict log(weight) given log transformed predictors
fit_2 <- stan_glm(log(weight) ~ log(diam1) + log(diam2) + log(canopy_height) +
                      log(total_height) + log(density) + group,
                  data=mesquite, seed=SEED, refresh=0)
(loo_2 <- loo(fit_2))

#' #### Jacobian correction to make the models comparable<br>
#' Jacobian correction is needed as model 1 is modeling y and model 2
#' is modeling log(y).
loo_2_with_jacobian <- loo_2
loo_2_with_jacobian$pointwise[,1] <- loo_2_with_jacobian$pointwise[,1]-
                                     log(mesquite$weight)
(elpd_loo_2_with_jacobian <- sum(loo_2_with_jacobian$pointwise[,1]))

#' there will be a warning that the target data is not the same same, 
#' this is ok because we have the jacobian correction
loo_compare(kfold_1, loo_2_with_jacobian)

#' #### Posterior predictive checking for model in original scale
yrep_1 <- posterior_predict(fit_1)
n_sims <- nrow(yrep_1)
sims_display <- sample(n_sims, 100)
ppc_1 <- ppc_dens_overlay(mesquite$weight, yrep_1[sims_display,]) +
    theme(axis.line.y = element_blank())
#' #### Posterior predictive checking for model in log scale
yrep_2 <- posterior_predict(fit_2)
ppc_2 <- ppc_dens_overlay(log(mesquite$weight), yrep_2[sims_display,]) +
  theme(axis.line.y = element_blank())
bpg <- bayesplot_grid(
  ppc_1, ppc_2,
  grid_args = list(ncol = 2),
  titles = c("Model for weight", "Model for log(weight)")
)
#' #### Display posterior predictive checking plots
bpg
#+ eval=FALSE, include=FALSE
if (savefigs)
  ggsave(root("Mesquite/figs","mesquite_ppc.pdf"), bpg, height=3, width=9, colormodel="gray")

#' #### Plot marginal posteriors
#+ fig.height=3, fig.width=6
mcmc_areas(as.matrix(fit_2), regex_pars = "^log|^gro")
#+ eval=FALSE, include=FALSE
if (savefigs)
    ggsave(root("Mesquite/figs","mesquite_areas.pdf"), height=3.5, width=5, colormodel="gray")

#' **Plot joint marginal posterior for log(canopy_height) and log(total_height)
mcmc_scatter(as.matrix(fit_2), pars = c("log(canopy_height)","log(total_height)"), size = 1) +
    geom_vline(xintercept=0) +
    geom_hline(yintercept=0) +
    labs(x="coef of log(canopy_height)", y="coef of log(total_height)")
#+ eval=FALSE, include=FALSE
if (savefigs)
    ggsave(root("Mesquite/figs","mesquite_scatter.pdf"), height=3.5, width=5, colormodel="gray")

#' ## Additional transformed variables
mesquite$canopy_volume <- mesquite$diam1 * mesquite$diam2 * mesquite$canopy_height
mesquite$canopy_area <- mesquite$diam1 * mesquite$diam2
mesquite$canopy_shape <- mesquite$diam1 / mesquite$diam2

#' ## A model with a canopy volume variable
fit_3 <- stan_glm(log(weight) ~ log(canopy_volume), data=mesquite,
                  seed=SEED, refresh=0)
print(fit_3)
loo_3 <- loo(fit_3)
#' Both models are modeling log(y) and can be compared directly.
loo_compare(loo_2, loo_3)

#' #### Compare also LOO-R^2
round(median(loo_R2(fit_2)),2)
round(median(loo_R2(fit_3)),2)

#' #### Compare Bayesian R^2
round(median(bayes_R2(fit_2)),2)
round(median(bayes_R2(fit_3)),2)


#' ## Add canopy area and canopy shape
fit_4 <- stan_glm(log(weight) ~ log(canopy_volume) +
                      log(canopy_area) + log(canopy_shape) +
                      log(total_height) + log(density) + group,
                  data=mesquite, seed=SEED, refresh=0)
print(fit_4)
(loo_4 <- loo(fit_4))
loo_compare(loo_2, loo_4)
round(median(loo_R2(fit_4)),2)
round(median(bayes_R2(fit_4)),2)

#' #### Plot Bayesian R^2
#+ results='hide', fig.height=3, fig.width=6
mcmc_hist(data.frame(bayes_R2(fit_4)), binwidth=0.005)+
  xlab('Bayesian R^2') + scale_y_continuous(breaks=NULL)

#' #### Plot marginals
#+ fig.height=3, fig.width=6
mcmc_areas(as.matrix(fit_4))

#' #### Plot pairwise joint marginals<br>
#' Strong collinearity between canopy volume and canopy area is obvious
#+ fig.width=8, fig.height=8
mcmc_pairs(as.matrix(fit_4), pars=c("log(canopy_volume)","log(canopy_area)",
                                    "log(canopy_shape)","log(total_height)",
                                    "log(density)"))

#' ## A model with just canopy volume and canopy shape
fit_5 <- stan_glm(log(weight) ~ log(canopy_volume) + log(canopy_shape) +
    group, data=mesquite, seed=SEED, refresh=0)
(loo_5 <- loo(fit_5))
loo_compare(loo_4, loo_5)
round(median(loo_R2(fit_5)),2)
round(median(bayes_R2(fit_5)),2)
