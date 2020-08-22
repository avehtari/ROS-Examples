#' ---
#' title: "Regression and Other Stories: Roaches"
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

#' Analyse the effect of integrated pest management on reducing
#' cockroach levels in urban apartments. See Chapter 15 in
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
library("rstanarm")
library("brms")
library("loo")
library("ggplot2")
library("bayesplot")
theme_set(bayesplot::theme_default(base_family = "sans"))
#+ eval=FALSE, include=FALSE
# grayscale figures for the book
if (savefigs) color_scheme_set(scheme = "gray")

#' Set random seed for reproducability
SEED <- 3579

#' #### Load data
data(roaches)
(n <- nrow(roaches))
#' scale the number of roaches by 100
roaches$roach100 <- roaches$roach1 / 100
head(roaches)

#' ## Negative-binomial model
#'
#' negative-binomial model is over-dispersed compared to Poisson
#' 
fit_1 <- stan_glm(y ~ roach100 + treatment + senior, family=neg_binomial_2,
  offset=log(exposure2), data=roaches, seed=SEED, refresh=0)
prior_summary(fit_1)
print(fit_1, digits=2)
loo_1 <- loo(fit_1)

#' #### Graphical posterior predictive checking
yrep_1 <- posterior_predict(fit_1)
n_sims <- nrow(yrep_1)
sims_display <- sample(n_sims, 100)
ppc_1 <- ppc_dens_overlay(log10(roaches$y+1), log10(yrep_1[sims_display,]+1))+
  xlab('log10(y+1)') +
  theme(axis.line.y = element_blank())
ppc_1

#' #### Predictive checking with test statistic<br>
#' ppc with proportion of zero counts test statistic
ppc_stat(y=roaches$y, yrep=yrep_1, stat=function(y) mean(y==0))
#' or
print(mean(roaches$y==0), digits=2)
print(mean(yrep_1==0), digits=2)

#' #### Predictive checking with test statistic<br>
#' ppc with proportion of counts of 1 test statistic
ppc_stat(y=roaches$y, yrep=yrep_1, stat=function(y) mean(y==1))
#' or
print(mean(roaches$y==1), digits=2)
print(mean(yrep_1==1), digits=2)

#' ppc with 95% quantile test statistic
ppc_stat(y=roaches$y, yrep=yrep_1, stat=function(y) quantile(y, probs=0.95))

#' ppc with 99% quantile test statistic
ppc_stat(y=roaches$y, yrep=yrep_1, stat=function(y) quantile(y, probs=0.99))

#' ppc with max count test statistic
ppc_stat(y=roaches$y, yrep=yrep_1, stat=max)
#' or
print(max(roaches$y), digits=2)
print(max(yrep_1), digits=2)

#' ## Poisson model
#' 
#' Poisson is a special case of negative-binomial
#' 
fit_2 <- stan_glm(y ~ roach100 + treatment + senior, family=poisson,
  offset=log(exposure2), data=roaches, seed=SEED, refresh=0)
prior_summary(fit_2)
print(fit_2, digits=2)
loo_2 <- loo(fit_2)

loo_compare(loo_1, loo_2)

#' #### Graphical posterior predictive checking<br>
#'
#' instead of y, we plot log10(y+1) to better show the differences in
#' the shape of the predictive distribution
yrep_2 <- posterior_predict(fit_2)
n_sims <- nrow(yrep_2)
sims_display <- sample(n_sims, 100)
ppc_2 <- ppc_dens_overlay(log10(roaches$y+1), log10(yrep_2[sims_display,]+1)) +
  xlim(0,3) +
  xlab('log10(y+1)') +
  theme(axis.line.y = element_blank())
ppc_2

#+ eval=FALSE, include=FALSE
pbg <- bayesplot_grid(ppc_2, ppc_1,
                      grid_args = list(ncol = 2),
                      titles = c("Poisson", "negative-binomial"))
if (savefigs) ggsave(root("Roaches/figs","roaches_ppc_12.pdf"), pbg, height=3, width=9, colormode="gray")

#' #### Predictive checking with test statistic<br>
#' test statistic used is the proportion of zero counts
ppc_stat(y=roaches$y, yrep=yrep_2, stat=function(y) mean(y==0))
#' or
print(mean(roaches$y==0), digits=2)
print(mean(yrep_2==0), digits=2)

#' ## Zero-inflated negative-binomial model
#'
#' Zero-inflated negative-binomial model is mixture of two models
#'  - logistic regression to model the proportion of extra zero counts
#'  - negative-binomial model
#'
#' we switch to brms as rstanarm doesn't support zero-inflated
#' negative-binomial model
roaches$logp1_roach1 <- log(roaches$roach1+1)
roaches$log_exposure2 <- log(roaches$exposure2)
#+ results='hide'
fit_3 <- brm(bf(y ~ logp1_roach1 + treatment + senior + offset(log_exposure2),
                zi ~ logp1_roach1 + treatment + senior + offset(log_exposure2)),
             family=zero_inflated_negbinomial(), data=roaches,
             prior=set_prior("normal(0,1)"), seed=SEED, refresh=500)
#+
print(fit_3)
loo_3 <- loo(fit_3)
loo_compare(loo_1, loo_3)

#' #### Graphical posterior predictive checking
yrep_3 <- posterior_predict(fit_3)
ppc_3 <- ppc_dens_overlay(log10(roaches$y+1), log10(yrep_3[sims_display,]+1))+
  xlab('log10(y+1)') +
  theme(axis.line.y = element_blank())
ppc_3
#+ eval=FALSE, include=FALSE
if (savefigs) ggsave(root("Roaches/figs","roaches_ppc_3.pdf"), ppc_3, height=3, width=4.5, colormode="gray")

#' #### Predictive checking with test statistic<br>
#' ppc with zero count test statistic
ppc_stat(y=roaches$y, yrep=yrep_3, stat=function(y) mean(y==0))
#' or
print(mean(roaches$y==0), digits=2)
print(mean(yrep_3==0), digits=2)

#' ppc with 95% quantile test statistic
ppc_stat(y=roaches$y, yrep=yrep_3, stat=function(y) quantile(y, probs=0.95))

#' ppc with 99% quantile test statistic
ppc_stat(y=roaches$y, yrep=yrep_3, stat=function(y) quantile(y, probs=0.99))

#' ppc with max count test statistic
ppc_stat(y=roaches$y, yrep=yrep_3, stat=max)
#' or
print(max(roaches$y), digits=2)
print(max(yrep_3), digits=2)
