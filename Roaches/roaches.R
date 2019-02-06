#' ---
#' title: "Regression and Other Stories: Roaches"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Analyse the effect of integrated pest management on reducing
#' cockroach levels in urban apartments
#' 
#' -------------
#' 

#' **Load packages**
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library("rprojroot")
root<-has_dirname("RAOS-Examples")$make_fix_file()
library("rstanarm")
options(mc.cores = parallel::detectCores())
library("brms")
library("loo")
library("ggplot2")
library("bayesplot")
theme_set(bayesplot::theme_default(base_family = "sans"))
color_scheme_set(scheme = "gray")

#' **Load data**
data(roaches)
(n <- nrow(roaches))
#' scale the number of roaches by 100
roaches$roach100 <- roaches$roach1 / 100

#' ### Poisson model
#' 
#+ results='hide'
fit_1 <- stan_glm(y ~ roach100 + treatment + senior, family=poisson,
  offset=log(exposure2), data=roaches)
#+
prior_summary(fit_1)
print(fit_1, digits=2)

#' **Graphical posterior predictive checking**<br>
#'
#' instead of y, we plot log10(y+1) to better show the differences in
#' the shape of the predictive distribution
yrep_1 <- posterior_predict(fit_1)
n_sims <- nrow(yrep_1)
subset <- sample(n_sims, 100)
(ppc_1 <- ppc_dens_overlay(log10(roaches$y+1), log10(yrep_1[subset,]+1))+
     xlim(0,3) + xlab('log10(y+1)'))

#' **Predictive checking with test statistic**<br>
#' test statistic used is the proportion of zero counts
ppc_stat(y=roaches$y, yrep=yrep_1, stat=function(y) mean(y==0))
#' or
print(mean(roaches$y==0), digits=2)
print(mean(yrep_1==0), digits=2)

#' ### Negative-binomial model
#'
#' negative-binomial model is over-dispersed compared to Poisson
#' 
#+ results='hide'
fit_2 <- stan_glm(y ~ roach100 + treatment + senior, family=neg_binomial_2,
  offset=log(exposure2), data=roaches)
#+
prior_summary(fit_2)
print(fit_2, digits=2)
loo_2 <- loo(fit_2)

#' **Graphical posterior predictive checking**
yrep_2 <- posterior_predict(fit_2)
(ppc_2 <- ppc_dens_overlay(log10(roaches$y+1), log10(yrep_2[subset,]+1))+
     xlab('log10(y+1)'))

#+ eval=FALSE, include=FALSE
pbg <- bayesplot_grid(ppc_1, ppc_2,
                      grid_args = list(ncol = 2),
                      titles = c("Poisson", "negative-binomial"))
ggsave(root("Roaches/figs","roaches_ppc_12.pdf"), pbg, height=3, width=9)

#' **Predictive checking with test statistic**<br>
#' ppc with proportion of zero counts test statistic
ppc_stat(y=roaches$y, yrep=yrep_2, stat=function(y) mean(y==0))
#' or
print(mean(roaches$y==0), digits=2)
print(mean(yrep_2==0), digits=2)

#' **Predictive checking with test statistic**<br>
#' ppc with proportion of counts of 1 test statistic
ppc_stat(y=roaches$y, yrep=yrep_2, stat=function(y) mean(y==1))
#' or
print(mean(roaches$y==1), digits=2)
print(mean(yrep_2==1), digits=2)

#' ppc with 95% quantile test statistic
ppc_stat(y=roaches$y, yrep=yrep_2, stat=function(y) quantile(y, probs=0.95))

#' ppc with 99% quantile test statistic
ppc_stat(y=roaches$y, yrep=yrep_2, stat=function(y) quantile(y, probs=0.99))

#' ppc with max count test statistic
ppc_stat(y=roaches$y, yrep=yrep_2, stat=max)
#' or
print(max(roaches$y), digits=2)
print(max(yrep_2), digits=2)

#' ### Zero-inflated negative-binomial model
#'
#' Zero-inflated negative-binomial model model is mixture of two models
#'  - logistic regression to model the proportion of extra zero counts
#'  - negative-binomial model
#'
#' we switch to brms as rstanarm doesn't support zero-inflated
#' negative-binomial model
roaches$logp1_roach1 <- log(roaches$roach1+1)
roaches$log_exposure2 <- log(roaches$exposure2)
#+ results='hide'
fit_3 <- brm(bf(y ~ logp1_roach1 + treatment + senior,
                 zi ~ logp1_roach1 + treatment + senior),
             family=zero_inflated_negbinomial(), data=roaches,
             prior=set_prior("normal(0,1)"))
#+
print(fit_3)
loo_3 <- loo(fit_3)
compare(loo_2, loo_3)

#' **Graphical posterior predictive checking**
#+ results='hide'
yrep_3 <- posterior_predict(fit_3)
#+
(ppc_3 <- ppc_dens_overlay(log10(roaches$y+1), log10(yrep_3[subset,]+1))+
     xlab('log10(y+1)'))
#+ eval=FALSE, include=FALSE
ggsave(root("Roaches/figs","roaches_ppc_3.pdf"), ppc_3, height=3, width=4.5)

#' **Predictive checking with test statistic**<br>
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
