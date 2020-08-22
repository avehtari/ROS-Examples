#' ---
#' title: "Regression and Other Stories: Cross-validation"
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

#' Introduction to cross-validation for linear regression. See Chapter
#' 11 in Regression and Other Stories.
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
library("loo")
library("ggplot2")
library("bayesplot")
theme_set(bayesplot::theme_default(base_family = "sans", base_size=16))
library("foreign")
# for reproducibility
SEED <- 1507

#' ## Simulation data example

#' #### Simulate fake data
x <- 1:20
n <- length(x)
a <- 0.2
b <- 0.3
sigma <- 1
# set the random seed to get reproducible results
# change the seed to experiment with variation due to random noise
set.seed(2141) 
y <- a + b*x + sigma*rnorm(n)
fake <- data.frame(x, y)

#' #### Fit linear model
#+ results='hide'
fit_all <- stan_glm(y ~ x, data = fake, seed=2141, chains=10, refresh=0)

#' #### Fit linear model without 18th observation
fit_minus_18 <- stan_glm(y ~ x, data = fake[-18,], seed=2141, refresh=0)

#' #### Extract posterior draws
sims <- as.matrix(fit_all)
sims_minus_18 <- as.matrix(fit_minus_18)

#' #### Compute posterior predictive distribution given x=18
condpred<-data.frame(y=seq(0,9,length.out=100))
condpred$x <- sapply(condpred$y, FUN=function(y) mean(dnorm(y, sims[,1] + sims[,2]*18, sims[,3])*6+18))

#' #### Compute LOO posterior predictive distribution given x=18
condpredloo<-data.frame(y=seq(0,9,length.out=100))
condpredloo$x <- sapply(condpredloo$y, FUN=function(y) mean(dnorm(y, sims_minus_18[,1] + sims_minus_18[,2]*18, sims_minus_18[,3])*6+18))

#' #### Create a plot with posterior mean and posterior predictive distribution
p1 <- ggplot(fake, aes(x = x, y = y)) +
  geom_point(color = "white", size = 3) +
  geom_point(color = "black", size = 2)
p2 <- p1 +
  geom_abline(
    intercept = mean(sims[, 1]),
    slope = mean(sims[, 2]),
    size = 1,
    color = "black"
  )
p3 <- p2 + 
  geom_path(data=condpred,aes(x=x,y=y), color="black") +
  geom_vline(xintercept=18, linetype=3, color="grey")

#' #### Add LOO mean and LOO predictive distribution when x=18 is left out
p4 <- p3 +
  geom_point(data=fake[18,], color = "grey50", size = 5, shape=1) +
  geom_abline(
    intercept = mean(sims_minus_18[, 1]),
    slope = mean(sims_minus_18[, 2]),
    size = 1,
    color = "grey50",
    linetype=2
  ) +
  geom_path(data=condpredloo,aes(x=x,y=y), color="grey50", linetype=2)
p4
#+ eval=FALSE, include=FALSE
if (savefigs) ggsave(root("CrossValidation/figs","fakeloo1a.pdf"),p4,width=6,height=5)
#+

#' #### Compute posterior and LOO residuals</br>
#' `loo_predict()` computes mean of LOO predictive distribution.
fake$residual <- fake$y-fit_all$fitted
fake$looresidual <- fake$y-loo_predict(fit_all)$value

#' #### Plot posterior and LOO residuals
p1 <- ggplot(fake, aes(x = x, y = residual)) +
  geom_point(color = "black", size = 2, shape=16) +
  geom_point(aes(y=looresidual), color = "grey50", size = 2, shape=1) +
  geom_segment(aes(xend=x, y=residual, yend=looresidual)) +
  geom_hline(yintercept=0, linetype=2)
p1
#+ eval=FALSE, include=FALSE
if (savefigs) ggsave(root("CrossValidation/figs","fakeloo2.pdf"),p4,width=6,height=5)
#+

#' #### Standard deviations of posterior and LOO residuals
round(sd(fake$residual),2)
round(sd(fake$looresidual),2)

#' Variance of residuals is connected to R^2, which can be defined as
#' 1-var(res)/var(y)
round(1-var(fake$residual)/var(y),2)
round(1-var(fake$looresidual)/var(y),2)

#' #### Compute log posterior predictive densities</br>
#' `log_lik` returns $\log(p(y_i|\theta^{(s)}))$
ll_1 <- log_lik(fit_all)
#' compute $\log(\frac{1}{S}\sum_{s=1}^S p(y_i|\theta^{(s)})$ in
#' computationally stable way
fake$lpd_post <- matrixStats::colLogSumExps(ll_1) - log(nrow(ll_1))

#' #### Compute log LOO predictive densities</br>
#' `loo` uses fast approximate leave-one-out cross-validation
loo_1 <- loo(fit_all)
fake$lpd_loo <-loo_1$pointwise[,"elpd_loo"]

#' #### Plot posterior and LOO log predictive densities
p1 <- ggplot(fake, aes(x = x, y = lpd_post)) +
  geom_point(color = "black", size = 2, shape=16) +
  geom_point(aes(y=lpd_loo), color = "grey50", size = 2, shape=1) +
  geom_segment(aes(xend=x, y=lpd_post, yend=lpd_loo)) +
  ylab("log predictive density")
p1
#+ eval=FALSE, include=FALSE
if (savefigs) ggsave(root("CrossValidation/figs","fakeloo2.pdf"),p1,width=6,height=5)
#+

#' ## KidIQ example

#' #### Load children's test scores data
kidiq <- read.dta(file=root("KidIQ/data","kidiq.dta"))

#' #### Linear regression
fit_3 <- stan_glm(kid_score ~ mom_hs + mom_iq, data=kidiq,
                  seed=SEED, refresh = 0)
print(fit_3)

#' #### Compute R^2 and LOO-R^2 manually
respost <- kidiq$kid_score-fit_3$fitted
resloo <- kidiq$kid_score-loo_predict(fit_3)$value
round(R2 <- 1 - var(respost)/var(kidiq$kid_score), 2)
round(R2loo <- 1 - var(resloo)/var(kidiq$kid_score), 2)

#' #### Add five pure noise predictors to the data
set.seed(SEED)
n <- nrow(kidiq)
kidiqr <- kidiq
kidiqr$noise <- array(rnorm(5*n), c(n,5))

#' #### Linear regression with additional noise predictors
fit_3n <- stan_glm(kid_score ~ mom_hs + mom_iq + noise, data=kidiqr,
                   seed=SEED, refresh = 0)
print(fit_3n)

#' #### Compute R^2 and LOO-R^2 manually
respostn <- kidiq$kid_score-fit_3n$fitted
resloon <- kidiq$kid_score-loo_predict(fit_3n)$value
round(R2n <- 1 - var(respostn)/var(kidiq$kid_score), 2)
round(R2loon <- 1 - var(resloon)/var(kidiq$kid_score), 2)

#' #### Alternative more informative regularized horseshoe prior
fit_3rhs <- stan_glm(kid_score ~ mom_hs + mom_iq, prior=hs(), data=kidiq,
                     seed=SEED, refresh = 0)
fit_3rhsn <- stan_glm(kid_score ~ mom_hs + mom_iq + noise, prior=hs(),
                      data=kidiqr, seed=SEED, refresh = 0)
round(median(bayes_R2(fit_3rhs)), 2)
round(median(loo_R2(fit_3rhs)), 2)
round(median(bayes_R2(fit_3rhsn)), 2)
round(median(loo_R2(fit_3rhsn)), 2)

#' #### Compute sum of log posterior predictive densities</br>
#' `log_lik` returns $\log(p(y_i|\theta^{(s)}))$
ll_3 <- log_lik(fit_3)
ll_3n <- log_lik(fit_3n)
#' compute $\log(\frac{1}{S}\sum_{s=1}^S p(y_i|\theta^{(s)})$ in
#' computationally stable way
round(sum(matrixStats::colLogSumExps(ll_3) - log(nrow(ll_3))), 1)
round(sum(matrixStats::colLogSumExps(ll_3n) - log(nrow(ll_3n))), 1)

#' #### Compute log LOO predictive densities</br>
#' `loo` uses fast approximate leave-one-out cross-validation
loo_3 <- loo(fit_3)
loo_3n <- loo(fit_3n)
round(loo_3$estimate["elpd_loo",1], 1)
round(loo_3n$estimate["elpd_loo",1], 1)

#' #### More information on LOO elpd estimate including standard errors
loo_3
loo_3n

#' #### Model using only the maternal high school indicator
#'
fit_1 <- stan_glm(kid_score ~ mom_hs, data=kidiq, refresh = 0)
(loo_1 <- loo(fit_1))

#' #### Compare models using LOO log score (elpd)
loo_compare(loo_3, loo_1)

#' #### Compare models using LOO-R^2
# we need to fix the seed to get comparison to work correctly in this case
set.seed(1414)
looR2_1 <- loo_R2(fit_1)
set.seed(1414)
looR2_3 <- loo_R2(fit_3)
round(mean(looR2_1), 2)
round(mean(looR2_3), 2)
round(mean(looR2_3-looR2_1), 2)
round(sd(looR2_3-looR2_1), 2)

#' #### Model with an interaction
fit_4 <- stan_glm(kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq,
                  data=kidiq, refresh=0)
#' #### Compare models using LOO log score (elpd)
loo_4 <- loo(fit_4)
loo_compare(loo_3, loo_4)

#' #### Compare models using LOO-R^2
set.seed(1414)
looR2_4 <- loo_R2(fit_4)
round(mean(looR2_4), 2)
round(mean(looR2_4-looR2_3), 2)
round(sd(looR2_4-looR2_3), 2)
