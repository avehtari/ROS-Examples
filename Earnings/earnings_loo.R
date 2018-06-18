#' ---
#' title: "Regression and Other Stories: Earnings Example"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#+ setup, message=FALSE, error=FALSE, warning=FALSE
library("here")
library("rstanarm")
options(mc.cores = parallel::detectCores())
library("ggplot2")
library("bayesplot")
theme_set(bayesplot::theme_default(base_family = "sans"))
library("loo")

#' **Load data**
earnings_all <- read.csv(here("Earnings/data","earnings.csv"))
earnings_all$positive <- earnings_all$earn > 0
# only non-zero earnings
earnings <- earnings_all[earnings_all$positive, ]
n <- nrow(earnings)
earnings$log_earn <- log(earnings$earn)

#' **Bayesian logistic regression on non-zero earnings**
fit_1a <- stan_glm(positive ~ height + male,
                   family = binomial(link = "logit"),
                   data = earnings_all)
sims_1a <- as.matrix(fit_1a)
loo1a<-loo(fit_1a)
m=sum(fit_1a$y)
n=length(fit_1a$y)
elpd0_pointwise=matrix(0,1,n)
elpd0_pointwise[fit_1a$y]=log((m-1+2/2)/(n-1+2))
elpd0_pointwise[!fit_1a$y]=log((n-m-1+2/2)/(n-1+2))
elpd0=sum(elpd0_pointwise)
sdelpd0=sd(elpd0_pointwise)*sqrt(n)
elpd0diff=sum(loo1a$pointwise[,"elpd_loo"]-elpd0_pointwise)
sdelpd0diff=sd(loo1a$pointwise[,"elpd_loo"]-elpd0_pointwise)*sqrt(n)
sprintf("The baseline by guessing the larger class elpd_loo %.1f sd %.1f",elpd0,sdelpd0)
sprintf("The difference to baseline elpd_diff %.1f sd %.1f",elpd0diff,sdelpd0diff)

sprintf('Baseline classification accuracy %.2f',m/n)
sprintf('Loo  accuracy %.2f',mean(loo1a$pointwise[,"elpd_loo"]>log(1/2)))

#' **Bayesian model on log scale**
fit_1b <- stan_glm(log_earn ~ height + male, data = earnings)
sims_1b <- as.matrix(fit_1b)
print(fit_1a, digits=2)
print(fit_1b, digits=2)

loo1b<-loo(fit_1b)
n=length(fit_1b$y)
elpd0_pointwise=dt((fit_1b$y-mean(fit_1b$y))/(sqrt(1+1/n)*sd(fit_1b$y)), df = n-1, log = TRUE)-log(sqrt(1+1/n)*sd(fit_1b$y))
elpd0_pointwise=sum(dnorm(fit_1b$y,mean=mean(fit_1b$y),sd=sd(fit_1b$y), log = TRUE))
elpd0=sum(elpd0_pointwise)
sdelpd0=sd(elpd0_pointwise)*sqrt(n)
elpd0diff=sum(loo1b$pointwise[,"elpd_loo"]-elpd0_pointwise)
sdelpd0diff=sd(loo1b$pointwise[,"elpd_loo"]-elpd0_pointwise)*sqrt(n)
sprintf("The baseline by guessing the larger class elpd_loo %.1f sd %.1f",elpd0,sdelpd0)
sprintf("The difference to baseline elpd_diff %.1f sd %.1f",elpd0diff,sdelpd0diff)

# PSIS-LOO weights
log_lik=log_lik(fit_1b)
psis=psislw(-log_lik)
# posterior predictive sample
preds <- posterior_linpred(fit_1b)
# LOO predictive mean
predloo=E_loo(preds,psis$lw_smooth)
# LOO Bayesian R2
R2loo=var(predloo)/(var(predloo)+var(fit_1b$y-predloo))
sprintf('LOO residual sd = %.2f, LOO-R-Squared = %.2f',sd(fit_1b$y-predloo),R2loo)
