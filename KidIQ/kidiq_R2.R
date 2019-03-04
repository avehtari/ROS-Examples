#' ---
#' title: "Regression and Other Stories: KidIQ Bayes-R2 and LOO-R2"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Linear regression and Bayes-R2 and LOO-R2
#' 
#' See also Gelman, Goodrich, Gabry, and Vehtari (2018). R-squared for
#' Bayesian regression models. The American Statistician,
#' [doi:10.1080/00031305.2018.1549100](https://doi.org/10.1080/00031305.2018.1549100)
#'
#' -------------
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(message=FALSE, error=FALSE, warning=FALSE, comment=NA)

#' **Load packages**
library("rprojroot")
root<-has_dirname("RAOS-Examples")$make_fix_file()
library("rstanarm")
options(mc.cores = parallel::detectCores())
library("loo")
library("foreign")
library("ggplot2")
theme_set(bayesplot::theme_default(base_family = "sans"))
library(reshape2)
# for reproducability
SEED<-1507

#' **Bayesian-R2** is defined in rstanarm
#' 
#' **LOO-R2**
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

#' ### Compare different models with LOO-CV
#' 

#' **Load children's test scores data**
kidiq <- read.dta(file=root("KidIQ/data","kidiq.dta"))

#' **A single binary predictor**
fit_1 <- stan_glm(kid_score ~ mom_hs, data=kidiq,
                  seed=SEED, refresh=0)
loo_1 <- loo(fit_1)
print(loo_1)

#' **Two predictors**
fit_3 <- stan_glm(kid_score ~ mom_hs + mom_iq, data=kidiq,
                  seed=SEED, refresh=0)
loo_3 <- loo(fit_3)
print(loo_3)

#' **Compare models based on expected log predictive density**
compare_models(loo_1, loo_3)

#' **Bayes-R2**<br>
#' Median Bayes-R2 increases
br2_1<-bayes_R2(fit_1)
br2_3<-bayes_R2(fit_3)
round(median(br2_1),3)
round(median(br2_3),3)

#' **Plot Bayes-R2 posteriors**<br>
#' Increase in R2 is clear
df <- melt(data.frame(fit_3=br2_1,fit_3n=br2_3))
ggplot(df, aes(x=value, linetype=variable)) +
    geom_density(alpha=0.25, show.legend=FALSE) +
    labs(x="Bayes-R^2", y="") +
    scale_y_continuous(breaks=NULL) + 
    annotate("text", x = 0.107, y = 16.2, label = "kid_score ~ mom_hs") +
    annotate("text", x = 0.28, y = 13.2, label = "kid_score ~ mom_hs + mom_iq")

#' **R2 with LOO-CV**<br>
#' LOO-R2 are smaller, but the difference is still large.
round(looR2(fit_1),3)
round(looR2(fit_3),3)

#' **Add five pure noise predictors to the data
set.seed(SEED)
n=nrow(kidiq)
kidiqr <- kidiq
kidiqr$noise <- array(rnorm(5*n), c(n,5))

#' **Linear regression with additional noise predictors**
#+ results='hide'
fit_3n <- stan_glm(kid_score ~ mom_hs + mom_iq + noise, data=kidiqr,
                   seed=SEED, refresh=0)

#' **Linear regression with interaction**
#+ results='hide'
fit_4 <- stan_glm(kid_score ~ mom_hs + mom_iq + mom_iq:mom_hs, data=kidiq,
                  seed=SEED, refresh=0)

#' **Bayes-R2**<br>
#' Median Bayes-R2 increases, but...
br2_3<-bayes_R2(fit_3)
br2_3n<-bayes_R2(fit_3n)
round(median(br2_3),3)
round(median(br2_3n),3)

#' **Plot Bayes-R2 posteriors**<br>
#' Median Bayes-R2 increases, but that increase is negligible
#' compared to the uncertainty
df <- melt(data.frame(fit_3=br2_3,fit_3n=br2_3n))
ggplot(df, aes(x=value, linetype=variable)) +
    geom_density(alpha=0.25, show.legend=FALSE) +
    labs(x="Bayes-R^2", y="") +
    scale_y_continuous(breaks=NULL) + 
    annotate("text", x = 0.15, y = 11.5, label = "kid_score ~ mom_hs + mom_iq") +
    annotate("text", x = 0.285, y = 12, label = "kid_score ~ mom_hs + mom_iq +\n noise")

#' **R2 with LOO-CV**<br>
#' LOO-R2 decreases when five noise predictors are addded
round(looR2(fit_3),3)
round(looR2(fit_3n),3)
#' LOO-R2 increases when interaction is addded
round(looR2(fit_4),2)
