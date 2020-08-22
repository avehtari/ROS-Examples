#' ---
#' title: "Regression and Other Stories: Imputation"
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

#' Regression-based imputation for the Social Indicators Survey. See
#' Chapter 17 in Regression and Other Stories.<br>
#' dplyr + ggplot version. 
#' 
#' -------------
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(message=FALSE, error=FALSE, warning=FALSE, comment=NA)
# switch this to TRUE to save figures in separate files
savefigs <- FALSE

#' **Load packages**
library("rprojroot")
root<-has_file(".ROS-Examples-root")$make_fix_file()
library("rstanarm")
library("dplyr")
library("ggplot2")
theme_set(bayesplot::theme_default(base_family = "sans"))

#' **Load data**
SIS <- read.csv(root("Imputation/data","SIS.csv"))
head(SIS)
summary(SIS)

#' **Imputation helper functions**</br>
#' Create a completed data vector using imputations
impute <- function(a, a_impute) {
  ifelse(is.na(a), a_impute, a)
}
#' Top code function
topcode <- function(a, top) {
  ifelse(a>top, top, a)
}
#' se missing codes to NA
na_fix <- function(a) {
  ifelse(a<0 | a==999999, NA, a)
}
is_any <- function(a) {
  any_a <- ifelse(a>0, 1, 0)
  any_a[is.na(a)] <- 0
  any_a
}

#' ### 1.  Deterministic imputation

#' **Impute 0 earnings using the logical rule (if worked 0 months and 0 hrs/wk)**
SIS <- mutate(SIS,
  zero_earnings = (workhrs_top==0 & workmos==0),
  earnings_top = if_else(zero_earnings, 0, topcode(earnings, 100)))

#' **Create a little dataset with all our redefined variables**
SIS <- select(SIS,
              earnings, earnings_top, interest, male, over65,
              white, immig, educ_r, workmos, workhrs_top,
              any_ssi, any_welfare, any_charity)

#' **Impute subset of earnings that are nonzero:  linear scale**
fit_imp_1 <- stan_glm(
  earnings ~ male + over65 + white + immig + educ_r +
              workmos + workhrs_top + any_ssi +
              any_welfare + any_charity,
  data = SIS,
  subset = earnings > 0,
  refresh = 0
)
print(fit_imp_1)
SIS_predictors <- select(SIS, -starts_with("earnings"))
pred_1 <- colMeans(posterior_linpred(fit_imp_1, newdata = SIS_predictors))
SIS <- mutate(SIS,
  earnings_imp_1 = impute(earnings, pred_1))

#' **Impute subset of earnings that are nonzero:  square root scale and topcoding**
fit_imp_2 <- stan_glm(
  sqrt(earnings_top) ~ male + over65 + white + immig +
                       educ_r + workmos + workhrs_top + any_ssi +
                       any_welfare + any_charity,
  data = SIS,
  subset = earnings > 0,
  refresh = 0
)
print(fit_imp_2)
#' point predictions
pred_2_sqrt <- colMeans(posterior_linpred(fit_imp_2, newdata = SIS_predictors))
pred_2 <- topcode(pred_2_sqrt^2, 100)
SIS <- mutate(SIS,
  pred_2 = pred_2,
  earnings_imp_2 = impute(earnings_top, pred_2))

#' ### 2.  Random imputation

#' **Linear scale (use fitted model fit_imp_1)**
pred_3 <- posterior_predict(fit_imp_1, newdata = SIS_predictors, draws = 1)
SIS <- mutate(SIS,
  earnings_imp_3 = impute(earnings, pred_3))

#' **Square root scale and topcoding (use fitted model fit_imp_2)**
pred_4_sqrt <- posterior_predict(fit_imp_2, newdata = SIS_predictors, draws = 1)
pred_4 <- topcode(pred_4_sqrt^2, 100)
SIS <- mutate(SIS,
  earnings_imp_4 = impute(earnings_top, pred_4))

#' ### 3.  Histograms and scatterplots of data and imputations
#'
#' Observed earnings (excluding 0's)
qplot(earnings_top, data=filter(SIS, !is.na(earnings)), breaks=seq(0,100,10),
      fill=I("white"), col=I("black")) +
    xlab("earnings") + ggtitle("Observed earnings (excluding 0's)")
#+ eval=FALSE, include=FALSE
if (savefigs) ggsave(root("Imputation/figs","impute_hist2_gg.pdf"), width = 5, height = 4)
qplot(earnings_imp_2, data = filter(SIS, !is.na(earnings)),
     breaks=seq(0,100,10), fill=I("white"), col=I("black")) +
    xlab("earnings") + ggtitle("Deterministic imputation of earnings")
#+ eval=FALSE, include=FALSE
if (savefigs) ggsave(root("Imputation/figs","impute_hist3_gg.pdf"), width = 5, height = 4)

#' Random imputation of earnings
qplot(earnings_imp_4, data = filter(SIS, !is.na(earnings)),
     breaks=seq(0,100,10), fill=I("white"), col=I("black")) +
    xlab("earnings") + ggtitle("Random imputation of earnings")
#+ eval=FALSE, include=FALSE
if (savefigs) ggsave(root("Imputation/figs","impute_hist4_gg.pdf"), width = 5, height = 4)

#' Deterministic imputation scatter plot
ggplot() + 
    geom_point(aes(x=earnings_imp_2, y=earnings_imp_2), shape=19,
               data = filter(SIS, !is.na(earnings))) +
    geom_point(aes(x=pred_2, y=earnings_top), shape=20, color="darkgrey",
               data = filter(SIS, !is.na(earnings))) +
    xlab("Regression prediction") + ylab("Earnings") +
    ggtitle("Deterministic imputation")
#+ eval=FALSE, include=FALSE
if (savefigs) ggsave(root("Imputation/figs","impute_scat1_gg.pdf"), width = 5, height = 4)

#' Random imputation scatter plot
ggplot() + 
    geom_point(aes(x=earnings_imp_2, y=earnings_imp_4), shape=19,
               data = filter(SIS, !is.na(earnings))) +
    geom_point(aes(x=pred_2, y=earnings_top), shape=20, color="darkgrey",
               data = filter(SIS, !is.na(earnings))) +
    xlab("Regression prediction") + ylab("Earnings") +
    ggtitle("Random imputation")
#+ eval=FALSE, include=FALSE
if (savefigs) ggsave(root("Imputation/figs","impute_scat2_gg.pdf"), width = 5, height = 4)

#' ### 4. Two-stage imputation model

#' **Fit the 2 models**
fit_positive <- stan_glm((earnings>0) ~ male + over65 + white + immig +
  educ_r + any_ssi + any_welfare + any_charity,
  data=SIS, family=binomial(link=logit), refresh=0)
print(fit_positive, digits=2)
# 2nd model was alraedy fitted before
fit_positive_sqrt <- fit_imp_2
print(fit_positive_sqrt)

#' **Predict the sign and then the earnings (if positive)**
pred_pos <- posterior_predict(fit_positive, newdata = SIS_predictors, draws = 1)
pred_ifpos_sqrt <- posterior_predict(fit_positive_sqrt, newdata = SIS_predictors, draws = 1)
pred_ifpos <- topcode(pred_ifpos_sqrt^2, 100)
SIS <- mutate(SIS,
 earnings_imp = impute(earnings, pred_pos*pred_ifpos))

#' ### 5.  Iterative regression imputation

#' **Starting values**
random_imp <- function (a){
  missing <- is.na(a)
  n_missing <- sum(missing)
  a_obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample(a_obs, n_missing)
  imputed
}
SIS <- mutate(SIS,
 interest_imp = random_imp(interest),
 earnings_imp = random_imp(earnings))

#' **Simplest regression imputation**
n_sims <- 10
for (s in 1:n_sims) {
  # Predict earnings
  output <- capture.output(
      fit <- stan_glm(earnings ~ interest_imp + male + over65 + white +
                            immig + educ_r + workmos + workhrs_top + any_ssi +
                            any_welfare + any_charity,
                        data = SIS, cores = 1, open_progress = FALSE, refresh=0))
  pred1 <- posterior_predict(fit, draws = 1)
  # Impute earnings
  SIS <- mutate(SIS,
    earnings_imp = impute(earnings, pred1))
  # Predict interest
  output <- capture.output(
    fit <- stan_glm(interest ~ earnings_imp + male + over65 + white +
                          immig + educ_r + workmos + workhrs_top + any_ssi +
                          any_welfare + any_charity,
                      data = SIS, cores = 1, open_progress = FALSE, refresh=0))
  pred2 <- posterior_predict(fit, draws = 1)
  # Impute interest
  SIS <- mutate(SIS,
    interest_imp = impute(interest, pred2))
}

