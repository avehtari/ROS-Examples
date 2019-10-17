#' ---
#' title: "Regression and Other Stories: SIS"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Regression-based imputation for the Social Indicators Survey
#' 
#' -------------
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(message=FALSE, error=FALSE, warning=FALSE, comment=NA)

#' **Load packages**
library("rprojroot")
root<-has_dirname("ROS-Examples")$make_fix_file()
library("rstanarm")
options(mc.cores = parallel::detectCores())
library("dplyr")
library("ggplot2")
theme_set(bayesplot::theme_default(base_family = "sans"))

#' **Helper functions**
#' 
#' General function for creating a completed data vector using imputations
impute <- function (a, a_impute){
  ifelse(is.na(a), a_impute, a)
}
#' Top coding
topcode <- function (a, top){
  return(ifelse(a>top, top, a))
}
#' se missing codes to NA
na_fix <- function (a) {
  ifelse(a<0 | a==999999, NA, a)
}
is_any <- function (a) {
  any_a <- ifelse(a>0, 1, 0)
  any_a[is.na(a)] <- 0
  return(any_a)
}

#' **Load data**
wave3 <- as.data.frame(read.table(root("SIS/data","siswave3v4impute3.csv"),
                                  header=TRUE, sep=","))
n <- nrow(wave3)
# set up some simplified variables to work with
wave3 <- mutate(wave3,
  white = if_else(race==1, 1, 0, missing=0),
  male = if_else(sex==1, 1, 0),
  over65 = if_else(r_age>65, 1, 0),
  no_earners = if_else(earners==0, 1, 0),
  workhrs_top = topcode(workhrs, 40),
  immig = if_else(is.na(immig), 0L, immig),
  educ_r = if_else(is.na(educ_r), 2.5, as.double(educ_r)),
  earners = if_else(is.na(earners), 1L, earners),
  earnings_orig = na_fix(rearn) + na_fix(tearn),
  earnings = if_else(workmos==0, 0L, earnings_orig),
  retirement = na_fix(socsec) + na_fix(pension),
  interest = na_fix(interest),
  assistance = na_fix(unemp) + na_fix(ssi) + na_fix(welfare) + na_fix(charity),
  other  = na_fix(alimony) + na_fix(giftmon),
  # any positive not na
  any_unemp = if_else(unemp>0, 1, 0, missing=0),
  any_ssi = if_else(ssi>0, 1, 0, missing=0),
  any_welfare = if_else(welfare>0, 1, 0, missing=0),
  any_charity = if_else(charity>0, 1, 0, missing=0),
  # transforming and 
  earnings = earnings/1000,
  retirement = retirement/1000,
  interest = interest/1000,
  assistance = assistance/1000,
  other = other/1000,
  # topcoding the different sources of income
  earnings_top = topcode(earnings, 100),
  retirement_top = topcode(retirement, 100),
  interest_top = topcode(interest, 100),
  assistance_top = topcode(assistance, 10),
  other_top = topcode(other, 10))

#' ## 1.  Deterministic imputation

#' **Impute 0 earnings using the logical rule (if worked 0 months and 0 hrs/wk)**
wave3 <- mutate(wave3,
  zero_earnings = (workhrs==0 & workmos==0),
  earnings_top = if_else(zero_earnings, 0, earnings_top))

#' **Create a little dataset with all our redefined variables**
SIS <- select(wave3,
              earnings, earnings_top, male, over65,
              white, immig, educ_r, workmos, workhrs_top,
              any_ssi, any_welfare, any_charity)

#' **Impute subset of earnings that are nonzero:  linear scale**
SIS_pos <- filter(SIS, earnings>0)
imp_1 <- stan_glm(earnings ~ male + over65 + white + immig +
  educ_r + workmos + workhrs_top + any_ssi + any_welfare + any_charity,
  data=SIS_pos)
print(imp_1)
SIS_predictors <- select(SIS, -starts_with("earnings"))
pred_1 <- colMeans(posterior_predict(imp_1, newdata = SIS_predictors))
SIS <- mutate(SIS,
  pred_1 = pred_1,
  earnings_imp_1 = impute(earnings, pred_1))

#' **Impute subset of earnings that are nonzero:  square root scale and topcoding**
imp_2_sqrt <- stan_glm(I(sqrt(earnings_top)) ~ male + over65 + white + immig +
  educ_r + workmos + workhrs_top + any_ssi + any_welfare + any_charity,
  data=SIS_pos)
print(imp_2_sqrt)
SIS_predictors <- select(SIS, -starts_with("earnings"))
pred_2_sqrt <- colMeans(posterior_predict(imp_2_sqrt, newdata = SIS_predictors))
SIS <- mutate(SIS,
  pred_2 = topcode(pred_2_sqrt^2, 100),
  earnings_imp_2 = impute(earnings_top, pred_2))

#' ## 2.  Random imputation

#' **Linear scale (use fitted model fit_imp_1)**
pred_3 <- rnorm(n, pred_1, sigma(imp_1))
SIS <- mutate(SIS,
  earnings_imp_3 = impute(earnings, pred_3))

#' **Square root scale and topcoding (use fitted model fit_imp_2)**
pred_4_sqrt <- rnorm(n, pred_2_sqrt, sigma(imp_2_sqrt))
SIS <- mutate(SIS,
  pred_4 = topcode(pred_4_sqrt^2, 100),
  earnings_imp_4 = impute(earnings_top, pred_4))

#' ## 3.  Histograms and scatterplots of data and imputations
#'
#' Observed earnings (excluding 0's)
qplot(earnings_top, data=SIS_pos, breaks=seq(0,100,10),
      fill=I("white"), col=I("black")) +
    xlab("earnings") + ggtitle("Observed earnings (excluding 0's)")
#+ eval=FALSE, include=FALSE
ggsave(root("SIS/figs","impute.hist2.gg.pdf"), width = 5, height = 4)

qplot(earnings_imp_2, data = filter(SIS, is.na(earnings)),
     breaks=seq(0,100,10), fill=I("white"), col=I("black")) +
    xlab("earnings") + ggtitle("Deterministic imputation of earnings")
#+ eval=FALSE, include=FALSE
ggsave(root("SIS/figs","impute.hist3.gg.pdf"), width = 5, height = 4)

#' Random imputation of earnings
qplot(earnings_imp_4, data = filter(SIS, is.na(earnings)),
     breaks=seq(0,100,10), fill=I("white"), col=I("black")) +
    xlab("earnings") + ggtitle("Random imputation of earnings")
#+ eval=FALSE, include=FALSE
ggsave(root("SIS/figs","impute.hist4.gg.pdf"), width = 5, height = 4)

#' Deterministic imputation scatter plot
ggplot() + 
    geom_point(aes(x=earnings_imp_2, y=earnings_imp_2), shape=19,
               data = filter(SIS, is.na(earnings))) +
    geom_point(aes(x=pred_2, y=earnings_top), shape=20, color="darkgrey",
               data=filter(SIS,earnings>0)) +
    xlab("Regression prediction") + ylab("Imputed income") +
    ggtitle("Deterministic imputation")
#+ eval=FALSE, include=FALSE
ggsave(root("SIS/figs","impute.scat1.gg.pdf"), width = 5, height = 4)

#' Random imputation scatter plot
ggplot() + 
    geom_point(aes(x=earnings_imp_2, y=earnings_imp_4), shape=19,
               data = filter(SIS, is.na(earnings))) +
    geom_point(aes(x=pred_2, y=earnings_top), shape=20, color="darkgrey",
               data=filter(SIS,earnings>0)) +
    xlab("Regression prediction") + ylab("Imputed income") +
    ggtitle("Random imputation")
#+ eval=FALSE, include=FALSE
ggsave(root("SIS/figs","impute.scat2.gg.pdf"), width = 5, height = 4)

###############################################################################

#' ## 4. Two-stage imputation model

#' **Fit the 2 models**
fit_pos <- stan_glm(I(earnings>0) ~ male + over65 + white + immig +
  educ_r + any_ssi + any_welfare + any_charity,
  data=SIS, family=binomial(link=logit))
print(fit_pos, digits=2)
# 2nd model was alraedy fitted before
fit_ifpos_sqrt <- imp_2_sqrt
print(fit_ifpos_sqrt)

#' **Predict the sign and then the earnings (if positive)**
SIS_predictors <- select(SIS, -starts_with("earnings"))
pred_pos <- posterior_predict(fit_pos, draws = 1, newdata = SIS_predictors)
pred_ifpos_sqrt <- posterior_predict(fit_ifpos_sqrt, draws = 1, 
                                     newdata = SIS_predictors)
pred_ifpos <- topcode(pred_ifpos_sqrt^2, 100)
SIS <- mutate(SIS,
 earnings_imp = impute(earnings, pred_pos*pred_ifpos))

#' ## 5. Iterative regression imputation

#' **Starting values**
random_imp <- function (a){
  missing <- is.na(a)
  n_missing <- sum(missing)
  a_obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample(a_obs, n_missing)
  return(imputed)
}
SIS$interest <- wave3$interest
SIS <- mutate(SIS,
 interest_imp = random_imp(interest),
 earnings_imp = random_imp(earnings))

#' **Simplest regression imputation**
n_sims <- 10
for (s in 1:n_sims) {
  # Predict earnings
  output <- capture.output(
      fit_1 <- stan_glm(earnings ~ interest_imp + male + over65 + white +
                            immig + educ_r + workmos + workhrs_top + any_ssi +
                            any_welfare + any_charity,
                        data = SIS, cores = 1, open_progress = FALSE))
  pred_1 <- posterior_predict(fit_1, draw = 1)
  # Impute earnings
  SIS <- mutate(SIS,
    earnings_imp = impute(earnings, pred_1))
  # Predict interest
  output <- capture.output(
    fit_1 <- stan_glm(interest ~ earnings_imp + male + over65 + white +
                          immig + educ_r + workmos + workhrs_top + any_ssi +
                          any_welfare + any_charity,
                      data = SIS, cores = 1, open_progress = FALSE))
  pred_1 <- posterior_predict(fit_1, draw = 1)
  # Impute interest
  SIS <- mutate(SIS,
    interest_imp = impute(interest, pred_1))
}

