setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/Imputation")
source("imputation_setup.R")
library("rstanarm")
options(mc.cores = parallel::detectCores())

## Regression-based imputation for the Social Indicators Survey

## General function for creating a completed data vector using imputations

impute <- function(a, a_impute){
  ifelse(is.na(a), a_impute, a)
}

## 1.  Deterministic imputation

## Impute 0 earnings using the logical rule (if worked 0 months and 0 hrs/wk)

zero_earnings <- workhrs==0 & workmos==0
earnings_top[zero_earnings] <- 0

## Create a little dataset with all our redefined variables!

n <- length(earnings)
SIS <- data.frame(earnings, earnings_top, male, over65,
                  white, immig, educ_r, workmos, workhrs_top,
                  any_ssi, any_welfare, any_charity)
SIS_predictors <- SIS[,3:ncol(SIS)]

## Impute subset of earnings that are nonzero:  linear scale

fit_imp_1 <- stan_glm(
  earnings ~ male + over65 + white + immig + educ_r +
              workmos + workhrs_top + any_ssi +
              any_welfare + any_charity,
  data = SIS,
  subset = earnings > 0
)
print(fit_imp_1)
pred_1 <- colMeans(posterior_linpred(fit_imp_1, SIS_predictors))  # point predictions
earnings_imp_1 <- impute(earnings, pred_1)

## Impute subset of earnings that are nonzero:  square root scale and topcoding

fit_imp_2 <- stan_glm(
  sqrt(earnings_top) ~ male + over65 + white + immig +
                       educ_r + workmos + workhrs_top + any_ssi +
                       any_welfare + any_charity,
  data = SIS,
  subset = earnings > 0
)
print(fit_imp_2)
pred_2_sqrt <- colMeans(posterior_linpred(fit_imp_2, SIS_predictors))  # point predictions
pred_2 <- topcode(pred_2_sqrt^2, 100)
earnings_imp_2 <- impute(earnings_top, pred_2)

###############################################################################

## 2.  One random imputation

## Linear scale (use fitted model lm_imp_1)

pred_3 <- posterior_predict(fit_imp_1, SIS_predictors)
earnings_imp_3 <- impute(earnings, pred_3[1,])

## Square root scale and topcoding (use fitted model lm_imp_2)

pred_4_sqrt <- posterior_predict(fit_imp_2, SIS_predictors)
pred_4 <- topcode(pred_4_sqrt^2, 100)
earnings_imp_4 <- impute(earnings_top, pred_4[1,])

###############################################################################

## 3.  Histograms and scatterplots of data and imputations

pdf("impute_hist2.pdf", height=4, width=5.5)
par(mar=c(3,3,1,1), mgp=c(1.7,.5,0), tck=-.01)
hist(earnings_top[earnings>0], breaks=seq(0,100,10), xlab="earnings", ylab="", main="Observed earnings (excluding 0's)")
dev.off()

pdf("impute_hist3.pdf", height=4, width=5.5)
par(mar=c(3,3,1,1), mgp=c(1.7,.5,0), tck=-.01)
hist(earnings_imp_2[is.na(earnings)], breaks=seq(0,100,10),
      xlab="earnings", ylab="", ylim=c(0,48),
      main="Deterministic imputation of earnings")
dev.off()

pdf("impute_hist4.pdf", height=4, width=5.5)
par(mar=c(3,3,1,1), mgp=c(1.7,.5,0), tck=-.01)
hist(earnings_imp_4[is.na(earnings)], breaks=seq(0,100,10),
      xlab="earnings", ylab="", ylim=c(0,48),
     main="Random imputation of earnings")
dev.off()

pdf("impute_scat_1.pdf", height=4, width=5)
par(mar=c(3,3,2,1), mgp=c(1.7,.5,0), tck=-.01)
plot(range(earnings_imp_2[is.na(earnings)]), c(0,100),
      xlab="Regression prediction", ylab="Imputed income",
      main="Deterministic imputation", type="n", bty="l")
points(earnings_imp_2[is.na(earnings)], earnings_imp_2[is.na(earnings)], pch=19, cex=.5)
points(pred_2[earnings>0], earnings[earnings>0], pch=20, col="darkgray", cex=.5)
dev.off()


pdf("impute_scat_2.pdf", height=4, width=5)
par(mar=c(3,3,2,1), mgp=c(1.7,.5,0), tck=-.01)
plot(range(earnings_imp_2[is.na(earnings)]), c(0,100),
      xlab="Regression prediction", ylab="Imputed income",
      main="Random imputation", type="n", bty="l")
points(earnings_imp_2[is.na(earnings)], earnings_imp_4[is.na(earnings)], pch=19, cex=.5)
points(pred_2[earnings>0], earnings[earnings>0], pch=20, col="darkgray", cex=.5)
dev.off()

###############################################################################

## 4.  Two-stage imputation model

## Fit the 2 models

fit_positive <- stan_glm((earnings>0) ~ male + over65 + white + immig +
  educ_r + any_ssi + any_welfare + any_charity,
  data=SIS, family=binomial(link=logit))
print(fit_positive)
fit_positive_sqrt <- stan_glm(sqrt(earnings_top) ~ male + over65 + white + immig +
  educ_r + any_ssi + any_welfare + any_charity,
  data=SIS, subset=earnings>0)  # (same as fit_imp_2 from above)
print(fit_positive_sqrt)

# Predict the sign and then the earnings (if positive)

pred_sign <- posterior_predict(fit_positive, SIS_predictors)[1,]  # one random imp
pred_pos_sqrt <- posterior_predict(fit_positive_sqrt, SIS_predictors)[1,]  # one random imp
pred_pos <- topcode(pred_pos_sqrt^2, 100)
earnings_imp <- impute(earnings, pred_sign*pred_pos)

###############################################################################

## 5.  Iterative regression imputation

## starting values

interest_imp <- random_imp(interest)
earnings_imp <- random_imp(earnings)
SIS <- cbind(SIS, interest_imp, earnings_imp)

## simplest regression imputation

n_sims <- 10
for (s in 1:n_sims){
  fit <- stan_glm(earnings ~ interest_imp + male + over65 + white +
    immig + educ_r + workmos + workhrs_top + any_ssi + any_welfare +
    any_charity, data=SIS)
  SIS_predictors <- SIS[,3:ncol(SIS)]
  pred <- posterior_predict(fit, SIS_predictors)[1,]
  SIS$earnings_imp <- impute(earnings, pred)

  fit <- stan_glm(interest ~ earnings_imp + male + over65 + white +
    immig + educ_r + workmos + workhrs_top + any_ssi + any_welfare +
    any_charity, data=SIS)
  SIS_predictors <- SIS[,3:ncol(SIS)]
  pred <- posterior_predict(fit, SIS_predictors)[1,]
  SIS$interest_imp <- impute(interest, pred)
}

