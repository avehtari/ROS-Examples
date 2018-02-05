setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/SIS")

## Regression-based imputation for the Social Indicators Survey

# General function for creating a completed data vector using imputations

impute <- function (a, a_impute){
  ifelse(is.na(a), a_impute, a)
}

# 1.  Deterministic imputation

# Impute 0 earnings using the logical rule (if worked 0 months and 0 hrs/wk)

zero_earnings <- workhrs==0 & workmos==0
earnings_top[zero_earnings] <- 0

# Create a little dataset with all our redefined variables!

SIS <- data.frame(cbind(earnings, earnings_top, male, over65,
  white, immig, educ_r, workmos, workhrs_top,
  any_ssi, any_welfare, any_charity))

# Impute subset of earnings that are nonzero:  linear scale

lm_imp_1 <- lm(earnings ~ male + over65 + white + immig +
  educ_r + workmos + workhrs_top + any_ssi + any_welfare + any_charity,
  data=SIS, subset=earnings>0)
display(lm_imp_1)
pred_1 <- predict(lm_imp_1, SIS)
earnings_imp_1 <- impute(earnings, pred_1)

# Impute subset of earnings that are nonzero:  square root scale and topcoding

lm_imp_2_sqrt <- stan_glm(I(sqrt(earnings_top)) ~ male + over65 + white + immig +
  educ_r + workmos + workhrs_top + any_ssi + any_welfare + any_charity,
  data=SIS, subset=earnings>0)
print(lm_imp_2_sqrt)
pred_2_sqrt <- posterior_predict(lm_imp_2_sqrt, SIS)
pred_2 <- topcode(pred_2_sqrt^2, 100)
earnings_imp_2 <- impute(earnings_top, pred_2)

###############################################################################

# 2.  Random imputation

# Linear scale (use fitted model lm_imp_1)

pred_3 <- rnorm(n, predict(lm_imp_1, SIS), sigma.hat(lm_imp_1))
earnings_imp_3 <- impute(earnings, pred_3)

# Square root scale and topcoding (use fitted model lm_imp_2)

pred_4_sqrt <- rnorm(n, predict(lm_imp_2_sqrt, SIS), sigma.hat(lm_imp_2_sqrt))
pred_4 <- topcode(pred_4_sqrt^2, 100)
earnings_imp_4 <- impute(earnings_top, pred_4)

###############################################################################

# 3.  Histograms and scatterplots of data and imputations

postscript("c:/books/multilevel/impute.hist2.ps", horizontal=TRUE)
par(mar=c(7,6,4,3)+.1)
hist(earnings_top[earnings>0], mgp=c(5,2,0),
      breaks=seq(0,100,10), xlab="earnings", ylab="", cex.lab=3,
      cex.axis=3, cex.main=3, main="Observed earnings (excluding 0's)")
dev.off()

postscript("c:/books/multilevel/impute.hist3.ps", horizontal=TRUE)
par(mar=c(7,6,4,3)+.1)
hist(earnings_imp_2[is.na(earnings)], breaks=seq(0,100,10),mgp=c(5,2,0),
      xlab="earnings", ylab="", cex.lab=3, ylim=c(0,48),
      cex.axis=3, cex.main=3, main="Deterministic imputation of earnings")
dev.off()

postscript("c:/books/multilevel/impute.hist4.ps", horizontal=TRUE)
par(mar=c(7,6,4,3)+.1)
hist(earnings_imp_4[is.na(earnings)], breaks=seq(0,100,10),mgp=c(5,2,0),
      xlab="earnings", ylab="", cex.lab=3, ylim=c(0,48),
      cex.axis=3, cex.main=3, main="Random imputation of earnings")
dev.off()

postscript("c:/books/multilevel/impute.scat1.ps", horizontal=TRUE)
par(mar=c(5,5,4,2)+.1)
plot(range(earnings_imp_2[is.na(earnings)]), c(0,100),
      xlab="Regression prediction", ylab="Imputed income",
      cex.lab=2.5, cex.axis=2.5, cex.main=2.5,
      main="Deterministic imputation", type="n")
points(earnings_imp_2[is.na(earnings)], earnings_imp_2[is.na(earnings)], cex=1.5, pch=19)
points(pred_2[earnings>0], earnings[earnings>0], pch=20, col="darkgray")
dev.off()

postscript("c:/books/multilevel/impute.scat2.ps", horizontal=TRUE)
par(mar=c(5,5,4,2)+.1)
plot(range(earnings_imp_2[is.na(earnings)]), c(0,100),
      xlab="Regression prediction", ylab="Imputed income",
      cex.lab=2.5, cex.axis=2.5, cex.main=2.5,
      main="Random imputation", type="n")
points(earnings_imp_2[is.na(earnings)], earnings_imp_4[is.na(earnings)], cex=1.5, pch=19)
points(pred_2[earnings>0], earnings[earnings>0], pch=20, col="darkgray")
dev.off()

###############################################################################

# 4.  Two-stage imputation model

# Fit the 2 models

glm_sign <- glm(I(earnings>0) ~ male + over65 + white + immig +
  educ_r + any_ssi + any_welfare + any_charity,
  data=SIS, family=binomial(link=logit))
display(glm_sign)
lm_ifpos_sqrt <- lm(I(sqrt(earnings_top)) ~ male + over65 + white + immig +
  educ_r + any_ssi + any_welfare + any_charity,
  data=SIS, subset=earnings>0)  # (same as lm_imp_2 from above)
display(lm_ifpos_sqrt)

# Predict the sign and then the earnings (if positive)

pred_sign <- rbinom(n, 1, predict(glm_sign, SIS, type="response"))
pred_pos_sqrt <- rnorm(n, predict(lm_ifpos_sqrt, SIS),
  sigma.hat(lm_ifpos_sqrt))
pred_pos <- topcode(pred_pos_sqrt^2, 100)
earnings_imp <- impute(earnings, pred_sign*pred_pos)

###############################################################################

# 5.  Iterative regression imputation

# starting values

interest_imp <- random_imp(interest)
earnings_imp <- random_imp(earnings)

# simplest regression imputation

n_sims <- 10
for (s in 1:n_sims){
  lm_1 <- lm(earnings ~ interest_imp + male + over65 + white +
    immig + educ_r + workmos + workhrs_top + any_ssi + any_welfare +
    any_charity)
  pred_1 <- rnorm(n, predict(lm_1), sigma.hat(lm_1))
  earnings_imp <- impute(earnings, pred_1)

  lm_1 <- lm(interest ~ earnings_imp + male + over65 + white +
    immig + educ_r + workmos + workhrs_top + any_ssi + any_welfare +
    any_charity)
  pred_1 <- rnorm(n, predict(lm_1), sigma.hat(lm_1))
  interest_imp <- impute(interest, pred_1)
}

