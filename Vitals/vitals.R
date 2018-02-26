#' ---
#' title: "Regression and Other Stories: Vitals"
#' author: "Andrew Gelman, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Predict weight
#' 
#' -------------
#' 

#' **Load libraries**
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library("here")
library("arm")
library("rstanarm")
options(mc.cores = parallel::detectCores())

#' **Load data**
vitals_full <- read.csv(here("Vitals/data","vitals.csv"))
minidata <- vitals_full[, c("weight","height","female","ethnicity","exercise","smokenow")]
ok <- apply(is.na(minidata), 1, sum) == 0
vitals <- minidata[ok,]

#' **Show some data**
print(vitals[1:5,])

#' ### Simulating uncertainty for linear predictors and predicted values

#' **Predict weight (in pounds) from height (in inches)**<br/>
#' **Bayesian regression**
fit_1 <- stan_glm(weight ~ height, data=vitals)
print(fit_1)

#' **Center weights**
vitals$c_height <- vitals$height - 66
fit_2 <- stan_glm(weight ~ c_height, data=vitals)
print(fit_2)

#' **Posterior simulations**
new <- data.frame(c_height=4.0)
linpred_2 <- posterior_linpred(fit_2, newdata=new)
postpred_2 <- posterior_predict(fit_2, newdata=new)

#' ### Indicator variables

#' **Predict weight (in pounds) from height (in inches)**<br/>
#' **Classical regression**
M_1 <- lm(weight ~ height, data=vitals)
display(M_1, digits=1)
coefs_1 <- coef(M_1)
predicted <- coefs_1[1] + coefs_1[2]*66

#' **Bayesian regression**
M_1a <- stan_glm(weight ~ height, data=vitals)
new <- data.frame(height=66)
pred <- posterior_predict(M_1a, newdata=new)
cat("Predicted weight for a 66-inch-tall person is", round(mean(pred)), "pounds with a sd of", round(sd(pred)), "\n")

#' **Center weights**
vitals$c_height <- vitals$height - 66
display(lm(weight ~ c_height, data=vitals), digits=1)

#' **Including a binary variable in a regression -- classical**
M_2 <- lm(weight ~ c_height + female, data=vitals)
display(M_2, digits=1)
coefs_2 <- coef(M_2)
predicted <- coefs_2[1] + coefs_2[2]*4 + coefs_2[3]*1

#' **Including a binary variable in a regression -- Bayesian**
M_2a <- stan_glm(weight ~ c_height + female, data=vitals)
new <- data.frame(c_height=4, female=1)
pred <- posterior_predict(M_2a, newdata=new)
print(mean(pred))

#' **Using indicator variables for multiple levels of a categorical predictor**<br/>
#' Include ethnicity in the regression as a factor
M_3 <- lm(weight ~ c_height + female + factor(ethnicity), data=vitals)
display(M_3, digits=1)

#' Choose the baseline category by setting the levels
vitals$eth <- factor(vitals$ethnicity,
  levels=c("white", "black", "hispanic", "other"))
M_4 <- lm(weight ~ c_height + female + eth, data=vitals)
display(M_4)

#' Alternatively create indicators for the four ethnic groups directly:
vitals$eth_white <- ifelse(vitals$ethnicity=="white", 1, 0)
vitals$eth_black <- ifelse(vitals$ethnicity=="black", 1, 0)
vitals$eth_hispanic <- ifelse(vitals$ethnicity=="hispanic", 1, 0)
vitals$eth_other <- ifelse(vitals$ethnicity=="other", 1, 0)
M_5 <- lm(weight ~ c_height + female + eth_black + eth_hispanic + eth_other, data=vitals)
display(M_5)
