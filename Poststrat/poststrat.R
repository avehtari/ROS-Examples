setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/Poststrat")
library("rstanarm")
options(mc.cores = parallel::detectCores())
library("arm")

## The CBS News poll conducted from 12--16 October 2016 reported that, among likely voters who preferred one of the two major-party candidates, 45\% intended to vote for Donald Trump and 55\% for Hillary Clinton.  Of these respondents, Party ID 33\% Republican, 40\% Republican, 27\% independent.
## % source:  http://www.cbsnews.com/news/cbs-poll-clintons-lead-over-trump-widens-with-three-weeks-to-go/
## and https://www.scribd.com/document/327938789/CBS-News-Poll-10-17-toplines

## Effective sample size of likely voters
## 254 Republican, 282 Democrat, 242 Independent

## Compare to:
## exit polls 2012  32 38 29
## exit polls 2016  33 36 31

## Republicans:  77% Trump, 8% Clinton (must normalize to 100%)
## Democrats:  5% Trump, 89% Clinton (must normalize to 100%)
## Independents:  36% Trump, 38% Clinton (must normalize to 100%)

## Create some fake data
n_pid <- c(254, 282, 242)
n <- sum(n_pid)
pid_names <- c("Republican", "Democrat", "Independent")
pid <- rep(pid_names, n_pid)
n_vote <- as.list(rep(NA, 3))
n_vote[[1]] <- round(c(0.77, 0.08)*n_pid[1])
n_vote[[2]] <- round(c(0.05, 0.89)*n_pid[2])
n_vote[[3]] <- round(c(0.36, 0.38)*n_pid[3])
vote <- NULL
y_bar_cells <- rep(NA, 3)
for (j in 1:3){
  n_vote[[j]]<- c(n_vote[[j]], n_pid[j] - sum(n_vote[[j]]))
  vote <- c(vote, rep(c(1, 0, NA), n_vote[[j]]))
  y_bar_cells[j] <- mean(vote[pid==pid_names[j]], na.rm=TRUE)
  pfround(y_bar_cells[j], 3)
}

## Simple poststrat
pid_poststrat <- c(0.33, 0.36, 0.31)
pfround(sum(pid_poststrat * y_bar_cells), 3)

## LINEAR

## lm
lm_1 <- lm(vote ~ factor(pid))
display(lm_1, digits=2)

## Raw estimate
pfround(mean(vote, na.rm=TRUE), digits=3)

## Poststrat using predict()
poststrat_data <- data.frame(pid=c("Republican", "Democrat", "Independent"), N=c(0.33, 0.36, 0.31))
predict_1a <- predict(lm_1, newdata=poststrat_data)
est_1a <- sum(poststrat$N*predict_1a)/sum(poststrat$N)
pfround(est_1a, digits=3)

## stan_glm
fit_1 <- stan_glm(vote ~ factor(pid))
print(fit_1, digits=2)

## Poststrat using posterior_predict()
predict_1b <- posterior_linpred(fit_1, newdata=poststrat_data)
poststrat_est_1 <- predict_1b %*% poststrat_data$N/sum(poststrat_data$N)
print(c(mean(poststrat_est_1), mad(poststrat_est_1)))

## Add extra uncertainty
n_sim <- nrow(predict_1b)
poststrat_est_2 <- poststrat_est_1 + rnorm(n_sim, 0, 0.02)
print(c(mean(poststrat_est_2), mad(poststrat_est_2)))

## LOGISTIC

## Fit the regression
fit <- stan_glm(vote ~ factor(pid), family=binomial(link="logit"))
print(fit, digits=2)

## Raw estimate
pfround(mean(vote, na.rm=TRUE), digits=3)

## Poststrat using the groups
predict_rep <- invlogit(coef(fit)[1] + coef(fit)[3])
predict_dem <- invlogit(coef(fit)[1])
predict_ind <- invlogit(coef(fit)[1] + coef(fit)[2])
poststrat_est_1 <- predict_rep*pid_poststrat[1] + predict_dem*pid_poststrat[2] + predict_ind*pid_poststrat[3]
pfround(poststrat_est_1, digits=3)

## Poststrat using the predict function
X_population <- data.frame(pid=c("Republican", "Democrat", "Independent"))
N_population <- c(0.33, 0.36, 0.31)
predict_poststrat <- predict(fit, newdata=X_population, type="response")
poststrat_est_2 <- sum(N_population*predict_poststrat)/sum(N_population)
pfround(poststrat_est_2, digits=3)

## Just to compare, poststrat using the original data
predict_a <- predict(fit, type="response")
pfround(mean(predict_a), 3)
## This doesn't work--it just spits back the raw estimate--because it's not using the external population info which is what makes poststrat work.

## More complicated example with two predictors
## Imagine a model, fit <- stan_glm(vote ~ factor(pid) + male, family=binomial(link="logit"))
## The poststrat population matrix now has 6 rows and 2 columns:
## X_population <- data.frame(pid=rep(c("Republican", "Democrat", "Independent"), c(2,2,2)), male=rep(c(0,1), 2))
## N_population <- . . . [look it up]
## Continue...

## Quick test

N <- 5
x <- 1:N
y <- c(1,rep(0,N-1))
fit <- stan_glm(y ~ x, family=binomial(link="logit"))
print(fit)

pred_1 <- predict(fit, type="response")
print(pred_1)

X_new <- data.frame(x=1:N)
pred_2 <- predict(fit, newdata=X_new, type="response")


