#' ---
#' title: "Regression and Other Stories: Poststrat"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Poststratification after estimation
#' 
#' The CBS News poll conducted from 12--16 October 2016 reported that, among likely voters who preferred one of the two major-party candidates, 45\% intended to vote for Donald Trump and 55\% for Hillary Clinton.  Of these respondents, Party ID 33\% Republican, 40\% Republican, 27\% independent.
#' 
#' source:  http://www.cbsnews.com/news/cbs-poll-clintons-lead-over-trump-widens-with-three-weeks-to-go/
#' and https://www.scribd.com/document/327938789/CBS-News-Poll-10-17-toplines
#'
#' Effective sample size of likely voters
#' ```
#' 254 Republican, 282 Democrat, 242 Independent
#' ```
#'
#' Compare to:
#' ```
#' exit polls 2012  32 38 29
#' exit polls 2016  33 36 31
#' ```
#'
#' ```
#' Republicans:  77% Trump,  8% Clinton (must normalize to 100%)
#' Democrats:     5% Trump, 89% Clinton (must normalize to 100%)
#' Independents: 36% Trump, 38% Clinton (must normalize to 100%)
#' ```

#' -------------
#' 

#' **Load packages**
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library("rstanarm")
options(mc.cores = parallel::detectCores())

#' **Create some fake data**
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
  round(y_bar_cells[j], 3)
}
poll <- data.frame(vote, pid)

#' **Simple poststrat**
poststrat_data <- data.frame(pid=c("Republican", "Democrat", "Independent"),
                             N=c(0.33, 0.36, 0.31))
round(sum(poststrat_data$N * y_bar_cells), 3)

#' ### Linear

#' **Raw estimate**
round(mean(poll$vote, na.rm=TRUE), 3)

#' **stan_glm**
fit_1 <- stan_glm(vote ~ factor(pid), data = poll)
print(fit_1, digits=2)

#' **Poststrat using posterior_linpred()**
predict_1 <- posterior_linpred(fit_1, newdata=poststrat_data)
poststrat_est_1 <- predict_1 %*% poststrat_data$N/sum(poststrat_data$N)
print(c(mean(poststrat_est_1), mad(poststrat_est_1)), digits=2)

#' **Add extra uncertainty**
n_sim <- nrow(predict_1)
poststrat_est_2 <- poststrat_est_1 + rnorm(n_sim, 0, 0.02)
print(c(mean(poststrat_est_2), mad(poststrat_est_2)), digits=2)

#' ### Logistic

#' **Fit the regression**
fit <- stan_glm(vote ~ factor(pid), family=binomial(link="logit"), data = poll)
print(fit, digits=2)

#' **Raw estimate**
round(mean(poll$vote, na.rm=TRUE), 3)

#' **Poststrat using the predict function**
X_population <- data.frame(pid=c("Republican", "Democrat", "Independent"))
N_population <- c(0.33, 0.36, 0.31)
predict_poststrat <- colMeans(posterior_predict(fit, newdata=X_population))
poststrat_est_2 <- sum(N_population*predict_poststrat)/sum(N_population)
round(poststrat_est_2, digits=3)

#' **Just to compare, poststrat using the original data**
predict_a <- predict(fit, type="response")
round(mean(predict_a), 3)
#' This doesn't work--it just spits back the raw estimate--because it's not using the external population info which is what makes poststrat work.
#' 

#' **More complicated example with two predictors**
#' 
#' Imagine a model, `fit <- stan_glm(vote ~ factor(pid) + male, family=binomial(link="logit"))`
#' The poststrat population matrix now has 6 rows and 2 columns:
#' ```
poll <- data.frame(pid=rep(c("Republican", "Democrat", "Independent"),
                           c(2,2,2)), male=rep(c(0,1), 3))
N_population <- c(0.16, 0.17, 0.19, 0.17, 0.16, 0.15)


fit_2 <- stan_glm(vote ~ factor(pid) + male + factor(pid):male, data=poll)

N <- 5
x <- 1:N
y <- c(1,rep(0,N-1))
fit <- stan_glm(y ~ x, family=binomial(link="logit"))
print(fit)

pred_1 <- predict(fit, type="response")
print(pred_1)

X_new <- data.frame(x=1:N)
pred_2 <- posterior_predict(fit, newdata=X_new)
