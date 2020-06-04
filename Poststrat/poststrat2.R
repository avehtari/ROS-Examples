#' ---
#' title: "Regression and Other Stories: Poststratification 2"
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

#' Demonstrate poststratification with simulated census and poll data
#' See Chapter 17 in Regression and Other Stories.
#' 
#' 
#' -------------
#' 

#' #### Load packages
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library("rstanarm")
invlogit <- plogis

#' ## Simulate fake data
#'
#' Create an empty poststratification table, with rows for each of the
#' $2 \times 4 \times 4$ sorts of people.
J <- c(2, 4, 4)
poststrat <- as.data.frame(array(NA, c(prod(J), length(J)+1)))
colnames(poststrat) <- c("sex", "age", "eth", "N")
count <- 0
for (i1 in 1:J[1]){
  for (i2 in 1:J[2]){
    for (i3 in 1:J[3]){
      count <- count + 1
      poststrat[count, 1:3] <- c(i1, i2, i3)
    }
  }
}

#' make up numbers for the populations of the cells
p_sex <- c(0.52, 0.48)
p_age <- c(0.2, 0.25, 0.3, 0.25)
p_eth <- c(0.7, 0.1, 0.1, 0.1)
for (j in 1:prod(J)){
  poststrat$N[j] <- 250e6 * p_sex[poststrat[j,1]] * p_age[poststrat[j,2]] *
  p_eth[poststrat[j,3]]
}

#' Hypothesize a nonresponse pattern in which women, older people, and
#' whites are more likely to respond than men, younger people, and
#' minorities
p_response_baseline <- 0.1
p_response_sex <- c(1, 0.8)
p_response_age <- c(1, 1.2, 1.6, 2.5)
p_response_eth <- c(1, 0.8, 0.7, 0.6)
p_response <- rep(NA, prod(J))
for (j in 1:prod(J)){
  p_response[j] <- p_response_baseline * p_response_sex[poststrat[j,1]] *
    p_response_age[poststrat[j,2]] * p_response_eth[poststrat[j,3]]
}

#' Sample from the assumed population with the assumed nonresponse probabilities
n <- 1000
people <- sample(prod(J), n, replace=TRUE, prob=poststrat$N*p_response)
# For respondent i, people[i] is that person's poststrat cell,
# some number between 1 and 32
n_cell <- rep(NA, prod(J))
for (j in 1:prod(J)){
  n_cell[j] <- sum(people==j)
}
print(cbind(poststrat, n_cell/n, poststrat$N/sum(poststrat$N)))

#' Assume the survey responses come from a logistic regression with
#' these coefficients
coef_intercept <- 0.6
coef_sex <- c(0, -0.2)
coef_age <- c(0, -0.2, -0.3, -0.4)
coef_eth <- c(0, 0.6, 0.3, 0.3)

#' The probabilities are:
prob_yes <- rep(NA, prod(J))
for (j in 1:prod(J)){
  prob_yes[j] <- invlogit(coef_intercept + coef_sex[poststrat[j,1]] +
  coef_age[poststrat[j,2]] + coef_eth[poststrat[j,3]])
}

#' Simulate the fake data:
y <- rbinom(n, 1, prob_yes[people])

#' ## Linear model
sex <- poststrat[people,1]
age <- poststrat[people,2]
eth <- poststrat[people,3]
fake <- data.frame(y, sex, age, eth)
fit <- stan_glm(y ~ factor(sex) + factor(age) + factor(eth),
                family=binomial(link="logit"), data=fake, refresh=0)
print(fit)

#' #### Prediction
pred_sim <- posterior_epred(fit, newdata=as.data.frame(poststrat))
pred_est <- colMeans(pred_sim)
round(cbind(poststrat, prob_yes, pred_est), 2)

#' #### Poststratification
poststrat_est <- sum(poststrat$N*pred_est)/sum(poststrat$N)
round(poststrat_est, 2)
#' plus uncertainty
poststrat_sim <- pred_sim %*% poststrat$N / sum(poststrat$N)
round(c(mean(poststrat_sim), sd(poststrat_sim)), 3)
