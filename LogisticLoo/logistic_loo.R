setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/LogisticLoo")
library("rstanarm")
options(mc.cores = parallel::detectCores())
library("arm")
library("MASS")      # needed for mvrnorm()

n <- 200
k <- 5
rho <- 0.8
Sigma <- rho*array(1, c(k,k)) + (1-rho)*diag(k)
X <- mvrnorm(n, rep(0,k), Sigma)
b <- seq(-1, 3)
z <- rlogis(n, X %*% b, 1)
y <- ifelse(z>0, 1, 0)

## Fit full model
glm_1 <- glm(y ~ X, family=binomial(link="logit"))
fit_1 <- stan_glm(y ~ X, family=binomial(link="logit"), prior=normal(0, 10, autoscale=FALSE))
display(glm_1)
print(fit_1, digits=2)
loo_1 <- loo(fit_1)
print(loo_1)

## Try kfold
kfold_1 <- kfold(fit_1, K=10)
print(kfold_1)

## Compare to a model removing predictor 5
Z <- X[,1:4]
fit_2 <- stan_glm(y ~ Z, family=binomial(link="logit"), prior=normal(0, 10, autoscale=FALSE))
kfold_2 <- kfold(fit_2, K=10)
print(kfold_2)
compare(kfold_1, kfold_2)


fit_2 <- stan_glm(y ~ X[,2:5], family=binomial(link="logit"), prior=normal(0, 10, autoscale=FALSE))
kfold_2 <- kfold(fit_2, K=10)


## Compare to a model removing predictor 1
Z <- X[,2:5]
fit_3 <- stan_glm(y ~ Z, family=binomial(link="logit"), prior=normal(0, 10, autoscale=FALSE))
kfold_3 <- kfold(fit_3, K=10)
print(kfold_3)
compare(kfold_1, kfold_3)

## Fit some subsets

subsets <- list(1, 2, 3, 4, 5, 4:5, 3:5, 2:5, 1:5)
n_subsets <- length(subsets)
fit <- as.list(rep(NA, n_subsets))
for (i in 1:n_subsets) {
  print(i)
  fit[[i]] <- stan_glm(y ~ X[,subsets[[i]]], family=binomial(link="logit"), prior=normal(0, 10, autoscale=FALSE))
}
for (i in 1:n_subsets) {
  cat("\nsubset is", subsets[[i]], "\n")
  print(fit[[i]])
}
for (i in 1:n_subsets) {
  cat("\nsubset is", subsets[[i]], "\n")
  print(loo(fit[[i]]))
}
