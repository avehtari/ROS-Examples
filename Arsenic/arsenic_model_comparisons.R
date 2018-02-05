setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/Arsenic")
library("rstanarm")
options(mc.cores = parallel::detectCores())
library("arm")
library("loo")

wells <- read.csv("wells.csv")
n <- nrow(wells)

fit_3 <- glm(y ~ dist100 + arsenic,
             family = binomial(link = "logit"),
             data=wells)
display(fit_3)

## Residual deviance
X <- cbind(rep(1,n), wells$dist100, wells$arsenic)
beta_hat <- coef(fit_3)
p_hat <- invlogit(X %*% beta_hat)
log_score <- sum(wells$y * log(p_hat) + (1 - wells$y) * log(1 - p_hat))
deviance <- -2 * log_score

## Null deviance
y_bar <- mean(wells$y)
print(-2 * (n*y_bar*log(y_bar) + n*(1 - y_bar)*log(1 - y_bar)))

##

wells$noise <- array(rnorm(5 * n), dim = c(n, 5))
write.table(wells$noise, "noise.txt")
display(glm(y ~ dist100 + arsenic + noise,
            family = binomial(link = "logit"),
            data = wells))

bayes_fit_3 <-
  stan_glm(
    y ~ dist100 + arsenic,
    family = binomial(link = "logit"),
    prior = normal(0, 10, autoscale = FALSE),
    data = wells
  )


wells$log_arsenic <- log(wells$arsenic)
bayes_fit_3a <-
  stan_glm(
    y ~ dist100 + log_arsenic,
    family = binomial(link = "logit"),
    prior = normal(0, 10, autoscale = FALSE),
    data = wells
  )

print(bayes_fit_3, digits = 2)
loo_3 <- loo(bayes_fit_3, cores = 4)
print(loo_3)

print(bayes_fit_3a, digits = 2)
loo_3a <- loo(bayes_fit_3a, cores = 4)
print(loo_3a)

compare_models(loo_3, loo_3a)

bayes_fit_4a <-
  stan_glm(
    y ~ dist100 + log_arsenic + dist100:log_arsenic,
    family = binomial(link = "logit"),
    prior = normal(0, 10, autoscale = FALSE),
    data = wells
  )

loo_4a <- loo(bayes_fit_4a, cores = 4)
compare_models(loo_3a, loo_4a)

fit_4 <- glm(y ~ dist100 + arsenic + dist100:arsenic,
             family = binomial(link="logit"),
             data = wells)

bayes_fit_4 <-
  stan_glm(
    y ~ dist100 + arsenic + dist100:arsenic,
    family = binomial(link = "logit"),
    prior = normal(0, 10, autoscale = FALSE),
    data = wells
  )
loo_4 <- loo(bayes_fit_4, cores = 4)
compare_models(loo_3, loo_4)

bayes_fit_4b <-
  stan_glm(
    y ~ dist100 + log_arsenic + dist100:log_arsenic + noise,
    family = binomial(link = "logit"),
    prior = normal(0, 10, autoscale = FALSE),
    data = wells
  )
loo_4b <- loo(bayes_fit_4b, cores = 4)
compare_models(loo_4a, loo_4b)

fit_3c <- glm(y ~ dist100 + arsenic + log_arsenic,
              family=binomial(link="logit"),
              data=wells)
display(fit_3c)

bayes_fit_3c <-
  stan_glm(
    y ~ dist100 + arsenic + log_arsenic,
    family = binomial(link = "logit"),
    prior = normal(0, 10, autoscale = FALSE),
    data = wells
  )
print(bayes_fit_3c)

loo_3c <- loo(bayes_fit_3c, cores = 4)
print(loo_3c)
