setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/LogisticPriors")
library("rstanarm")
options(mc.cores = parallel::detectCores())

bayes_sim <- function(n, a=-2, b=0.8){
  x <- runif(n, -1, 1)
  z <- rlogis(n, a + b*x, 1)
  y <- ifelse(z>0, 1, 0)
  glm_fit <- glm(y ~ x, family = binomial(link = "logit"))
  stan_fit <- stan_glm(y ~ x, family = binomial(link = "logit"),
     prior=normal(0.5, 0.5, autoscale=FALSE))
  display(glm_fit, digits=1)
  print(stan_fit, digits=1)
}

bayes_sim(10)
bayes_sim(100)
bayes_sim(1000)

