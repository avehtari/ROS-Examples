setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/ElectionsEconomy")
library("arm")
library("rstanarm")
options(mc.cores = parallel::detectCores())

hibbs <- read.table("hibbs.dat", header=TRUE)

## Fake data simulation to check confidence interval coverage

a <- 46.2
b <- 3.1
sigma <- 3.8
x <- hibbs$growth
n <- length(x)

y <- a + b*x + rnorm(n, 0, sigma)
fake <- data.frame(x, y)

fit <- lm(y ~ x, data=fake)
display(fit)

b_hat <- coef(fit)["x"]
b_se <- se.coef(fit)["x"]

cover_68 <- abs(b - b_hat) < b_se
cover_95 <- abs(b - b_hat) < 2*b_se
cat(paste("68% coverage: ", cover_68, "\n"))
cat(paste("95% coverage: ", cover_95, "\n"))

n_fake <- 1000
cover_68 <- rep(NA, n_fake)
cover_95 <- rep(NA, n_fake)
for (s in 1:n_fake){
  y <- a + b*x + rnorm(n, 0, sigma)
  fake <- data.frame(x, y)
  fit <- lm(y ~ x, data=fake)
  b_hat <- coef(fit)["x"]
  b_se <- se.coef(fit)["x"]
  cover_68[s] <- abs(b - b_hat) < b_se
  cover_95[s] <- abs(b - b_hat) < 2*b_se
}
cat(paste("68% coverage: ", mean(cover_68), "\n"))
cat(paste("95% coverage: ", mean(cover_95), "\n"))

n_fake <- 1000
cover_68 <- rep(NA, n_fake)
cover_95 <- rep(NA, n_fake)
t_68 <- qt(0.84, n - 2)
t_95 <- qt(0.975, n - 2)
for (s in 1:n_fake){
  y <- a + b*x + rnorm(n, 0, sigma)
  fake <- data.frame(x, y)
  fit <- lm(y ~ x, data=fake)
  b_hat <- coef(fit)["x"]
  b_se <- se.coef(fit)["x"]
  cover_68[s] <- abs(b - b_hat) < t_68 * b_se
  cover_95[s] <- abs(b - b_hat) < t_95 * b_se
}
cat(paste("68% coverage: ", mean(cover_68), "\n"))
cat(paste("95% coverage: ", mean(cover_95), "\n"))
