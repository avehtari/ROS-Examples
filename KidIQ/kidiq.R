setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/KidIQ")
library("arm")
library("rstanarm")
options(mc.cores = parallel::detectCores())
library("ggplot2")
library("bayesplot")
theme_set(bayesplot::theme_default(base_family = "sans"))

## code for nlsy kid iq data for Chapter 3

library("foreign")
kidiq <- read.dta(file="kidiq.dta")

### fitting and summarizing regressions in R

fit_3 <- lm(kid_score ~ mom_hs + mom_iq, data=kidiq)
display(fit_3)
print(fit_3)
summary(fit_3)

stan_fit_3 <- stan_glm(kid_score ~ mom_hs + mom_iq, data=kidiq)
print(stan_fit_3)

### graphical displays of data and fitted models

fit_2 <- lm(kid_score ~ mom_iq, data=kidiq)
plot(kidiq$mom_iq, kidiq$kid_score, xlab="Mother IQ score", ylab="Child test score")
abline(coef(fit_2)[1], coef(fit_2)[2])

# alternately
curve(cbind(1,x) %*% coef(fit_2), add=TRUE)

# ggplot version
ggplot(kidiq, aes(mom_iq, kid_score)) +
  geom_point() +
  geom_abline(
    intercept = coef(fit_2)[1],
    slope = coef(fit_2)[2]
  ) +
  labs(
    x = "Mother IQ score",
    y = "Child test score"
  )

### two fitted regression lines

## model with no interaction
fit_3 <- lm(kid_score ~ mom_hs + mom_iq, data=kidiq)
colors <- ifelse(kidiq$mom_hs==1, "black", "gray")
plot(kidiq$mom_iq, kidiq$kid_score,
  xlab="Mother IQ score", ylab="Child test score", col=colors, pch=20)
b_hat <- coef(fit_3)
abline(b_hat[1] + b_hat[2], b_hat[3], col="black")
abline(b_hat[1], b_hat[3], col="gray")

# ggplot version
ggplot(kidiq, aes(mom_iq, kid_score)) +
  geom_point(aes(color = factor(mom_hs)), show.legend = FALSE) +
  geom_abline(
    intercept = c(coef(fit_3)[1], coef(fit_3)[1] + coef(fit_3)[2]),
    slope = coef(fit_3)[3],
    color = c("gray", "black")
  ) +
  scale_color_manual(values = c("gray", "black")) +
  labs(
    x = "Mother IQ score",
    y = "Child test score"
  )

### two fitted regression lines:

## model with interaction
fit_4 <- lm(kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq, data=kidiq)
colors <- ifelse(kidiq$mom_hs==1, "black", "gray")
plot(kidiq$mom_iq, kidiq$kid_score,
  xlab="Mother IQ score", ylab="Child test score", col=colors, pch=20)
b_hat <- coef(fit_4)
abline(b_hat[1] + b_hat[2], b_hat[3] + b_hat[4], col="black")
abline(b_hat[1], b_hat[3], col="gray")

# ggplot version
ggplot(kidiq, aes(mom_iq, kid_score)) +
  geom_point(aes(color = factor(mom_hs)), show.legend = FALSE) +
  geom_abline(
    intercept = c(coef(fit_4)[1], sum(coef(fit_4)[1:2])),
    slope = c(coef(fit_4)[3], sum(coef(fit_4)[3:4])),
    color = c("gray", "black")
  ) +
  scale_color_manual(values = c("gray", "black")) +
  labs(
    x = "Mother IQ score",
    y = "Child test score"
  )

## displaying uncertainty in the fitted regression
stan_fit_2 <- stan_glm(kid_score ~ mom_iq, data = kidiq)
print(stan_fit_2)
sims_2 <- as.matrix(stan_fit_2)
n_sims_2 <- nrow(sims_2)
subset <- sample(n_sims_2, 10)

plot(kidiq$mom_iq, kidiq$kid_score,
     xlab="Mother IQ score", ylab="Child test score")
for (i in subset){
  abline(sims_2[i,1], sims_2[i,2], col="gray")
}
abline(coef(stan_fit_2)[1], coef(stan_fit_2)[2], col="black")

# ggplot version
ggplot(kidiq, aes(mom_iq, kid_score)) +
  geom_point() +
  geom_abline(
    intercept = sims_2[subset, 1],
    slope = sims_2[subset, 2],
    color = "gray",
    size = 0.25
  ) +
  geom_abline(
    intercept = coef(stan_fit_2)[1],
    slope = coef(stan_fit_2)[2],
    size = 0.75
  ) +
  labs(
    x = "Mother IQ score",
    y = "Child test score"
  )

## 2 plots

stan_fit_3 <- stan_glm(kid_score ~ mom_hs + mom_iq, data=kidiq)
sims_3 <- as.matrix(stan_fit_3)
n_sims_3 <- nrow(sims_3)

pdf("kidiq.betasim2.pdf", height=3.5, width=9)
par(mar=c(3,3,1,3), mgp=c(1.7, .5, 0), tck=-.01)
par(mfrow=c(1,2))

plot(kidiq$mom_iq, kidiq$kid_score, xlab="Mother IQ score", ylab="Child test score", bty="l", pch=20, xaxt="n", yaxt="n")
axis(1, seq(80, 140, 20))
axis(2, seq(20, 140, 40))
mom_hs_bar <- mean(kidiq$mom_hs)
subset <- sample(n_sims_2, 10)
for (i in subset){
  curve(cbind(1, mom_hs_bar, x) %*% sims_3[i,1:3], lwd=.5,
     col="gray", add=TRUE)
}
curve(cbind(1, mom_hs_bar, x) %*% coef(stan_fit_3), col="black", add=TRUE)

jitt <- runif(nrow(kidiq), -.03, .03)
plot(kidiq$mom_hs + jitt, kidiq$kid_score, xlab="Mother completed high school", ylab="Child test score", bty="l", pch=20, xaxt="n", yaxt="n")
axis(1, c(0,1))
axis(2, seq(20, 140, 40))
mom_iq_bar <- mean(kidiq$mom_iq)
for (i in subset){
  curve(cbind(1, x, mom_iq_bar) %*% sims_3[i,1:3], lwd=.5,
     col="gray", add=TRUE)
}
curve(cbind(1, x, mom_iq_bar) %*% coef(stan_fit_3), col="black", add=TRUE)

dev.off()

