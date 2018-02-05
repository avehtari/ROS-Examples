setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/SexRatio")
library("rstanarm")
library("arm")
options(mc.cores = parallel::detectCores())

theta_hat_prior <- 0
se_prior <- 0.25
theta_hat_data <- 8
se_data <- 3

theta_hat_bayes <- (theta_hat_prior/se_prior^2 + theta_hat_data/se_data^2)/(1/se_prior^2 + 1/se_data^2)
se_bayes <- sqrt(1/(1/se_prior^2 + 1/se_data^2))

x <- seq(-2,2,1)
y <- c(50, 44, 50, 47, 56)
sexratio <- data.frame(x, y)

fit <- lm(y ~ x, data = sexratio)
display(fit)

pdf("sexratio_bayes_1.pdf", height=4, width=10)
par(mfrow=c(1,2), mar=c(3,3,3,2), mgp=c(1.7,.5,0), tck=-.01)
plot(x, y, ylim=c(43, 57), xlab="Attractiveness of parent", ylab="Percentage of girl babies", bty="l", yaxt="n", main="Data on beauty and sex ratio",  pch=19, cex=1)
axis(2, c(45,50,55), paste(c(45,50,55), "%", sep=""))
plot(x, y, ylim=c(43, 57), xlab="Attractiveness of parent", ylab="Percentage of girl babies", bty="l", yaxt="n", main="Data and least-squares regression line",  pch=19, cex=1)
axis(2, c(45,50,55), paste(c(45,50,55), "%", sep=""))
abline(coef(fit)[1], coef(fit)[2])
text(1, 52.2, paste("y = ", fround(coef(fit)[1], 1), " + ", fround(coef(fit)[2], 1), " x\n(Std err of slope is ", fround(se.coef(fit)[2], 1), ")", sep=""))
dev.off()


fit_default <- stan_glm(y ~ x, data = sexratio)
prior_summary(fit_flat)
print(fit_default)


fit_post <-
  stan_glm(
    y ~ x,
    data = sexratio,
    prior = normal(0, 0.2, autoscale = FALSE),
    prior_intercept = normal(48.8, 0.5, autoscale = FALSE)
  )#, prior_aux=normal(0, 2, autoscale=FALSE))
prior_summary(fit_post)
print(fit_post)



pdf("sexratio_bayes_2.pdf", height=8, width=10)
par(mfrow=c(2,2), mar=c(5,3,3,2), mgp=c(1.7,.5,0), tck=-.01)
fit_bayes <- list(as.data.frame(fit_default), as.data.frame(fit_post))
for (k in 1:2){
  sims <- fit_bayes[[k]]
  coef_est <- apply(sims, 2, median)
  b_se <- 1.483*median(abs(sims[,2] - median(sims[,2])))
  plot(sims[,1], sims[,2], xlim=range(fit_bayes[[1]][,1], fit_bayes[[2]][,1]), ylim=range(fit_bayes[[1]][,2], fit_bayes[[2]][,2]), xlab="Intercept, a", ylab="Slope, b", main=paste("Posterior simulations under", if (k==1) "default prior" else "informative prior"), pch=19, cex=.2, bty="l")
  plot(x, y, ylim=c(43, 57), xlab="Attractiveness of parent", ylab="Percentage of girl babies", bty="l", yaxt="n", main=if (k==1) "Least-squares regression line and\nposterior uncertainty given default prior" else "Bayes estimated regression line and\nposterior uncertainty given informative prior", pch=19, cex=1)
  axis(2, c(45,50,55), paste(c(45,50,55), "%", sep=""))
  for (i in 1:100){
    abline(sims[i,1], sims[i,2], lwd=.5, col="gray50")
  }
  abline(coef_est[1], coef_est[2], lwd=2)
}
dev.off()
