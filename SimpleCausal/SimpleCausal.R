## Simple graphs illustrating regression for causal inference

library("arm")
setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/SimpleCausal")

N <- 50
x <- runif(N, 1, 5)
y <- rnorm(N, 10 + 3*x, 3)
x_binary <- ifelse(x<3, 0, 1)

lm_1a <- lm(y ~ x_binary)
display(lm_1a)

pdf("overview_1a.pdf", height=3.5, width=4.5)
par(mar=c(3, 3, 1.5, 1), mgp=c(1.7, .5, 0), tck=-.01)
plot(x_binary, y, xlab="", ylab="Outcome measurement", pch=20, cex=.5, bty="l", main="Regression with binary treatment", cex.main=.9, xaxt="n", cex.lab=.9, cex.axis=.9)
axis(1, c(0,1), c("    Control", "Treatment    "), cex.axis=.9)
abline(coef(lm_1a)[1], coef(lm_1a)[2])
text(0.3, 13, paste("Estimated treatment effect is\nslope of fitted line: ", fround(coef(lm_1a)[2], 1)), cex=.8, adj=0)
dev.off()

lm_1b <- lm(y ~ x)
display(lm_1b)

pdf("overview_1b.pdf", height=3.5, width=4.5)
par(mar=c(3, 3, 1.5, 1), mgp=c(1.7, .5, 0), tck=-.01)
plot(x, y, xlab="Treatment level", ylab="Outcome measurement", pch=20, cex=.5, bty="l", main="Regression with continuous treatment", cex.main=.9, cex.lab=.9, cex.axis=.9)
abline(coef(lm_1b)[1], coef(lm_1b)[2])
text(3.2, 15, paste("Estimated treatment\neffect per unit of x is\nslope of fitted line: ", fround(coef(lm_1b)[2], 1)), cex=.8, adj=0)
dev.off()

y <- rnorm(N, 10 + 25*exp(-x), 3)

lm_2a <- lm(y ~ x)
display(lm_2a)


pdf("overview_2a.pdf", height=3.5, width=4.5)
par(mar=c(3, 3, 1.5, 1), mgp=c(1.7, .5, 0), tck=-.01)
plot(x, y, xlab="Treatment level", ylab="Outcome measurement", pch=20, cex=.5, bty="l", main="Nonlinear treatment effect", cex.main=.9, cex.lab=.9, cex.axis=.9)
curve(10 + 25*exp(-x), add=TRUE)
dev.off()

pdf("overview_2b.pdf", height=3.5, width=4.5)
par(mar=c(3, 3, 1.5, 1), mgp=c(1.7, .5, 0), tck=-.01)
plot(x, y, xlab="Treatment level", ylab="Outcome measurement", pch=20, cex=.5, bty="l", main="Nonlinear effect, estimated with straight line fit", cex.main=.9, cex.lab=.9, cex.axis=.9)
abline(coef(lm_2a)[1], coef(lm_2a)[2])
dev.off()

N <- 100
xx <- rnorm(N, 0, 1)^2
z <- rep(0:1, N/2)
xx <- ifelse(z==0, rnorm(N, 0, 1.2)^2, rnorm(N, 0, .8)^2)
yy <- rnorm(N, 20 + 5*xx + 10*z, 3)
lm_2 <- lm(yy ~ xx + z)
display(lm_2)

pdf("overview_3.pdf", height=3.5, width=4.5)
par(mar=c(3, 3, 1.5, 1), mgp=c(1.7, .5, 0), tck=-.01)
plot(xx, yy, xlab="Pre-treatment predictor", ylab="Outcome measurement", bty="l", main="Continuous pre-treatment predictor and binary treatment    ", cex.main=.9, cex.lab=.9, cex.axis=.9, type="n")
points(xx[z==0], yy[z==0], pch=20, cex=.5)
points(xx[z==1], yy[z==1], pch=1, cex=1)
abline(coef(lm_2)[1], coef(lm_2)[2])
abline(coef(lm_2)[1] + coef(lm_2)[3], coef(lm_2)[2])
text(2.3, 27, "Controls", cex=.9, adj=0)
text(1.5, 45, "Treated", cex=.9, adj=0)
x0 <- 5.2
arrows(x0, coef(lm_2)[1] + coef(lm_2)[2]*x0, x0, coef(lm_2)[1] + coef(lm_2)[2]*x0 + coef(lm_2)[3], length=.1, code=3)
text(x0+.1, coef(lm_2)[1] + coef(lm_2)[2]*x0 + .5*coef(lm_2)[3], paste("Estimated\ntreatment\neffect is", fround(coef(lm_2)[3], 1)), cex=.8, adj=0)
dev.off()

for (j in 0:1) print(mean(yy[z==j]))
for (j in 0:1) print(mean(xx[z==j]))

