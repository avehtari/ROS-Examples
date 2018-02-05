library(HistData)
data(PearsonLee)

setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/PearsonLee")
write.table(PearsonLee, "PearsonLee.txt")

data <- read.table("PearsonLee.txt", header=TRUE)
print(data[1:10,])
subset <- data[,"gp"]=="md"
daughter_height <- rep(data[subset, "child"], 4*data[subset,"frequency"])
mother_height <- rep(data[subset, "parent"], 4*data[subset,"frequency"])
write.table(cbind(daughter_height, mother_height), "Heights.txt", row.names=FALSE)

heights <- read.table("Heights.txt", header=TRUE)
daughter_height <- heights$daughter_height
mother_height <- heights$mother_height

pdf("PearsonLee1.pdf", height=4.5, width=4.5)
par(mar=c(3, 3, 2, 1), mgp=c(1.7, .5, 0), tck=-.01)
par(pty="s")
rng <- range(mother_height, daughter_height)
plot(mother_height, daughter_height, xlab="Mother's height (inches)", ylab="Adult daughter's height (inches)", bty="l", xlim=rng, ylim=rng, xaxt="n", yaxt="n", pch=20, cex=.5)
x <- seq(48, 84, 6)
axis(1, x)
axis(2, x)
for (i in x){
  abline(h=i, col="gray70", lty=2)
  abline(v=i, col="gray70", lty=2)
}
dev.off()

library("arm")
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

y <- cbind(mother_height, daughter_height)
N <- nrow(y)
fit <- stan("bivariate_normal_rounded.stan", iter=100)
## This is too slow!  Talk with Ben about how to fix.
z_post <- extract(fit)$z
z_sim <- z_post[1,,]           # One random draw

fit_trivlal <- stan("trivial_rounded.stan", iter=100)

mu <- c(63, 63)
Sigma <- cbind(c(6,3), c(3,6))
fit_2 <- stan("bivariate_normal_rounded_2.stan", iter=100)

n <- length(mother_height)
mother_height_jitt <- mother_height + runif(n, -0.5, 0.5)
daughter_height_jitt <- daughter_height + runif(n, -0.5, 0.5)

pdf("PearsonLee2.pdf", height=4.5, width=4.5)
par(mar=c(3, 3, 2, 1), mgp=c(1.7, .5, 0), tck=-.01)
par(pty="s")
rng <- range(mother_height, daughter_height)
plot(mother_height_jitt, daughter_height_jitt, xlab="Mother's height (inches)", ylab="Adult daughter's height (inches)", bty="l", xlim=rng, ylim=rng, xaxt="n", yaxt="n", pch=20, cex=.2)
x <- seq(48, 84, 6)
axis(1, x)
axis(2, x)
for (i in x){
  abline(h=i, col="gray70", lty=2)
  abline(v=i, col="gray70", lty=2)
}
dev.off()

lm_1 <- lm (daughter_height ~ mother_height)
display(lm_1)
ab_hat <- coef(lm_1)

pdf("PearsonLee3a.pdf", height=4.5, width=4.5)
par(mar=c(3, 3, 2, .1), mgp=c(2, .5, 0), tck=-.01)
par(pty="s")
rng <- range(mother_height, daughter_height)
plot(mother_height_jitt, daughter_height_jitt, xlab="Mother's height (inches)", ylab="Adult daughter's height (inches)", bty="l", xlim=c(rng[1], rng[2]), ylim=rng, xaxt="n", yaxt="n", pch=20, cex=.2)
x <- seq(48, 84, 6)
axis(1, x)
axis(2, x)
for (i in x){
  abline(h=i, col="gray70", lty=2)
  abline(v=i, col="gray70", lty=2)
}
abline(ab_hat[1], ab_hat[2], lwd=3, col="white")
abline(ab_hat[1], ab_hat[2], lwd=1.5)
points(mean(mother_height), mean(daughter_height), pch=20, cex=2, col="white")
mtext("Mothers' and daughters' heights,\naverage of data, and fitted regression line", side=3, line=0)
dev.off()


pdf("PearsonLee3b.pdf", height=4.5, width=4.5)
par(mar=c(3, 3, 2, .1), mgp=c(2, .5, 0), tck=-.01)
par(pty="s")
rng <- range(mother_height, daughter_height)
plot(mother_height_jitt, daughter_height_jitt, xlab="Mother's height (inches)", ylab="Adult daughter's height (inches)", bty="l", xlim=c(rng[1], rng[2]), ylim=rng, xaxt="n", yaxt="n", pch=20, cex=.2, type="n")
x <- seq(54, 72, 6)
axis(1, x)
axis(2, x)
abline(ab_hat[1], ab_hat[2], lwd=3, col="white")
abline(ab_hat[1], ab_hat[2], lwd=1.5)
lines(rep(mean(mother_height), 2), c(0, mean(daughter_height)), lwd=.5)
lines(c(0, mean(mother_height)), rep(mean(daughter_height), 2), lwd=.5)
axis(1, mean(mother_height), fround(mean(mother_height), 1))
axis(2, mean(daughter_height), fround(mean(daughter_height), 1))
text(68, 64, paste("y =", round(ab_hat[1]), "+", fround(ab_hat[2], 2), "x"))
text(63, 62, paste("Equivalently,  y = ", fround(mean(daughter_height), 1), " + ", fround(ab_hat[2], 2), " * (x - ", fround(mean(mother_height), 1), ")", sep=""))
points(mean(mother_height), mean(daughter_height), pch=20, cex=2)
mtext("The fitted regression line and the average of the data      ", side=3, line=1)
dev.off()

pdf("PearsonLee4a.pdf", height=4, width=4.5)
par(mar=c(3, 3, 2, .1), mgp=c(2, .5, 0), tck=-.01)
plot(c(0, 100), c(0, 100), xlab="", ylab="", xaxt="n", yaxt="n", bty="n", type="n")
abline(h=0)
abline(v=0)
axis(2, round(ab_hat[1]), tck=0, las=1)
axis(1, 0, tck=0, las=1, line=-.4)
axis(2, 0, tck=0, las=1)
abline(ab_hat[1], ab_hat[2], lwd=2)
text(40, 40, paste("slope", fround(ab_hat[2], 2)))
mtext(paste("The line, y =", round(ab_hat[1]), "+", fround(ab_hat[2], 2), "x"), side=3, line=0)
dev.off()



pdf("PearsonLee4b.pdf", height=4, width=4.5)
par(mar=c(3, 3, 2, .1), mgp=c(2, .5, 0), tck=-.01)
plot(c(0, 100), c(0, 100), xlab="", ylab="", xaxt="n", yaxt="n", bty="n", type="n")
abline(h=0)
abline(v=0)
axis(2, round(ab_hat[1]), tck=0, las=1)
points(mother_height_jitt, daughter_height_jitt, pch=20, cex=.2)
abline(ab_hat[1], ab_hat[2], lwd=3, col="white")
abline(ab_hat[1], ab_hat[2], lwd=1.5)
axis(1, 0, tck=0, las=1, line=-.4)
axis(2, 0, tck=0, las=1)
axis(1, mean(mother_height), fround(mean(mother_height), 1), tck=0, las=1, line=-.4)
axis(2, mean(daughter_height), fround(mean(daughter_height), 1), tck=0, las=1, line=-.7)
lines(rep(mean(mother_height), 2), c(0, mean(daughter_height)), lwd=.5)
lines(c(0, mean(mother_height)), rep(mean(daughter_height), 2), lwd=.5)
text(40, 43, paste("slope", fround(ab_hat[2], 2)), cex=.9)
mtext(paste("The line, y =", round(ab_hat[1]), "+", fround(ab_hat[2], 2), "x, in the context of the data"), side=3, line=0)
dev.off()
