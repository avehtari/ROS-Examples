setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/FakeMidtermFinal")
library("arm")
library("rstanarm")
options(mc.cores = parallel::detectCores())

N <- 1000
true_ability <- rnorm(N, 50, 10)
noise_1 <- rnorm(N, 0, 10)
noise_2 <- rnorm(N, 0, 10)
midterm <- true_ability + noise_1
final <- true_ability + noise_2
exams <- data.frame(midterm, final)

lm_2 <- stan_glm(final ~ midterm, data=exams)
print(lm_2)

pdf("FakeMidtermFinal1.pdf", height=4, width=4)
par(mar=c(3, 3, 2, 1), mgp=c(1.7, .5, 0), tck=-.01)
par(pty="s")
plot(midterm, final, xlab="Midterm exam score", ylab="Final exam score", xlim=c(0,100), ylim=c(0,100), xaxs="i", yaxs="i", xaxt="n", yaxt="n", pch=20, cex=.5)
x <- seq(0,100,20)
axis(1, x)
axis(2, x)
for (i in x){
  abline(h=i, col="gray70", lty=2)
  abline(v=i, col="gray70", lty=2)
}
abline(coef(lm_2)[1], coef(lm_2)[2])
dev.off()
