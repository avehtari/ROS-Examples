setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/Coverage")

n_rep <- 100
est <- rep(NA, n_rep)
conf <- array(NA, c(n_rep, 4))
mu <- 6
sigma <- 4
for (i in 1:n_rep){
  y <- rnorm(1, mu, sigma)
  est[i] <- y
  conf[i,] <- y + c(-2, -.67, .67, 2) * sigma
}

pdf("coverage.pdf", height=4, width=8)
par(mar=c(3,3,0,0), mgp=c(1.5,.5,0), tck=-.01)
plot(c(-2, n_rep+2), range(conf), bty="l", xlab="Simulation", ylab="Estimate, 50%, and 95% confidence interval", xaxs="i", yaxt="n", type="n")
axis(2, seq(-10,20,10))
points(1:n_rep, est, pch=20)
abline(mu, 0, col="gray")
for (i in 1:n_rep){
  lines(c(i,i), conf[i,c(1,4)], lwd=.8)
  lines(c(i,i), conf[i,c(2,3)], lwd=2)
}
dev.off()
