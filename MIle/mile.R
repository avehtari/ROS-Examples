setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/Mile")
library("arm")
library("rstanarm")
options(mc.cores = parallel::detectCores())

mile <- read.table("mile2.txt", header=TRUE)

year <- mile$yr + mile$month/12
seconds <- mile$min*60 + mile$sec

fit <- lm(seconds ~ year)
display(fit, 3)

## Predictions for 1900 and 2000

print(1007 -.393*c(1900,2000))  # Approx
print(coef(fit)[1] + coef(fit)[2]*c(1900,2000)) # Exact


pdf ("aplusbx1a.pdf", height=3.5, width=5)
a <- 0.15
b <- 0.4
par (mar=c(3,3,1,1), mgp=c(2,.5,0), tck=-.01)
plot (c(0,2.2), c(0,a+2.2*b), pch=20, cex=.5, main="y = a + bx",
  bty="l", type="n", xlab="x", ylab="y", xaxt="n", yaxt="n", xaxs="i", yaxs="i")
axis (1, c(0,1,2))
axis (2, c(a,a+b,a+2*b), c("a","a+b","a+2b"))
abline (a, b)
dev.off()

pdf ("aplusbx1b.pdf", height=3.5, width=5)
a <- 0.95
b <- -0.4
par (mar=c(3,3,1,1), mgp=c(2,.5,0), tck=-.01)
plot (c(0,2.2), c(0,a+.2), pch=20, cex=.5, main="y = a + bx (with b < 0)",
  bty="l", type="n", xlab="x", ylab="y", xaxt="n", yaxt="n", xaxs="i", yaxs="i")
axis (1, c(0,1,2))
axis (2, c(a,a+b,a+2*b), c("a","a+b","a+2b"))
abline (a, b)
dev.off()

pdf ("aplusbx2a.pdf", height=3.5, width=5)
par (mar=c(3,3,1,1), mgp=c(2,.5,0), tck=-.01)
curve (1007 - 0.393*x, from=0, to=2.1, xlab="x", ylab="y", bty="l",
  main="y = 1007 - 0.393x")
dev.off()

pdf ("aplusbx2b.pdf", height=3.5, width=5)
par (mar=c(3,3,1,1), mgp=c(2,.5,0), tck=-.01)
curve (1007 - 0.393*x, from=0, to=100, xlab="x", ylab="y", bty="l",
  main="y = 1007 - 0.393x")
dev.off()     

pdf ("aplusbx3.pdf", height=3.5, width=5)
par (mar=c(3,3,1,1), mgp=c(2,.5,0), tck=-.01)
curve (1007 - 0.393*x, from=1900, to=2000, xlab="Year", ylab="Time (seconds)", bty="l",
  main="Approx. trend of record times in the mile run", ylim=c(210, 270))
dev.off()
