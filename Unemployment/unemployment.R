#' ---
#' title: "Regression and Other Stories: Unemployment"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Time series fit and model checking for unemployment series
#' 
#' -------------
#' 

#' **Load libraries**
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library("rprojroot")
root<-has_dirname("RAOS-Examples")$make_fix_file()
## library("arm")
library("rstanarm")
options(mc.cores = parallel::detectCores())

#' **Load data**
unemp <- read.table(root("Unemployment/data","unemployment_simple.dat"),
                    header=TRUE)
unemp$y <- unemp$unemployed.pct

#' **Plot the unemployment rate**
#+ eval=FALSE, include=FALSE
pdf(root("Unemployment/figs","unemployment1.pdf"), height=3, width=4.5)
#+
par(mar=c(3,3,1,.1), mgp=c(1.7,.5,0), tck=-.01)
plot(unemp$year, unemp$y, type="l", ylab="Unemployment rate", xlab="Year", yaxs="i",
  ylim=c(0, max(unemp$y)*1.05), xaxt="n", yaxt="n", bty="l")
axis(1, seq(1950,2010,10), rep("",7))
axis(1, seq(1950,2010,20))
axis(2, seq(0,10), rep("",11))
axis(2, c(0,5,10), paste (c(0,5,10), "%", sep=""))
#+ eval=FALSE, include=FALSE
dev.off()

#' **Fit a 1st-order autogregression**
n <- nrow(unemp)
unemp$y_lag <- c(NA, unemp$y[1:(n-1)])
#+ results='hide'
fit_lag <- stan_glm(y ~ y_lag, data=unemp)
#+
print(fit_lag, digits=2)

#' **Simulate replicated datasets (using beta.hat, sigma.hat)**
y_rep <- posterior_predict(fit_lag)
y_rep <- cbind(unemp$y[1], y_rep)
n_sims <- nrow(y_rep)

#' **Plot the simulated unemployment rate series**
#+ eval=FALSE, include=FALSE
pdf(root("Unemployment/figs","unemployment2.pdf"), height=4.5, width=7.5)
#+
par(mar=c(1,1,3,.1), mgp=c(2,.5,0), tck=-.01)
par(mfrow=c(3,5))
for (s in sort(sample(n_sims, 15))){
  plot (unemp$year, y_rep[s,], type="l", ylab="", xlab="", yaxs="i",
  ylim=c(0, max(unemp$y)*1.05), xaxt="n", yaxt="n", bty="l", main=paste("Simulation", s))
  axis(1, seq(1950,2010,10), rep("",7))
  axis(2, seq(0,10), rep("",11))
}
#+ eval=FALSE, include=FALSE
dev.off()

#' **Numerical model check**
Test <- function (y){
  n <- length(y)
  y_lag <- c(NA, y[1:(n-1)])
  y_lag_2 <- c(NA, NA, y[1:(n-2)])
  return(sum(sign(y-y_lag) != sign(y_lag-y_lag_2), na.rm=TRUE))
}
test_y <- Test(unemp$y)
test_rep <- apply(y_rep, 1, Test)
print(mean(test_rep > test_y))
print(quantile(test_rep, c(.1,.5,.9)))
