#' ---
#' title: "Regression and Other Stories: Gay"
#' author: "Andrew Gelman, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     theme: readable
#'     toc: true
#'     toc_depth: 2
#'     toc_float: true
#'     code_download: true
#' ---

#' Simple models (linear and discretized age) and attitudes as a
#' function of age. See Chapter 12 in Regression and Other Stories.
#' 
#' -------------
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(message=FALSE, error=FALSE, warning=FALSE, comment=NA)
# switch this to TRUE to save figures in separate files
savefigs <- FALSE

#' #### Load packages
library("rprojroot")
root<-has_file(".ROS-Examples-root")$make_fix_file()
library("rstanarm")

#' #### Load data
data <- read.csv(root("Gay/data","naes04.csv"))
age <- seq(18,91)
response <- as.character(data[,"gayFavorStateMarriage"])
n_age <- length(age)
yes <- rep(NA, n_age)
no <- rep(NA, n_age)
for (i in 1:n_age){
  if (i == n_age)
    ok <- data[,"age"] >= age[i]
  else
    ok <- data[,"age"] == age[i]
  yes[i] <- sum(ok & response=="Yes", na.rm=TRUE)
  no[i] <- sum(ok & response=="No", na.rm=TRUE)
}

support <- yes/(yes + no)
age_cutpoints <- c(0, seq(29, 79, 10), 100)
age_discrete <- cut(age, age_cutpoints)
gay_total <- data.frame(support, age, age_discrete)

#' #### Fit linear regression model
fit_linear <- stan_glm(support ~ age, data=gay_total, refresh=0)
print(fit_linear)

#' #### Fit discretized age model
fit_binned <- stan_glm(support ~ factor(age_discrete), data=gay_total, refresh=0)
print(fit_binned)


#' #### Plot linear regression model
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Gay/figs","gay_simple_1a.pdf"), height=3.5, width=5)
#+
par(mar=c(3,3,01,0), mgp=c(1.7, .5, 0), tck=-.02)
plot(age, support, xlim=c(min(age)-1, max(age)+1), ylim=c(0, 0.65), xaxs="i", yaxs="i", xlab="Age", ylab="Support for same-sex marriage", xaxt="n", yaxt="n", bty="l", main="Linear regression", cex.main=1, type="n")
points(age, support, pch=20, col="gray40")
axis(1, seq(20, 80, 20))
axis(2, seq(0, 0.60, 0.20), paste(seq(0, 60, 20), "%", sep=""))
abline(coef(fit_linear)[1], coef(fit_linear)[2], lwd=2)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Plot discretized age model
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Gay/figs","gay_simple_1b.pdf"), height=3.5, width=5)
#+
par(mar=c(3,3,1,0), mgp=c(1.7, .5, 0), tck=-.02)
plot(age, support, xlim=c(min(age)-1, max(age)+1), ylim=c(0, 0.65), xaxs="i", yaxs="i", xlab="Age", ylab="Support for same-sex marriage", xaxt="n", yaxt="n", bty="l", main="Discretized age predictors", cex.main=1, type="n")
points(age, support, pch=20, col="gray40")
axis(1, seq(20, 80, 20))
axis(2, seq(0, 0.60, 0.20), paste(seq(0, 60, 20), "%", sep=""))
for (k in 1:(length(age_cutpoints) - 1)){
  lines(age_cutpoints[k:(k+1)], rep(coef(fit_binned)[1] + ifelse (k==1, 0, coef(fit_binned)[k]), 2), lwd=2)
}
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
