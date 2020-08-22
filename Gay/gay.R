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

#' Nonlinear models (loess, spline, GP, and BART) and political
#' attitudes as a function of age. See Chapter 22 in Regression and
#' Other Stories.
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
# Enable parallel computation in stan_gamm4 used for splines and GPs
options(mc.cores = parallel::detectCores())
library("dbarts")

#' #### Define common plot functions
gay_plot <- function(fit=NULL, question=NULL, title=NULL, savefigs=FALSE) {
  if (savefigs) pdf(root("Gay/figs",paste("gay", index, ".pdf", sep="")), height=4.5, width=6)
  par(mar=c(3,3,1,1), mgp=c(1.7, .5, 0), tck=-.01)
  plot(gay_sum[[j]]$age, gay_sum[[j]]$y/gay_sum[[j]]$n, ylim=c(0,.65), yaxs="i", xlab="Age", ylab=question, yaxt="n", bty="l", type="n", main=title)
  axis(2, seq(0,1,.2), c("0","20%","40%","60%","80%","100%"))
  symbols(gay_sum[[j]]$age, gay_sum[[j]]$y/gay_sum[[j]]$n, circles=sqrt(gay_sum[[j]]$n), inches=.1, add=TRUE, fg=if (is.null(fit)) "black" else "gray80", bg=if (is.null(fit)) "gray70" else "gray90")
  if (!is.null(fit)) {
    n_sims <- nrow(fit)
    for (i in sample(n_sims, 20)){
      lines(gay_sum[[j]]$age, fit[i,], lwd=.5, col="gray50")
    }
    lines(gay_sum[[j]]$age, colMeans(fit), lwd=2)
  }
  if (savefigs) dev.off()
  index <<- index + 1
}

gay_plot_1 <- function(fit=NULL, question=NULL, title=NULL, k_bottom) {
  age <- gay_sum[[j]]$age
  y <- gay_sum[[j]]$y
  n <- gay_sum[[j]]$n
  plot(age, y/n, ylim=c(0,.65), yaxs="i", xlab= if (k==k_bottom) "Age" else "", ylab="", xaxt= if (k<k_bottom) "n", yaxt="n", bty="l", type="n", cex.lab=1.2, cex.axis=1.2)
  axis(1, seq(20,80,20), rep("",4), cex.axis=1.2)
  axis(2, seq(0,1,.1), rep("",11))
  axis(2, seq(0,1,.2), c("0","20%","40%","60%","80%","100%"), cex.axis=1.2)
  symbols(age, y/n, circles=sqrt(n), inches=.1, add=TRUE, fg="gray80", bg="gray90")
  n_sims <- nrow(fit)
  for (i in sample(n_sims, 20)){
    lines(age, fit[i,], lwd=.5, col="gray50")
  }
  lines(age, colMeans(fit), lwd=2)
}

gay_plot_2 <- function(fit=NULL, question=NULL, title=NULL, m=NULL) {
  ok <- gay_sum_2[[j]]$male == m
  age <- gay_sum_2[[j]]$age[ok]
  y <- gay_sum_2[[j]]$y[ok]
  n <- gay_sum_2[[j]]$n[ok]
  plot(age, y/n, ylim=c(0,.65), yaxs="i", xlab= if (k==4) "Age" else "", ylab="", xaxt= if (k<4) "n", yaxt="n", bty="l", type="n", cex.lab=1.4, cex.axis=1.4, main=if (m==0) "Women" else "Men")
  axis(1, seq(20,80,20), rep("",4), cex.axis=1.4)
  axis(2, seq(0,1,.1), rep("",11))
  axis(2, seq(0,1,.2), c("0","20%","40%","60%","80%","100%"), cex.axis=1.4)
  symbols(age, y/n, circles=sqrt(n), inches=.1, add=TRUE, fg="gray80", bg="gray90")
  n_sims <- nrow(fit)
  for (i in sample(n_sims, 20)){
    lines(age, fit[i,ok], lwd=.5, col="gray50")
  }
  lines(age, colMeans(fit[,ok]), lwd=2)
}

#' #### Two different questions
question <- c("Support for same-sex marriage", "Do you know any gay people?")
variable <- c("gayFavorStateMarriage", "gayKnowSomeone")
index <- 0

#' #### Loop over two different questions
#' Prepare variables to store results
gay <- as.list(rep(NA, 2))
gay_sum <- as.list(rep(NA, 2))
gay_sum_2 <- as.list(rep(NA, 2))
gay_loess <- as.list(rep(NA, 2))
gay_spline <- as.list(rep(NA, 2))
gay_GP <- as.list(rep(NA, 2))
gay_bart <- as.list(rep(NA, 2))
gay_spline_2 <- as.list(rep(NA, 2))
for (j in 1:2){
  
  # Prepare the data
  gay[[j]] <- read.csv(root("Gay/data","naes04.csv"))
  gay[[j]] <- gay[[j]][!is.na(gay[[j]][,"age"]) & !is.na(gay[[j]][,variable[j]]),]
  gay[[j]]$age[gay[[j]]$age>90] <- 91
  y <- as.character(gay[[j]][,variable[j]])
  gay[[j]]$y <- ifelse(y=="Yes", 1, ifelse(y=="No", 0, NA))
  gender <- as.character(gay[[j]][,"gender"])
  gay[[j]]$male <- ifelse(gender=="Female", 0, ifelse(gender=="Male", 1, NA))
  
  uniq_age <- sort(unique(gay[[j]]$age))
  n_age <- length(uniq_age)
  tab <- table(gay[[j]]$age, gay[[j]]$y)
  y_sum <- as.vector(tab[,2])
  n_sum <- as.vector(tab[,1] + tab[,2])
  gay_sum[[j]] <- data.frame(n=n_sum, y=y_sum, age=uniq_age)
  gay_sum_2[[j]] <- data.frame(array(NA, c(n_age*2, 4), dimnames=list(NULL, c("n", "y", "age", "male"))))
  for (i1 in 1:n_age){
    for (i2 in 0:1){
      ok <- gay[[j]]$age==uniq_age[i1] & gay[[j]]$male==i2
      n_sum_2 <- sum(ok)
      y_sum_2 <- sum(gay[[j]]$y[ok])
      age_sum_2 <- uniq_age[i1]
      male_sum_2 <- i2
      gay_sum_2[[j]][n_age*i2 + i1,] <- c(n_sum_2, y_sum_2, age_sum_2, male_sum_2)
    }
  }
  
  # Make the plots
  gay_plot(question=question[j], title="Raw data from a national survey", savefigs = savefigs)

  # LOESS
  gay_loess[[j]] <- loess(y ~ age, data=gay[[j]])
  gay_loess_fit <- predict(gay_loess[[j]], data.frame(age=gay_sum[[j]]$age))
  gay_loess_fit <- matrix(gay_loess_fit, nrow=100, ncol=length(gay_loess_fit), byrow=TRUE)
  gay_plot(gay_loess_fit, question=question[j], title="Loess fit", savefigs = savefigs)

  # Splines
  gay_spline[[j]] <- stan_gamm4(I(y/n) ~ s(age), data=gay_sum[[j]], adapt_delta=0.99)
  gay_spline_fit <- posterior_linpred(gay_spline[[j]], data.frame(age=gay_sum[[j]]$age))
  gay_plot(gay_spline_fit, question=question[j], title="Spline fit and uncertainty", savefigs = savefigs)

  # GP represented with splines
  gay_GP[[j]] <- stan_gamm4(I(y/n) ~ age + s(age, bs="gp"), data=gay_sum[[j]], prior_smooth=student_t(df=4, scale=100), adapt_delta=0.99)
  gay_GP_fit <- posterior_linpred(gay_GP[[j]], data.frame(age=gay_sum[[j]]$age))
  gay_plot(gay_GP_fit, question=question[j], title="Gaussian process fit and uncertainty", savefigs = savefigs)
  # BART
  output <- capture.output(
    gay_bart[[j]] <- bart(gay[[j]]$age, gay[[j]]$y, matrix(uniq_age), ntree = 20))
  gay_bart_fit <- pnorm(gay_bart[[j]]$yhat.test)
  gay_plot(gay_bart_fit, question=question[j], title="Bart fit and uncertainty", savefigs = savefigs)

  # Another spline
  gay_spline_2[[j]] <- stan_gamm4(I(y/n) ~ s(age, male), data=gay_sum_2[[j]])
}

#' #### New graphs
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Gay/figs","gay10.pdf"), height=4, width=10)
#+
par(mar=c(3,2,1,1), mgp=c(1.7, .5, 0), tck=-.01)
par(mfrow=c(1,2))
for (j in 1:2){
  plot(gay_sum[[j]]$age, gay_sum[[j]]$y/gay_sum[[j]]$n, ylim=c(0,.69), yaxs="i", xlab="Age", ylab="", yaxt="n", bty="l", type="n", main=question[[j]])
  axis(2, seq(0,1,.1), rep("",11))
  axis(2, seq(0,1,.2), c("0","20%","40%","60%","80%","100%"))
  symbols(gay_sum[[j]]$age, gay_sum[[j]]$y/gay_sum[[j]]$n, circles=sqrt(gay_sum[[j]]$n), inches=.1, add=TRUE, fg="black", bg="gray70")
}
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Gay/figs","gay11.pdf"), height=10, width=7)
#+
par(mar=c(3,2,1,1), mgp=c(1.7, .5, 0), tck=-.01)
par(mfcol=c(4,2), oma=c(0,0,2.5,0))
for (j in 1:2){
  k=1
  gay_loess_fit <- predict(gay_loess[[j]], data.frame(age=gay_sum[[j]]$age))
  gay_loess_fit <- matrix(gay_loess_fit, nrow=100, ncol=length(gay_loess_fit), byrow=TRUE)
  gay_plot_1(gay_loess_fit, question=question[j], title="Loess fit", k_bottom=4)
  k=2
  gay_spline_fit <- posterior_linpred(gay_spline[[j]], data.frame(age=gay_sum[[j]]$age))
  gay_plot_1(gay_spline_fit, question=question[j], title="Spline fit and uncertainty", k_bottom=4)
  k=3
  gay_GP_fit <- posterior_linpred(gay_GP[[j]], data.frame(age=gay_sum[[j]]$age))
  gay_plot_1(gay_GP_fit, question="Support for same-sex marriage", title="Gaussian process fit and uncertainty", k_bottom=4)
  k=4
  gay_bart_fit <- pnorm(gay_bart[[j]]$yhat.test)
  gay_plot_1(gay_bart_fit, question=question[j], title="Bart fit and uncertainty", k_bottom=4)
}
mtext(paste(question[[1]], question[[2]], sep="                               "), side=3, line=1, outer=TRUE)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()


#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Gay/figs","gay12.pdf"), height=8, width=10)
#+
par(mar=c(3,2,1,1), mgp=c(1.7, .5, 0), tck=-.01)
par(mfcol=c(2,2))
for (j in 1:2){
  gay_spline_2_fit <- posterior_linpred(gay_spline_2[[j]], data.frame(age=gay_sum_2[[j]]$age, male=gay_sum_2[[j]]$male))
  for (m in 0:1){                    
    gay_plot_2(gay_spline_2_fit, question=question[j], title="2-dimensional spline fit", m=m)
  }
}
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()


#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Gay/figs","gay13.pdf"), height=5.5, width=7)
#+
par(mar=c(3,2,1,1), mgp=c(1.7, .5, 0), tck=-.01)
par(mfcol=c(2,2), oma=c(0,0,2.5,0))
for (j in 1:2){
  k=1
  gay_loess_fit <- predict(gay_loess[[j]], data.frame(age=gay_sum[[j]]$age))
  gay_loess_fit <- matrix(gay_loess_fit, nrow=100, ncol=length(gay_loess_fit), byrow=TRUE)
  gay_plot_1(gay_loess_fit, question=question[j], title="Loess fit", k_bottom=2)
  k=2
  gay_spline_fit <- posterior_linpred(gay_spline[[j]], data.frame(age=gay_sum[[j]]$age))
  gay_plot_1(gay_spline_fit, question=question[j], title="Spline fit and uncertainty", k_bottom=2)
  k=3
}
mtext(paste(question[[1]], question[[2]], sep="                               "), side=3, line=1, outer=TRUE)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
