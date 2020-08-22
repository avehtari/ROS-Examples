#' ---
#' title: "Regression and Other Stories: National election study"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     theme: readable
#'     toc: true
#'     toc_depth: 2
#'     toc_float: true
#'     code_download: true
#' ---

#' Logistic regression, identifiability, and separation. See Chapters
#' 13 and 14 in Regression and Other Stories.
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
library("arm")
library("rstanarm")
library("foreign")

#' #### Load data
nes <- read.table(root("NES/data","nes.txt"), header=TRUE)
head(nes)

#' Use first only data from 1992 and remove missing data
ok <- nes$year==1992 & !is.na(nes$rvote) & !is.na(nes$dvote) & (nes$rvote==1 | nes$dvote==1)
nes92 <- nes[ok,]

#' ## A single predictor logistic regression
#' 

#' #### Logistic regression of vote preference on income
fit_1 <- stan_glm(rvote ~ income, family=binomial(link="logit"), data=nes92,
                  refresh=0)
print(fit_1)

#' #### Predictions
new <- data.frame(income=5)
#' Predict vote preference point estimate
pred <- predict(fit_1, type="response", newdata=new)
print(pred, digits=2)
#' Linear predictor with uncertainty
linpred <- posterior_linpred(fit_1, newdata=new)
head(linpred)
print(c(mean(linpred), sd(linpred)), digits=2)
#' Expected outcome with uncertainty
epred <- posterior_epred(fit_1, newdata=new)
head(epred)
print(c(mean(epred), sd(epred)), digits=2)
#' Predictive distribution for a new observation
postpred <- posterior_predict(fit_1, newdata=new)
head(postpred)
print(c(mean(postpred), sd(postpred)), digits=2)

#' #### Prediction given a range of input values
new <- data.frame(income=1:5)
pred <- predict(fit_1, type="response", newdata=new)
linpred <- posterior_linpred(fit_1, newdata=new)
head(linpred)
epred <- posterior_epred(fit_1, newdata=new)
head(epred)
postpred <- posterior_predict(fit_1, newdata=new)
head(postpred)
#' the posterior probability, according to the fitted model, that Bush
#' was more popular among people with income level 5 than among people
#' with income level 4
mean(epred[,5] > epred[,4])
#' 95\% posterior distribution for the difference in support for Bush,
#' comparing people in the richest to the second-richest category
quantile(epred[,5] - epred[,4], c(0.025, 0.975))

#' ## Fake data example
data <- data.frame(rvote=rep(c(0,1), 10), income=1:20)
fit_f <- stan_glm(rvote ~ income, family=binomial(link="logit"), data=data,
                  refresh=0)
new <- data.frame(income=5)
predict <- posterior_predict(fit_f, newdata=new)

#' #### Plot jittered data and prediction from the logistic regression
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("NES/figs","income1a.pdf"), height=2.8, width=3.8)
#+
n <- nrow(nes92)
income_jitt <- nes92$income + runif(n, -.2, .2)
vote_jitt <- nes92$rvote + ifelse(nes92$rvote==0, runif(n, .005, .05), runif(n, -.05, -.005))
par(mar=c(3,3,1,.1), tck=-.01, mgp=c(1.7, .3, 0))
ok <- nes92$presvote<3
vote <- nes92$presvote[ok] - 1
income <- nes92$income[ok]
curve(invlogit(fit_1$coef[1] + fit_1$coef[2]*x), 1, 5, ylim=c(0,1),
  xlim=c(-2,8), xaxt="n", xaxs="i", 
  ylab="Pr (Republican vote)", xlab="Income", lwd=4, yaxs="i")
curve(invlogit(fit_1$coef[1] + fit_1$coef[2]*x), -2, 8, lwd=.5, add=TRUE)
axis(1, 1:5)
mtext("(poor)", 1, 1.2, at=1, adj=.5)
mtext("(rich)", 1, 1.2, at=5, adj=.5)
points(income_jitt, vote_jitt, pch=20, cex=.1)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Plot jittered data and prediction with uncertainties
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("NES/figs","income1b.pdf"), height=2.8, width=3.8)
#+
par(mar=c(3,3,1,.1), tck=-.01, mgp=c(1.7, .3, 0))
ok <- nes92$presvote<3
vote <- nes92$presvote[ok] - 1
income <- nes92$income[ok]
curve (invlogit(fit_1$coef[1] + fit_1$coef[2]*x), .5, 5.5, ylim=c(0,1),
  xlim=c(.5, 5.5), xaxt="n", xaxs="i", 
  ylab="Pr (Republican vote)", xlab="Income", yaxs="i")
axis(1, 1:5)
mtext("(poor)", 1, 1.2, at=1, adj=.5)
mtext("(rich)", 1, 1.2, at=5, adj=.5)
sims_1 <- as.matrix(fit_1)
n_sims <- nrow(sims_1)
for (j in sample(n_sims, 20)){
  curve(invlogit(sims_1[j,1] +sims_1[j,2]*x), .5, 5.5, lwd=.5, col="gray", add=TRUE)
}
curve(invlogit(fit_1$coef[1] + fit_1$coef[2]*x), .5, 5.5, add=TRUE)
points(income_jitt, vote_jitt, pch=20, cex=.1)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Series of regressions for different years
yrs <- seq(1952, 2000, 4)
n_yrs <- length(yrs)
fits <- array(NA, c(n_yrs, 3), dimnames <- list(yrs, c("year", "coef", "se")))
for (j in 1:n_yrs){
  yr <- yrs[j]
  ok <- (nes$year==yr & !is.na(nes$presvote) & nes$presvote<3 &
         !is.na(nes$vote) & !is.na(nes$income))
  vote <- nes$presvote[ok] - 1
  income <- nes$income[ok]
  fit_y <- stan_glm(vote ~ income, family=binomial(link="logit"),
                    data = data.frame(vote, income),
                    warmup = 500, iter = 1500, refresh = 0, 
                    save_warmup = FALSE, cores = 1, open_progress = FALSE)
  fits[j,] <- c(yr, coef(fit_y)[2], se(fit_y)[2])
}

#' #### Plot the series of regression
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("NES/figs","incomeseries.pdf"), height=3.4, width=4.9)
#+
par(mar=c(3,2.5,1,.2), tck=-.01, mgp=c(1.5, .3, 0))
plot (fits[,"year"], fits[,"coef"], xlim=c(1950,2000), ylim=range(fits[,"coef"]-fits[,"se"], fits[,"coef"]+fits[,"se"]),
  pch=20, ylab="Coefficient of income", xlab="Year", bty="l")
for (j in 1:n_yrs){
  lines(rep(fits[j,"year"], 2), fits[j,"coef"] + fits[j,"se"]*c(-1,1), lwd=.5)
}
abline(0,0,lwd=.5, lty=2)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()


#' ## Predictive accuracy and log score for logistic regression
#' 

#' #### Estimate the with-in sample predictive accuracy
predp <- fitted(fit_1)
round(c(mean(predp[nes92$rvote==1]), mean(1-predp[nes92$rvote==0])), 3)

#' **Estimate the predictive performance of a model using
#' within-sample log-score**
round(sum(log(c(predp[nes92$rvote==1], 1-predp[nes92$rvote==0]))), 1)

#' **Estimate the predictive performance of a model using
#' leave-one-out log-score (elpd_loo)**
loo(fit_1)


#' ## Identifiability and separation
#' 

#' #### Illustrate nonidentifiability of logistic regression<br>
#' Use "black" as a predictor (nonidentifiability in 1964)
#+ results='hide'
fits_2 <- array(NA, c(n_yrs, 4, 2, 2), dimnames <- list(yrs, c("Intercept", "female", "black", "income"), c("coef", "se"), c("glm", "bayes")))
ok <- (!is.na(nes$rvote) & !is.na(nes$female) & !is.na(nes$black) & !is.na(nes$income))
for (j in 1:n_yrs){
  print(yrs[j])
  fit_glm <- glm(rvote ~ female + black + income, subset=(year==yrs[j]),
                 family=binomial(link="logit"), data=nes[ok,])
  fits_2[j,,1,1] <- coef(fit_glm)
  fits_2[j,,2,1] <- se.coef(fit_glm)
  fit_bayes <- stan_glm(rvote ~ female + black + income, subset=(year==yrs[j]),
                        family=binomial(link="logit"), data=nes[ok,],
                        warmup = 500, iter = 1500, refresh = 0, 
                        save_warmup = FALSE, cores = 1, open_progress = FALSE)
  fits_2[j,,1,2] <- coef(fit_bayes)
  fits_2[j,,2,2] <- se(fit_bayes)
  display(fit_glm)
  print(fit_bayes)
}

#' #### Plot illustration on nonidentifiability of logistic regression
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("NES/figs","separation_compare.pdf"), height=2.8, width=8.3)
#+ fig.width=9, fig.height=6
par(mfrow=c(2,5), mar=c(3,3,0,1), tck=-.02, mgp=c(1.2,.3,0), oma=c(0,0,3,0))
for (k in 1:2){
  plot(0,0,xlab="",ylab="",xaxt="n",yaxt="n",bty="n",type="n")
  text(0, 0, if (k==1) "Maximum\nlikelihood\nestimate,\nfrom glm()" else "Bayes estimate\nwith default prior,\nfrom stan_glm()", cex=1.3)
  for (l in 1:4){
    rng <- range(fits_2[,l,"coef",] - fits_2[,l,"se",], fits_2[,l,"coef",] + fits_2[,l,"se",])
    if (l==3) rng <- c(-18, 1)
    plot(yrs, fits_2[,l,"coef",k], ylim=rng, ylab="Coefficient", xlab=if (k==2) "Year" else "", bty="l", xaxt="n", pch=20)
    axis(1, c(1960, 1980, 2000))
    abline(0,0,lwd=.5,lty=2)
    for (j in 1:n_yrs){
      lines(rep(yrs[j], 2), c(fits_2[j,l,"coef",k] - fits_2[j,l,"se",k], fits_2[j,l,"coef",k] + fits_2[j,l,"se",k]), lwd=.5)
    }
    if (k==1) mtext(dimnames(fits_2)[[2]][l], 3, 1.5, cex=.8)
  }
}
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
