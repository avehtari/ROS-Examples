#' ---
#' title: "Regression and Other Stories: Arsenic"
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

#' Building a logistic regression model: wells in Bangladesh. See
#' Chapter 13 in Regression and Other Stories.
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
library("loo")
invlogit <- plogis

#' #### Load data
wells <- read.csv(root("Arsenic/data","wells.csv"))
head(wells)
n <- nrow(wells)

#' ## Null model
#' 

#' #### Log-score for coin flipping
prob <- 0.5
round(log(prob)*sum(wells$switch) + log(1-prob)*sum(1-wells$switch),1)

#' #### Log-score for intercept model
round(prob <- mean(wells$switch),2)
round(log(prob)*sum(wells$switch) + log(1-prob)*sum(1-wells$switch),1)

#' ## A single predictor
#'

#' #### Fit a model using distance to the nearest safe well
#+ results='hide'
fit_1 <- stan_glm(switch ~ dist, family = binomial(link = "logit"), data=wells)
#'
print(fit_1, digits=3)
#' #### LOO log score
(loo1 <- loo(fit_1))

#' #### Histogram of distances
#+ eval=FALSE, include=FALSE
if (savefigs) postscript(root("Arsenic/figs","arsenic.distances.bnew.ps"),
                         height=3, width=4, horizontal=TRUE)
#+
hist(wells$dist, breaks=seq(0,10+max(wells$dist),10), freq=TRUE,
     xlab="Distance (in meters) to nearest safe well", ylab="", main="", mgp=c(2,.5,0))
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Scale distance in meters to distance in 100 meters
wells$dist100 <- wells$dist/100

#' #### Fit a model using scaled distance to the nearest safe well
#+ results='hide'
fit_2 <- stan_glm(switch ~ dist100, family = binomial(link = "logit"), data=wells)
#'
print(fit_2, digits=2)
#' #### LOO log score
(loo2 <- loo(fit_2, save_psis = TRUE))

#' #### Plot model fit
jitter_binary <- function(a, jitt=.05){
  a + (1-2*a)*runif(length(a),0,jitt)
}
#+ eval=FALSE, include=FALSE
if (savefigs) postscript(root("Arsenic/figs","arsenic.logitfit.1new.a.ps"),
                         height=3.5, width=4, horizontal=TRUE)
#+
plot(c(0,max(wells$dist, na.rm=TRUE)*1.02), c(0,1),
     xlab="Distance (in meters) to nearest safe well", ylab="Pr (switching)",
     type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
curve(invlogit(coef(fit_1)[1]+coef(fit_1)[2]*x), lwd=1, add=TRUE)
points(wells$dist, jitter_binary(wells$switch), pch=20, cex=.1)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Plot uncertainty in the estimated coefficients
#+ eval=FALSE, include=FALSE
if (savefigs) postscript(root("Arsenic/figs","arsenic.logitfit.scatterplot.ps"),
                         height=3.5, width=3.5, horizontal=TRUE)
#+
sims <- as.matrix(fit_2)
par(pty="s")
plot(sims[1:500,1], sims[1:500,2], xlim=c(.4,.8), ylim=c(-1,0),
     xlab=expression(beta[0]), ylab=expression(beta[1]), mgp=c(1.5,.5,0),
     pch=20, cex=.5, xaxt="n", yaxt="n")
axis(1, seq(.4,.8,.2), mgp=c(1.5,.5,0))
axis(2, seq(-1,0,.5), mgp=c(1.5,.5,0))
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Plot uncertainty in the estimated predictions
#+ eval=FALSE, include=FALSE
if (savefigs) postscript(root("Arsenic/figs","arsenic.logitfit.1new.b.ps"),
                         height=3.5, width=4, horizontal=TRUE)
#+
plot(c(0,max(wells$dist, na.rm=T)*1.02), c(0,1),
     xlab="Distance (in meters) to nearest safe well", ylab="Pr (switching)",
     type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
for (j in 1:20) {
    curve (invlogit(sims[j,1]+sims[j,2]*x/100), lwd=.5,
           col="darkgray", add=TRUE)
}
curve(invlogit(coef(fit_2)[1]+coef(fit_2)[2]*x/100), lwd=1, add=T)
points(wells$dist, jitter_binary(wells$switch), pch=20, cex=.1)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()


#' ## Two predictors
#' 

#' #### Histogram of arsenic levels
#+ eval=FALSE, include=FALSE
if (savefigs) postscript(root("Arsenic/figs","arsenic.levels.a.ps"),
                         height=3, width=4, horizontal=TRUE)
#+
hist(wells$arsenic, breaks=seq(0,.25+max(wells$arsenic),.25), freq=TRUE,
     xlab="Arsenic concentration in well water", ylab="", main="", mgp=c(2,.5,0))
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Fit a model using scaled distance and arsenic level
#+ results='hide'
fit_3 <- stan_glm(switch ~ dist100 + arsenic, family = binomial(link = "logit"),
                  data=wells)
#'
print(fit_3, digits=2)
#' #### LOO log score
(loo3 <- loo(fit_3, save_psis = TRUE))
#' #### Compare models
loo_compare(loo2, loo3)

#' #### Average improvement in LOO predictive probabilities<br>
#' from dist100 to dist100 + arsenic
pred2 <- loo_predict(fit_2, psis_object = loo2$psis_object)$value
pred3 <- loo_predict(fit_3, psis_object = loo3$psis_object)$value
round(mean(c(pred3[wells$switch==1]-pred2[wells$switch==1],pred2[wells$switch==0]-pred3[wells$switch==0])),3)

#' #### Plot model fits
#+ eval=FALSE, include=FALSE
if (savefigs) postscript(root("Arsenic/figs","arsenic.2variables.a.ps"),
                         height=3.5, width=4, horizontal=TRUE)
#+
plot(c(0,max(wells$dist,na.rm=T)*1.02), c(0,1),
     xlab="Distance (in meters) to nearest safe well", ylab="Pr (switching)",
     type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
points(wells$dist, jitter_binary(wells$switch), pch=20, cex=.1)
curve(invlogit(coef(fit_3)[1]+coef(fit_3)[2]*x/100+coef(fit_3)[3]*.50), lwd=.5, add=T)
curve(invlogit(coef(fit_3)[1]+coef(fit_3)[2]*x/100+coef(fit_3)[3]*1.00), lwd=.5, add=T)
text(50, .27, "if As = 0.5", adj=0, cex=.8)
text(75, .50, "if As = 1.0", adj=0, cex=.8)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
#+ eval=FALSE, include=FALSE
if (savefigs) postscript(root("Arsenic/figs","arsenic.2variables.b.ps"),
                         height=3.5, width=4, horizontal=TRUE)
#+
plot(c(0,max(wells$arsenic,na.rm=T)*1.02), c(0,1),
     xlab="Arsenic concentration in well water", ylab="Pr (switching)",
     type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
points(wells$arsenic, jitter_binary(wells$switch), pch=20, cex=.1)
curve(invlogit(coef(fit_3)[1]+coef(fit_3)[2]*0+coef(fit_3)[3]*x), from=0.5, lwd=.5, add=T)
curve(invlogit(coef(fit_3)[1]+coef(fit_3)[2]*0.5+coef(fit_3)[3]*x), from=0.5, lwd=.5, add=T)
text(.5, .78, "if dist = 0", adj=0, cex=.8)
text(2, .6, "if dist = 50", adj=0, cex=.8)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' ## Interaction
#' 

#' #### Fit a model using scaled distance, arsenic level, and an interaction
#+ results='hide'
fit_4 <- stan_glm(switch ~ dist100 + arsenic + dist100:arsenic,
                  family = binomial(link="logit"), data = wells)
#'
print(fit_4, digits=2)
#' #### LOO log score
(loo4 <- loo(fit_4))
#' #### Compare models
loo_compare(loo3, loo4)

#' #### Centering the input variables
wells$c_dist100 <- wells$dist100 - mean(wells$dist100)
wells$c_arsenic <- wells$arsenic - mean(wells$arsenic)
#+ results='hide'
fit_5 <- stan_glm(switch ~ c_dist100 + c_arsenic + c_dist100:c_arsenic,
                  family = binomial(link="logit"), data = wells)
#'
print(fit_5, digits=2)

#' #### Plot model fits
#+ eval=FALSE, include=FALSE
if (savefigs) postscript(root("Arsenic/figs","arsenic.interact.a.ps"),
                         height=3.5, width=4, horizontal=TRUE)
#+
plot(c(0,max(wells$dist,na.rm=T)*1.02), c(0,1),
     xlab="Distance (in meters) to nearest safe well", ylab="Pr (switching)",
     type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
points(wells$dist, jitter_binary(wells$switch), pch=20, cex=.1)
curve(invlogit(coef(fit_4)[1]+coef(fit_4)[2]*x/100+coef(fit_4)[3]*.50+coef(fit_4)[4]*x/100*.50), lwd=.5, add=T)
curve(invlogit(coef(fit_4)[1]+coef(fit_4)[2]*x/100+coef(fit_4)[3]*1.00+coef(fit_4)[4]*x/100*1.00), lwd=.5, add=T)
text (50, .29, "if As = 0.5", adj=0, cex=.8)
text (75, .50, "if As = 1.0", adj=0, cex=.8)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
#+ eval=FALSE, include=FALSE
if (savefigs) postscript(root("Arsenic/figs","arsenic.interact.b.ps"),
                         height=3.5, width=4, horizontal=TRUE)
#+
plot(c(0,max(wells$arsenic,na.rm=T)*1.02), c(0,1),
     xlab="Arsenic concentration in well water", ylab="Pr (switching)",
     type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
points(wells$arsenic, jitter_binary(wells$switch), pch=20, cex=.1)
curve(invlogit(coef(fit_4)[1]+coef(fit_4)[2]*0+coef(fit_4)[3]*x+coef(fit_4)[4]*0*x), from=0.5, lwd=.5, add=T)
curve(invlogit(coef(fit_4)[1]+coef(fit_4)[2]*0.5+coef(fit_4)[3]*x+coef(fit_4)[4]*0.5*x), from=0.5, lwd=.5, add=T)
text (.5, .78, "if dist = 0", adj=0, cex=.8)
text (2, .6, "if dist = 50", adj=0, cex=.8)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' ## More predictors
#' 

#' #### Adding social predictors
#+ results='hide'
fit_6 <- stan_glm(switch ~ dist100 + arsenic + educ4 + assoc,
                  family = binomial(link="logit"), data = wells)
#'
print(fit_6, digits=2)
#' #### LOO log score
(loo6 <- loo(fit_6))
#' #### Compare models
loo_compare(loo4, loo6)

#' #### Remove assoc
#+ results='hide'
fit_7 <- stan_glm(switch ~ dist100 + arsenic + educ4,
                  family = binomial(link="logit"), data = wells)
#'
print(fit_7, digits=2)
#' #### LOO log score
(loo7 <- loo(fit_7))
#' #### Compare models
loo_compare(loo4, loo7)
loo_compare(loo6, loo7)

#' #### Add interactions with education
wells$c_educ4 <- wells$educ4 - mean(wells$educ4)
#+ results='hide'
fit_8 <- stan_glm(switch ~ c_dist100 + c_arsenic + c_educ4 +
                      c_dist100:c_educ4 + c_arsenic:c_educ4,
                  family = binomial(link="logit"), data = wells)
#'
print(fit_8, digits=2)
#' #### LOO log score
(loo8 <- loo(fit_8, save_psis=TRUE))
#' #### Compare models
loo_compare(loo3, loo8)
loo_compare(loo7, loo8)




#' #### Average improvement in LOO predictive probabilities<br>
#' from dist100 + arsenic to dist100 + arsenic + educ4 + dist100:educ4 + arsenic:educ4
pred8 <- loo_predict(fit_8, psis_object = loo8$psis_object)$value
round(mean(c(pred8[wells$switch==1]-pred3[wells$switch==1],pred3[wells$switch==0]-pred8[wells$switch==0])),3)

#' ## Transformation of variable
#' 

#' #### Fit a model using scaled distance and log arsenic level
wells$log_arsenic <- log(wells$arsenic)
#+ results='hide'
fit_3a <- stan_glm(switch ~ dist100 + log_arsenic, family = binomial(link = "logit"),
                   data = wells)
#'
print(fit_3a, digits=2)
#' #### LOO log score
(loo3a <- loo(fit_3a))
#' #### Compare models
loo_compare(loo3, loo3a)

#' #### Fit a model using scaled distance, log arsenic level, and an interaction<br>
#+ results='hide'
fit_4a <- stan_glm(switch ~ dist100 + log_arsenic + dist100:log_arsenic,
                  family = binomial(link = "logit"), data = wells)
#'
print(fit_4a, digits=2)
#' #### LOO log score
(loo4a <- loo(fit_4a))
#' #### Compare models
loo_compare(loo3a, loo4a)

#' #### Add interactions with education
wells$c_log_arsenic <- wells$log_arsenic - mean(wells$log_arsenic)
#+ results='hide'
fit_8a <- stan_glm(switch ~ c_dist100 + c_log_arsenic + c_educ4 +
                      c_dist100:c_educ4 + c_log_arsenic:c_educ4,
                  family = binomial(link="logit"), data = wells)
#'
print(fit_8a, digits=2)
#' #### LOO log score
(loo8a <- loo(fit_8a, save_psis=TRUE))
#' #### Compare models
loo_compare(loo8, loo8a)
