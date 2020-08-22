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

#' Average predictive comparisons for a logistic regression model:
#' wells in Bangladesh. See Chapter 14 in Regression and Other Stories.
#' 
#' -------------
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(message=FALSE, error=FALSE, warning=FALSE, comment=NA)

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

#' #### Predict switching with distance, arsenic, and education
#+ results='hide'
fit_7 <- stan_glm(switch ~ dist100 + arsenic + educ4,
                  family = binomial(link="logit"), data = wells)
#'
print(fit_7, digits=2)

#' #### Average predictive difference in probability of switching 1)
#' comparing households that are next to, or 100 meters from, the nearest safe well
b <- coef(fit_7)
hi <- 1
lo <- 0
delta <- invlogit (b[1] + b[2]*hi + b[3]*wells$arsenic + b[4]*wells$educ4) -
         invlogit (b[1] + b[2]*lo + b[3]*wells$arsenic + b[4]*wells$educ4)
round(mean(delta), 2)

#' #### Average predictive difference in probability of switching 2)
#' comparing households with existing arsenic levels of 0.5 and 1.0
b <- coef(fit_7)
hi <- 1.0
lo <- 0.5
delta <- invlogit (b[1] + b[2]*wells$dist100 + b[3]*hi + b[4]*wells$educ4) -
         invlogit (b[1] + b[2]*wells$dist100 + b[3]*lo + b[4]*wells$educ4)
round(mean(delta), 2)

#' #### Average predictive difference in probability of switching 3)
#' comparing householders with 0 and 12 years of education
b <- coef(fit_7)
hi <- 3
lo <- 0
delta <- invlogit (b[1]+b[2]*wells$dist100+b[3]*wells$arsenic+b[4]*hi) -
         invlogit (b[1]+b[2]*wells$dist100+b[3]*wells$arsenic+b[4]*lo)
round(mean(delta), 2)

#' #### Predict switching with distance, arsenic, education and interactions
#+ results='hide'
wells$c_dist100 <- wells$dist100 - mean(wells$dist100)
wells$c_arsenic <- wells$arsenic - mean(wells$arsenic)
wells$c_educ4 <- wells$educ4 - mean(wells$educ4)
fit_8 <- stan_glm(switch ~ c_dist100 + c_arsenic + c_educ4 +
                      c_dist100:c_educ4 + c_arsenic:c_educ4,
                  family = binomial(link="logit"), data = wells)
#'
print(fit_8, digits=2)

#' ### Average predictive difference in probability of switching 4)
#' comparing households that are next to, or 100 meters from, the nearest safe well
b <- coef(fit_8)
hi <- 1
lo <- 0
delta <- invlogit(b[1] + b[2]*hi + b[3]*wells$c_arsenic +
                  b[4]*wells$c_educ4 + b[5]*hi*wells$c_educ4 +
                  b[6]*wells$c_arsenic*wells$c_educ4) -
         invlogit(b[1] + b[2]*lo + b[3]*wells$c_arsenic +
                  b[4]*wells$c_educ4 + b[5]*lo*wells$c_educ4 +
                  b[6]*wells$c_arsenic*wells$c_educ4)
round(mean(delta), 2)
