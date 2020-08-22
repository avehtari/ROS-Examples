#' ---
#' title: "Regression and Other Stories: RiskyBehavior"
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
#'
#' Data from a randomized trial targeting couples at high risk of HIV
#' infection. The intervention provided counseling sessions regarding
#' practices that could reduce their likelihood of contracting
#' HIV. Couples were randomized either to a control group, a group in
#' which just the woman participated, or a group in which both members
#' of the couple participated. One of the outcomes examined after
#' three months was “number of unprotected sex acts.”. See Chapter 15
#' in Regression and Other Stories.
#' 
#' Reference: El-Bassel, N., Witte, S. S., Gilbert, L., Wu, E.,
#' Chang, M., Hill, J., and Steinglass, P. (2003). The efficacy of a
#' relationship-based HIV/STD prevention program for heterosexual
#' couples. *American journal of public health*, **93**, 963--969.
#'
#' -------------
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(message=FALSE, error=FALSE, warning=FALSE, comment=NA)
# switch this to TRUE to save figures in separate files
savefigs <- FALSE

#' **Load packages**
library("rprojroot")
root<-has_file(".ROS-Examples-root")$make_fix_file()

#' **Load data**
risky <- read.csv(root("RiskyBehavior/data","risky.csv"))
head(risky)
