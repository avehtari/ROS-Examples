#' ---
#' title: "Regression and Other Stories: Pollution"
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

#' A pollution data set. See Chapter 12 in Regression and Other
#' Stories.
#'
#' Source: McDonald, G.C. and Schwing, R.C. (1973) 'Instabilities of
#' regression estimates relating air pollution to mortality',
#' Technometrics, vol.15, 463-482. See data/pollution.txt for the
#' explanation of the variables.
#' 
#' -------------
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(message=FALSE, error=FALSE, warning=FALSE, comment=NA)

#' **Load packages**
library("rprojroot")
root<-has_file(".ROS-Examples-root")$make_fix_file()

#' **Load data**
pollution <- read.csv(root("Pollution/data","pollution.csv"))
head(pollution)
