#' ---
#' title: "Regression and Other Stories: Helicopters"
#' author: "Andrew Gelman, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Example data file for helicopter flying time exercise
#' 
#' -------------
#' 

#' **Load libraries**
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library("here")

#' **Load data**
helicopters <- read.table(here("Helicopters/data","helicopters.txt"), header=TRUE)

#' **Display the example data**
print(helicopters)
