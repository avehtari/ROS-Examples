#' ---
#' title: "Regression and Other Stories: Helicopters"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Example data file for helicopter flying time exercise
#' 
#' -------------
#' 

#' **Load libraries**
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library("rprojroot")
root<-has_dirname("RAOS-Examples")$make_fix_file()

#' **Load data**
helicopters <- read.table(root("Helicopters/data","helicopters.txt"), header=TRUE)

#' **Display the example data**
print(helicopters)
