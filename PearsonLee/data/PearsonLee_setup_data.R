#' ---
#' title: "Regression and Other Stories: Pearson Lee Heights"
#' author: "Andrew Gelman, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---
#'

#' The heredity of height. Published in 1903 by Karl Pearson and Alice Lee.
#' 
#' -------------
#' 

#' **Load libraries**
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library(here)
library(HistData)

#' Load original Pearson Lee data and store it in a file
data(PearsonLee)
write.table(here("PearsonLee/data", "PearsonLee.txt"))
#' Make data file for heights of daughters and mothers
data <- read.table(here("PearsonLee/data","PearsonLee.txt"), header=TRUE)
print(data[1:10,])
subset <- data[,"gp"]=="md"
daughter_height <- rep(data[subset, "child"], 4*data[subset,"frequency"])
mother_height <- rep(data[subset, "parent"], 4*data[subset,"frequency"])
write.table(cbind(daughter_height, mother_height),
            here("PearsonLee/data","Heights.txt"), row.names=FALSE)
