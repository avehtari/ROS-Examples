#' ---
#' title: "Regression and Other Stories: Balance"
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

#' Figures for hypothetical example of zero causal effect but positive
#' predictive comparison.  See Chapter 20 in Regression and Other
#' Stories.
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

#' #### Plot figures
bell <- function(filename, mu, sd, lo, hi, ymax){
  if (savefigs) pdf(filename, height = 3, width = 9)
  par(mar = c(4,0,3,0))
  curve(
    dnorm(x, mu, sd),
    from = lo, to = hi,
    ylim = c(0, ymax),
    xlab = "", ylab = "", bty = "n",
    yaxt = "n", yaxs = "i",
    cex.axis = 3.5,
    mgp = c(3,3,0)
  )
  if (savefigs) dev.off()
}

bell("bell1h.pdf", 2, .4, .3, 5.7, 1)
bell("bell1l.pdf", 2, .4, .3, 5.7, 3)
bell("bell2.pdf", 3, .4, .3, 5.7, 2)
bell("bell3l.pdf", 4, .4, .3, 5.7, 3)
bell("bell3h.pdf", 4, .4, .3, 5.7, 1)

bell("bell4l.pdf", 1.5, .4, .3, 5.7, 3)
bell("bell5h.pdf", 2.5, .4, .3, 5.7, 1)
bell("bell5.pdf", 2.5, .4, .3, 5.7, 2)
bell("bell6.pdf", 3.5, .4, .3, 5.7, 2)
bell("bell6h.pdf", 3.5, .4, .3, 5.7, 1)
bell("bell7l.pdf", 4.5, .4, .3, 5.7, 3)
