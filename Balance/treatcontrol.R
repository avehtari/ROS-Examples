#' ---
#' title: "Regression and Other Stories: Balance"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' **Load libraries**
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library("rprojroot")
root<-has_dirname("RAOS-Examples")$make_fix_file()

#' **Plot figures**
savepdf <- FALSE
#+ eval=FALSE, include=FALSE
savepdf <- TRUE
#+
bell <- function(filename, mu, sd, lo, hi, ymax){
  if (savepdf) pdf(filename, height = 3, width = 9)
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
  if (savepdf) dev.off()
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
