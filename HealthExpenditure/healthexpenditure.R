#' ---
#' title: "Regression and Other Stories: Health Expenditure"
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

#' Health Expenditure - Discovery through graphs of data and
#' models. See Chapter 2 in Regression and Other Stories.
#' 
#' -------------
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(message=FALSE, error=FALSE, warning=FALSE, comment=NA)
# switch this to TRUE to save figures in separate files
savefigs <- FALSE

#' #### Load packages
library("rprojroot")
root<-has_dirname("ROS-Examples")$make_fix_file()

#' #### Load data
read.page <- function (datapage){
  variables.keep <- paste ("X", 1960:2007, sep="")
  data <- read.csv (datapage, skip=3)
  data <- data[1:30,]           # file has only 30 rows of data
  countries <- as.character (data[,"X"])
  numbers <- data[,variables.keep]
  n <- length(countries)
  recent.data <- rep(NA, n)
  for (i in 1:n) {
    y <- as.numeric(numbers[i,])
    ok <- !is.na(y)
    if (sum(ok)>0) {
      years.keep <- (1:length(y))[ok]
      recent.data[i] <- y[max(years.keep)]
    }
    else {
      recent.data[i] <- NA
    }
  }
  return(list (countries=countries, recent.data=recent.data))
}
expend <- read.page(root("HealthExpenditure/data","healthexpenditure.csv"))
life <- read.page(root("HealthExpenditure/data","lifeexpectancy.csv"))
doctor <- read.page(root("HealthExpenditure/data","doctorvisits.csv"))
# shorten some country names
countries <- expend$countries
countries[countries=="Czech Republic"] <- "Czech"
countries[countries=="New Zealand"] <- "N.Zealand"
countries[countries=="Slovak Republic"] <- "Slovakia"
countries[countries=="United Kingdom"] <- "UK"
countries[countries=="United States"] <- "USA"
# specific colors
color <- ifelse (countries %in% c("USA","Mexico"), "red","black")
#
expend <- expend$recent.data
life <- life$recent.data
doctor <- doctor$recent.data

#' #### Scatterplot
#+ eval=FALSE, include=FALSE
png(root("HealthExpenditure/figs","healthscatter.png"), height=600, width=700)
#+
par(mgp=c(1.7,.5,0), tck=-.01, mar=c(3,3,.1,.1))
plot(expend, life, xlim=c(0,1.05*max(expend)), xaxs="i",
      type="n", xlab="Health care spending (PPP US$)",
      ylab="Life expectancy (years)")
#symbols(expend, life, circles=sqrt(doctor), inches=.8, add=TRUE, fg="gray80")
text(expend, life, countries, col=color)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Plot  scatterplot, excluding some countries
removec <- countries %in% c("Netherlands", "Belgium", "Germany",
  "Ireland", "Iceland", "Greece", "Italy", "Sweden", "UK")
#+ eval=FALSE, include=FALSE
png(root("HealthExpenditure/figs","healthscatter2.png"), height=600, width=700)
#+
par(mgp=c(2.5,.7,0), tck=-.01, mar=c(4,4,.1,.1))
plot(expend[!removec], life[!removec], xlim=c(0,1.05*max(expend)),
      xaxs="i",  type="n", xlab="Health care spending (PPP US$)",
      ylab="Life expectancy (years)", cex.axis=1.3, cex.lab=1.3, las=1, xaxt="n", bty="l")
#symbols(expend[!removec], life[!removec], circles=sqrt(doctor[!removec]), inches=.8, add=TRUE, fg="gray80")
axis(1, seq(0,8000,2000), cex.axis=1.3, cex.lab=1.3)
text(expend[!removec], life[!removec], countries[!removec],
      col=color[!removec], cex=1.3)
for (x in seq(2000,6000,2000)) abline(v=x, col="gray", lwd=.5)
for (y in seq(74,82,2)) abline(y,0,col="gray", lwd=.5)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off ()

#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("HealthExpenditure/figs","healthscatter3.pdf"), height=4, width=5.5)
#+
par(mgp=c(1.7,.5, 0), tck=-.01, mar=c(3,3,.1,.1))
plot(expend[!removec], life[!removec], xlim=c(0,1.05*max(expend)),
      xaxs="i",  type="n", xlab="Health care spending (PPP US$)",
      ylab="Life expectancy (years)", bty="l", xaxt="n")
axis(1, seq(0,6000,2000))
#symbols (expend[!removec], life[!removec], circles=sqrt(doctor[!removec]), inches=.8, add=TRUE, fg="gray80")
text(expend[!removec], life[!removec], countries[!removec], cex=.9)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

