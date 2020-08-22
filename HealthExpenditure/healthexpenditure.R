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
library("ggplot2")
library("bayesplot")
theme_set(bayesplot::theme_default(base_family = "sans"))
library("rprojroot")
root<-has_file(".ROS-Examples-root")$make_fix_file()

#' #### Load data
health <- read.table(root("HealthExpenditure/data","healthdata.txt"), header=TRUE)
head(health)
# Use red for USA and Mexico
color <- ifelse(health$country %in% c("USA","Mexico"), "red","black")

#' #### Scatterplot
#'
#' (see ggplot versions at the end)
#' 
#' All countries:
#+ eval=FALSE, include=FALSE
png(root("HealthExpenditure/figs","healthscatter.png"), height=600, width=700)
#+
par(mgp=c(1.7,.5,0), tck=-.01, mar=c(3,3,.1,.1))
plot(health$spending, health$lifespan, xlim=c(0,1.05*max(health$spending)), xaxs="i",
      type="n", xlab="Health care spending (PPP US$)",
      ylab="Life expectancy (years)")
text(health$spending, health$lifespan, health$country, col=color)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Plot  scatterplot, excluding some countries for improved clarity
removec <- health$country %in% c("Netherlands", "Belgium", "Germany",
  "Ireland", "Iceland", "Greece", "Italy", "Sweden", "UK")
#+ eval=FALSE, include=FALSE
png(root("HealthExpenditure/figs","healthscatter2.png"), height=600, width=700)
#+
par(mgp=c(2.5,.7,0), tck=-.01, mar=c(4,4,.1,.1))
plot(health$spending[!removec], health$lifespan[!removec], xlim=c(0,1.05*max(health$spending)),
      xaxs="i",  type="n", xlab="Health care spending (PPP US$)",
      ylab="Life expectancy (years)", cex.axis=1.3, cex.lab=1.3, las=1, xaxt="n", bty="l")
axis(1, seq(0,8000,2000), cex.axis=1.3, cex.lab=1.3)
text(health$spending[!removec], health$lifespan[!removec], health$country[!removec],
      col=color[!removec], cex=1.3)
for (x in seq(2000,6000,2000)) abline(v=x, col="gray", lwd=.5)
for (y in seq(74,82,2)) abline(y,0,col="gray", lwd=.5)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off ()

#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("HealthExpenditure/figs","healthscatter3.pdf"), height=4, width=5.5)
#+
par(mgp=c(1.7,.5, 0), tck=-.01, mar=c(3,3,.1,.1))
plot(health$spending[!removec], health$lifespan[!removec], xlim=c(0,1.05*max(health$spending)),
      xaxs="i",  type="n", xlab="Health care spending (PPP US$)",
      ylab="Life expectancy (years)", bty="l", xaxt="n")
axis(1, seq(0,6000,2000))
text(health$spending[!removec], health$lifespan[!removec], health$country[!removec], cex=.9)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### ggplot versions
#'
#' All countries:
# this could be further modified to include color in the dataframe
ggplot(data=health, aes(x=spending, y=lifespan, label=country)) +
  geom_text(color=color) +
  labs(x="Health care spending (PPP US$)",
       y="Life expectancy (years)")

#' Selected countries:
# this could be further modified to include color in the dataframe
ggplot(data=subset(health, !removec),                   
       aes(x=spending, y=lifespan, label=country)) +
  geom_text(color=color[!removec]) +
  labs(x="Health care spending (PPP US$)",
       y="Life expectancy (years)")
