#' ---
#' title: "Regression and Other Stories: National election study"
#' author: "Andrew Gelman, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Fitting the same regression to many datasets
#' 
#' -------------
#' 

#' **Load libraries**
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library("rprojroot")
root<-has_dirname("RAOS-Examples")$make_fix_file()
library("foreign")

#' **Load data**
data <- read.table(root("NES/data","nes.dat"))

#' **Partyid model to illustrate repeated model use (secret weapon)**
regress_year <- function (yr) {
  this_year <- data[data$year==yr,]
  lm_0 <- lm(partyid7 ~ real_ideo + race_adj + factor(age_discrete) + educ1 + female + income, data=this_year)
  coefs <- summary(lm_0)$coef[,1:2]
}
summary <- array (NA, c(9,2,8))
for (yr in seq(1972,2000,4)){
  i <- (yr-1968)/4
  summary[,,i] <- regress_year(yr)
}
yrs <- seq(1972,2000,4)

#' **Plot**
#+ eval=FALSE, include=FALSE
pdf(root("NES/figs","partyid_1.pdf"), height=2.5, width=7.5)
#+
coef_names <- c("Intercept", "Ideology", "Black", "Age_30_44", "Age_45_64", "Age_65_up", "Education", "Female", "Income")
par(mfrow=c(2,5), mar=c(2,3,2,2), tck=-.02, mgp=c(2,.7,0))
for (k in 1:9){
  plot(range(yrs), range(0,summary[k,1,]+.67*summary[k,2,],summary[k,1,]-.67*summary[k,2,]), type="n", xlab="", ylab="Coefficient", main=coef_names[k], mgp=c(1.2,.2,0), cex.main=1, cex.axis=1, cex.lab=1, tcl=-.1, bty="l", xaxt="n")
  axis(1, c(1972,1986,2000), mgp=c(.5,.3,0))
  abline(0,0, lty=2)
  points(yrs, summary[k,1,], pch=20)
  segments(yrs, summary[k,1,]-.67*summary[k,2,], yrs, summary[k,1,]+.67*summary[k,2,])
}
#+ eval=FALSE, include=FALSE
dev.off()
