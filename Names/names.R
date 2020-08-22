#' ---
#' title: "Regression and Other Stories: Names"
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

#' Names - Distributions of names of American babies. See Chapter 2 in
#' Regression and Other Stories.
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

#' #### Load data
allnames <- read.csv(root("Names/data","SSA-longtail-names.csv"))
girl <- as.vector(allnames$sex)=="F"
names <- as.vector(allnames$name)
columns <- colnames(allnames)
range <- (1:length(columns))[columns=="X1931"]:(1:length(columns))[columns=="X2000"]
years <- 1931:2000
colRenorm <- function(a){
  a / matrix(colSums(a), nrow=nrow(a), ncol=ncol(a), byrow=TRUE)
}
counts <- as.matrix(allnames[,range])
counts.norm <- colRenorm(counts)
totals <- rowMeans(counts.norm)
counts.adj <- ifelse (counts==0, 2, counts)
counts.adj.norm <- colRenorm(counts.adj)/colSums(counts.adj)

#' #### Compute stats
N <- nrow(allnames)
stats.labels <- c("avg.year", "avg.pop", "max.pop", "ratio", "year.of.max.pop",
  "volatility", "slope.1931.1965", "slope.1966.2000","slope.1931.2000",
  "slope.1981.2000", "pop.2000","avg.year.2")
stats <- array(NA, c(N,length(stats.labels)))
dimnames(stats) <- list(names, stats.labels)
for (i in 1:N){
  avg.year <- sum(years*counts.norm[i,])/sum(counts.norm[i,])
  avg.year.2 <- sum(years*as.numeric(counts[i,]))/sum(as.numeric(counts[i,]))
  avg.pop <- mean(counts.norm[i,])
  max.pop <- max(counts.norm[i,])
  ratio <- max(counts.adj.norm[i,])/ min(counts.adj.norm[i,])
  year.of.max.pop <- min(years[counts.norm[i,]==max.pop])
  logcounts <- log(counts.adj.norm[i,])
  volatility <- sd(logcounts)
  M1 <- lm(logcounts ~ I(years/10), subset=years<=1965)
  M2 <- lm(logcounts ~ I(years/10), subset=years>=1966)
  M3 <- lm(logcounts ~ I(years/10))
  M4 <- lm(logcounts ~ I(years/10), subset=years>=1981)
  slope.1931.1965 <- coef(M1)[2]
  slope.1966.2000 <- coef(M2)[2]
  slope.1931.2000 <- coef(M3)[2]
  slope.1981.2000 <- coef(M4)[2]
  pop.2000 <- counts.norm[i,years==2000]
  stats[i,] <- unlist(lapply(stats.labels, get))
}

#' Helper function
name.subset <- function(subset, n){
  if (n==0){
    n <- length(years)
    probs.remaining <- counts.norm
    sample.1.raw <- rep(NA, n)
    for (i in 1:n){
      sample.1.raw[i] <- sample(1:N, 1, prob=ifelse(subset,probs.remaining[,i],0))
      probs.remaining[sample.1.raw[i],] <- 0
    }
  }
  else {
    sample.1.raw <- sample(1:N, n, prob=ifelse(subset,totals,0))
  }
  year.of.max.pop.1 <- stats[sample.1.raw,"year.of.max.pop"]
  sample.1 <- sample.1.raw[order(year.of.max.pop.1)]
  a <- stats[sample.1, c("year.of.max.pop","avg.year.2","max.pop","ratio","slope.1931.2000","slope.1981.2000","pop.2000")]
  return(a)
}

#' Helper plot function
namesplot <- function(a){
  labels <- c("Peak year", "Avg year", "Peak\npopularity", "Ratio max/min\npopularity", "Avg trend,\n1931-2000", "Avg trend,\n1981-2000", "Popularity\nin 2000")
  digits <- rep(0, 5)
  is.log <- c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE)
  lo <- c(1930, 1930, -5, 0, -2, -2, -5)
  hi <- c(2000, 2000, -1, 4, 2, 2, -1)
  J <- ncol(a)
  n <- nrow(a)
  plot(c(-.6,J), c(-n,3), xlab="", ylab="", xaxt="n", yaxt="n", type="n", bty="n")
  for (i in seq(0,-n,-6)){
    polygon(c(-.6,J,J,-.6), rep(c(i-.5, max(i-3,-n)-.5), c(2,2)), col="gray90", border=NA)
  }
  text(-.1, -(1:n), dimnames(a)[[1]], adj=1, cex=.7)
  for (j in 1:J){
    if (is.log[j]){
      x <- log10(a[,j])
      text(c(j-.8,j-.5,j-.2), c(1,1,1), 10^seq(lo[j],hi[j],length=3), cex=.7)
    }
    else {
      x <- a[,j]
      text(c(j-.8,j-.5,j-.2), c(1,1,1), seq(lo[j],hi[j],length=3), cex=.7)
    }
    lines(c(j-.8,j-.2), c(0,0))
    segments(c(j-.8,j-.5,j-.2), c(0,0,0), c(j-.8,j-.5,j-.2), c(.2,.2,.2))
    lines(c(j-1,j-1), c(0,-n), col="gray")
    
    text(j-.5, 3, labels[j], cex=.7)
    points(j-.8 + .6*(x-lo[j])/(hi[j]-lo[j]), -(1:n), pch=20, cex=.6)
  }
}

#' #### Plot stats
girls50 <- name.subset(girl, 50)
boys50 <- name.subset(!girl, 50)
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Names/figs","girls50.pdf"), height=8, width=8)
#+
par(mar=c(1,2,1,1))
namesplot(girls50)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
if (savefigs) pdf(root("Names/figs","boys50.pdf"), height=8, width=8)
#+
par(mar=c(1,2,1,1))
namesplot(boys50)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
#+
girls70 <- name.subset(girl, 0)
boys70 <- name.subset(!girl, 0)
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Names/figs","girls70.pdf"), height=10, width=8)
#+
par(mar=c(1,2,1,1))
namesplot(girls70)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
if (savefigs) pdf(root("Names/figs","boys70.pdf"), height=10, width=8)
#+
par(mar=c(1,2,1,1))
namesplot(boys70)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Restrict to top 1000 and remove Laura's list of bad names
error.names <- as.matrix(read.csv(root("Names/data","ErrorNames.csv")))
error.names.girl <- error.names[,2]=="F"
bad.girl.names <- error.names[error.names.girl,1]
bad.boy.names <- error.names[!error.names.girl,1]
top1000 <- array(NA, c(N,length(years)))
for (i in 1:length(years)){
  top1000[girl,i] <- counts[girl,i] >= rev(sort(counts[girl,i]))[1000]
  top1000[!girl,i] <- counts[!girl,i] >= rev(sort(counts[!girl,i]))[1000]
}
evertop1000 <- rowSums(top1000) > 0
keep <- evertop1000
for (i in (1:N)[evertop1000]){
  if (girl[i] & names[i] %in% bad.girl.names) keep[i] <- FALSE
  if (!girl[i] & names[i] %in% bad.boy.names) keep[i] <- FALSE
}

girls50new <- name.subset(girl & keep, 50)
boys50new <- name.subset(!girl & keep, 50)
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Names/figs","girls50new.pdf"), height=8, width=8)
#+
par(mar=c(1,2,1,1))
namesplot(girls50new)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
if (savefigs) pdf(root("Names/figs","boys50new.pdf"), height=8, width=8)
#+
par(mar=c(1,2,1,1))
namesplot(boys50new)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' Add new column
avg.year.2 <- rep(NA, 50)
names50 <- row.names(girls50new)
for (i in 1:50){
  ok <- (1:N)[names==names50[i]&girl]
  avg.year.2[i] <- stats[ok,"avg.year.2"]
}
girls50new <- cbind(girls50new[,1], avg.year.2, girls50new[,2:6])
colnames(girls50new)[1] <- "year.of.max.pop"
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Names/figs","girls50new.pdf"), height=8, width=9)
#+
par(mar=c(1,2,1,1))
namesplot(girls50new)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
#'
avg.year.2 <- rep(NA, 50)
names50 <- row.names(boys50new)
for (i in 1:50){
  ok <- (1:N)[names==names50[i]&!girl]
  avg.year.2[i] <- stats[ok,"avg.year.2"]
}
boys50new <- cbind(boys50new[,1], avg.year.2, boys50new[,2:6])
colnames(boys50new)[1] <- "year.of.max.pop"
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Names/figs","boys50new.pdf"), height=8, width=9)
#+
par(mar=c(1,2,1,1))
namesplot(boys50new)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
