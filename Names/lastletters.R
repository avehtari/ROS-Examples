#' ---
#' title: "Regression and Other Stories: Last letters of names"
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

#' Last letters of names - Distributions of last letters of names of
#' American babies. See Chapter 2 in Regression and Other Stories.
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
counts.adj <- ifelse(counts==0, 2, counts)
counts.adj.norm <- colRenorm(counts.adj)/colSums(counts.adj)
#'

#' #### Remove Laura's list of bad names
error.names <- as.matrix(read.csv(root("Names/data","ErrorNames.csv")))
error.names.girl <- error.names[,2]=="F"
bad.girl.names <- error.names[error.names.girl,1]
bad.boy.names <- error.names[!error.names.girl,1]
N <- nrow(allnames)
keep <- rep(TRUE, N)
for (i in (1:N)) {
  if (girl[i] & names[i] %in% bad.girl.names) keep[i] <- FALSE
  if (!girl[i] & names[i] %in% bad.boy.names) keep[i] <- FALSE
}

allnames <- allnames[keep,]
names <- names[keep]
girl <- girl[keep]

namelength <- nchar(names)
lastletter <- substr(names, namelength, namelength)
firstletter <- substr(names, 1, 1)

discrete.histogram <- function (x, prob, prob2=NULL,
    xlab="x", ylab="Probability", xaxs.label=NULL, yaxs.label=NULL, bar.width=NULL, ...){
  if (length(x) != length(prob)) stop()
  x.values <- sort (unique(x))
  n.x.values <- length (x.values)
  gaps <- x.values[2:n.x.values] - x.values[1:(n.x.values-1)]
  if (is.null(bar.width)) bar.width <- min(gaps)*.2
  par(mar=c(3,3,2,2), mgp=c(1.7,.3,0), tck=0)
  plot(range(x)+c(-1,1)*bar.width, c(0,max(prob)),
    xlab=xlab, ylab=ylab, xaxs="i", xaxt="n",  yaxs="i",
    yaxt=ifelse(is.null(yaxs.label),"s","n"), bty="l", type="n", ...)
  if (is.null(xaxs.label)){
    axis(1, x.values)
  }
  else {
    n <- length(xaxs.label[[1]])
    even <- (1:n)[(1:n)%%2==0]
    odd <- (1:n)[(1:n)%%2==1]
    axis(1, xaxs.label[[1]][even], xaxs.label[[2]][even], cex.axis=.9)
    axis(1, xaxs.label[[1]][odd], xaxs.label[[2]][odd], cex.axis=.9)
  }
  if (!is.null(yaxs.label)){
    axis(2, yaxs.label[[1]], yaxs.label[[2]], tck=-.02)
  }
  for (i in 1:length(x)){
    polygon(x[i] + c(-1,-1,1,1)*bar.width/2, c(0,prob[i],prob[i],0),
      border="gray10", col="gray10")
    if (!is.null(prob2))
      polygon(x[i] + c(-1,-1,1,1)*bar.width/10, c(0,prob2[i],prob2[i],0),
        border="red", col="black")
  }
}

for (year in c(1900,1950,2010)){
  thisyear <- allnames[,paste("X",year,sep="")]
  lastletter.by.sex <- array(NA, c(26,2))
  firstletter.by.sex <- array(NA, c(26,2))
  for (i in 1:26){
    lastletter.by.sex[i,1] <- sum(thisyear[lastletter==letters[i] & girl])
    lastletter.by.sex[i,2] <- sum(thisyear[lastletter==letters[i] & !girl])
    firstletter.by.sex[i,1] <- sum(thisyear[firstletter==LETTERS[i] & girl])
    firstletter.by.sex[i,2] <- sum(thisyear[firstletter==LETTERS[i] & !girl])
  }
  if (savefigs) pdf(root("Names/figs", paste("boys", year, ".pdf", sep="")),
                   height=3, width=4.5)
  discrete.histogram(1:26, 100*(lastletter.by.sex[,2])/sum(lastletter.by.sex[,2]), xaxs.label=list(1:26,letters), yaxs.label=list(seq(0,30,10),seq(0,30,10)), xlab="", ylab="Percentage of boys born", main=paste("Last letter of boys' names in", year), cex.axis=.9, cex.main=.9, bar.width=.8)
  for (y in c(10,20,30)) abline (y,0,col="gray",lwd=.5)
  if (savefigs) dev.off()
  if (savefigs) pdf(root("Names/figs", paste("girls", year, ".pdf", sep="")),
                   height=3, width=4.5)
  discrete.histogram(1:26, 100*(lastletter.by.sex[,1])/sum(lastletter.by.sex[,1]), xaxs.label=list(1:26,letters), yaxs.label=list(seq(0,30,10),seq(0,30,10)), xlab="", ylab="Percentage of girls born", main=paste("Last letter of girls' names in", year), cex.main=.9)
  if (savefigs) dev.off()
}

yrs <- 1880:2010
n.yrs <- length(yrs)
lastletterfreqs <- array(NA, c(n.yrs,26,2))
firstletterfreqs <- array(NA, c(n.yrs,26,2))
dimnames(lastletterfreqs) <- list(yrs, letters, c("girls","boys"))
dimnames(firstletterfreqs) <- list(yrs, letters, c("girls","boys"))
for (i in 1:n.yrs){
  thisyear <- allnames[,paste("X",yrs[i],sep="")]
  for (j in 1:26){
    lastletterfreqs[i,j,1] <- sum(thisyear[lastletter==letters[j] & girl])
    lastletterfreqs[i,j,2] <- sum(thisyear[lastletter==letters[j] & !girl])
    firstletterfreqs[i,j,1] <- sum(thisyear[firstletter==LETTERS[j] & girl])
    firstletterfreqs[i,j,2] <- sum(thisyear[firstletter==LETTERS[j] & !girl])
  }
  for (k in 1:2){
    lastletterfreqs[i,,k] <- lastletterfreqs[i,,k]/sum(lastletterfreqs[i,,k])
    firstletterfreqs[i,,k] <- firstletterfreqs[i,,k]/sum(firstletterfreqs[i,,k])
  }
}

#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Names/figs", "namestimeboys.pdf"), height=3.5, width=6)
#+
par(mar=c(2,2,1,1), mgp=c(1.7,.3,0), tck=-.01, oma=c(0,0,2,0), mfrow=c(2,3))
popular <- rev(order(lastletterfreqs[1,,2]))[1:6]
for (k in 1:length(popular)){
  plot(range(yrs), c(0,50), type="n", xlab="", ylab="", bty="l", xaxt="n", yaxt="n", yaxs="i", xaxs="i")
  axis(1, seq(1900,2000,50))
  axis(2, seq(0,40,20), paste(seq(0,40,20), "%", sep=""))
  mtext(paste(". . .", LETTERS[popular[k]]), side=3, line=-1, cex=.8)
  for (j in 1:26){
    maxfreq <- max(lastletterfreqs[,j,2])
    best <- (1:n.yrs)[lastletterfreqs[,j,2]==maxfreq]
    lines(yrs, 100*lastletterfreqs[,j,2], col=if (j==popular[k]) "black" else "darkgray", lwd=if (j==popular[k]) 1 else .5)
  }
}
mtext("Last letters of boys' names", side=3, outer=TRUE, line=.5)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
#+

#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Names/figs", "namestimeboys2.pdf"), height=4, width=6)
#+
par(mar=c(2,3,2,1), mgp=c(1.7,.3,0), tck=-.01)
popular <- c(14,25,4)
width <- rep(.5,26)
type <- rep(1,26)
width[popular] <- c(2,3,3)
type[popular] <- c(1,3,2)
plot(range(yrs), c(0,41), type="n", xlab="", ylab="Percentage of all boys' names that year", bty="l", xaxt="n", yaxt="n", yaxs="i", xaxs="i")
  axis(1, seq(1900,2000,50))
  axis(2, seq(0,40,20), paste(seq(0,40,20), "%", sep=""))
  for (j in 1:26){
    maxfreq <- max(lastletterfreqs[,j,2])
    best <- (1:n.yrs)[lastletterfreqs[,j,2]==maxfreq]
    lines(yrs, 100*lastletterfreqs[,j,2], col="black", lwd=width[j], lty=type[j])
  }
text(2000, 35, "N")
text(1935, 20, "D")
text(1975, 15, "Y")
mtext("Last letters of boys' names", side=3, line=.5)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
#+

#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Names/figs", "namestimeboys3.pdf"), height=4, width=6)
#+
par(mar=c(2,3,2,1), mgp=c(1.7,.3,0), tck=-.01)
popular <- c(14,25,4)
plot(range(yrs), c(0,41), type="n", xlab="", ylab="Percentage", bty="l", xaxt="n", yaxt="n", yaxs="i", xaxs="i")
  axis(1, seq(1900,2000,50))
  axis(2, seq(0,40,20), paste(seq(0,40,20), "%", sep=""))
  for (j in 1:26){
    maxfreq <- max(firstletterfreqs[,j,2])
    best <- (1:n.yrs)[firstletterfreqs[,j,2]==maxfreq]
    if (j %in% popular){
      lines(yrs, 100*firstletterfreqs[,j,2], col="black", lwd=2)
    }
    else{
      lines(yrs, 100*firstletterfreqs[,j,2], col="black", lwd=.5)
    }
  }
mtext("First letters of boys' names", side=3, line=.5)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
#+

#' Stuff for NYT column
dim(lastletterfreqs[,,2])
round(lastletterfreqs[yrs>2005,,2], 2)  # 35% end in n
round(lastletterfreqs[yrs>2005,,1], 2)  # 38% of all girls end in a

#' 1950
round(lastletterfreqs[yrs==1950,,2], 2)  # 14% end in n (tied with d, s, and y as most popular)
round(lastletterfreqs[yrs==1950,,1], 2)  # 34% of all girls end in a

#' 1900
round(lastletterfreqs[yrs==1900,,2], 2)  # 14% end in n (tied with d, s, and y as most popular)
round(lastletterfreqs[yrs==1900,,1], 2)  # 34% of all girls end in a

#' Most popular names in any given year
boy.names <- names[!girl]
girl.names <- names[girl]
for (year in c(1900,1950,2010)){
  thisyear <- allnames[,paste("X",year,sep="")]
  boy.totals <- thisyear[!girl]
  boy.proportions <- boy.totals/sum(boy.totals)
  index <- rev(order(boy.proportions))
  popular.names <- boy.names[index]
  popularity <- boy.proportions[index]
  print(year)
  print(popular.names[1:30])
  round(popularity[1:30],3)
  print(c(sum(popularity[1:10]), sum(popularity[1:20]), sum(popularity[1:30]), sum(popularity[1:50]), sum(popularity[1:100])))
}

n_percentage <- 100*lastletterfreqs[,14,2]
topten_percentage <- array(NA, c(length(yrs), 2))
for (i in 1:length(yrs)){
  thisyear <- allnames[,paste("X",yrs[i],sep="")]
  boy.totals <- thisyear[!girl]
  boy.proportions <- boy.totals/sum(boy.totals)
  index <- rev(order(boy.proportions))
  popular.names <- boy.names[index]
  popularity <- boy.proportions[index]
  topten_percentage[i,2] <- 100*sum(popularity[1:10])
  girl.totals <- thisyear[girl]
  girl.proportions <- girl.totals/sum(girl.totals)
  index <- rev(order(girl.proportions))
  popular.names <- girl.names[index]
  popularity <- girl.proportions[index]
  topten_percentage[i,1] <- 100*sum(popularity[1:10])
}

#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Names/figs", "n.pdf"), height=3, width=4)
#+
par(mar=c(4,2,1,0), mgp=c(1.3,.2,0), tck=-.02)
plot(yrs, n_percentage, type="l", xaxt="n", yaxt="n", xaxs="i", yaxs="i", ylim=c(0,45), bty="l", xlab="Year", ylab="", cex.lab=.8)
axis(1, c(1900,1950,2000), cex.axis=.8)
axis(2, c(0,20,40), c("0%","20%","40%"), cex.axis=.8)
mtext("Percentage of new boys' names each year ending in 'n'", cex=.8)
mtext("Source:  Social Security Administration, courtesy of Laura Wattenberg", 1, 2.5, cex=.5, adj=0)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
#+

#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Names/figs", "topten.pdf"), height=3, width=4)
#+
par(mar=c(4,2,1,0), mgp=c(1.3,.2,0), tck=-.02)
plot(yrs, topten_percentage[,2], type="l", xaxt="n", yaxt="n", xaxs="i", yaxs="i", ylim=c(0,45), bty="l", xlab="Year", ylab="", cex.lab=.8)
lines(yrs, topten_percentage[,1])
axis(1, c(1900,1950,2000), cex.axis=.8)
axis(2, c(0,20,40), c("0%","20%","40%"), cex.axis=.8)
text(1902, 35, "Boys", cex=.75, adj=0)
text(1911, 20, "Girls", cex=.75, adj=0)
mtext("Total popularity of top ten names each year, by sex", cex=.8)
mtext("Source:  Social Security Administration, courtesy of Laura Wattenberg", 1, 2.5, cex=.5, adj=0)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off ()
#+
