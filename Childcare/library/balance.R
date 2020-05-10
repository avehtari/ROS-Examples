# balance function after 2019

balance <- function (rawdata, treat, matched, estimand="ATT")
                     #factor = TRUE) 
{
  # rawdata: the full covariate dataset 
  # treat: the vector of treatment assignments for the full dataset
  # matched: vector of weights to apply to the full dataset to create the 
  # restructured data:  
  # --for matching without replacement these will all be 0's and 1's
  # --for one-to-one matching with replacement these will all be non-negative 
  #   integers
  # --for IPTW or more complicated matching methods these could be any 
  #   non-negative numbers
  # estimand: can either be ATT, ATC, or ATE
  require("Hmisc")
  if(missing(rawdata)) stop("rawdata is required")
    if(missing(matched)) stop("argument matched is required")
      if(missing(treat)) stop("treatment vector (treat) is required")
        cat("Balance diagnostics assume that the estimand is the",estimand,"\n")
  #
  #raw.dat <- data.frame(rawdata, treat = treat)
    covnames <- colnames(rawdata)
    if (is.null(covnames)){
        cat("No covariate names provided.  Generic names will be generated.")
        covnames = paste("v",c(1:ncol(rawdata)),sep="")
    }
    K <- length(covnames) 
    diff.means <- matrix(NA, K, 5)
    var.t <- numeric(K)
    var.c <- numeric(K)
    std.denom <- numeric(K)
    binary <- rep(1,K)
  #
  # First we calculate balance on the RAW DATA
  # Columns are (1) treat mean, (2) control mean, (3) diff in means, (4) abs std diff,
  # (5) ratio of sds
    for (i in 1:K) {
    # separate means by group
        diff.means[i, 1] <- mean(rawdata[treat==1, i]) 
        diff.means[i, 2] <- mean(rawdata[treat==0, i])
    # separate variances by group == only used as input to calculations below
        var.t[i] <- var(rawdata[(treat == 1), i])
        var.c[i] <- var(rawdata[(treat == 0), i])
    # denominator in standardized difference calculations
        if(estimand=="ATE"){std.denom[i] <- sqrt((var.t[i]+var.c[i])/2)}
        else{
          std.denom[i] <- ifelse(estimand=="ATT",sqrt(var.t[i]),sqrt(var.c[i]))
      }
    # difference in means
      diff.means[i, 3] <- diff.means[i, 1] - diff.means[i, 2]
    # standardized difference in means (sign intact)
      diff.means[i, 4] <- abs(diff.means[i, 3]/std.denom[i])
      if(length(unique(rawdata[,covnames[i]]))>2){
          binary[i] = 0
      }
  }
    #ifelse(estimand="ATT",sqrt(var.c[i]/var.t[i]),sqrt(var.t[i]/var.c[i]))
#  dimnames(diff.means) <- list(covnames[-(K + 1)], c("treat", "control", "unstd.diff", 
#                                                     "abs.std.diff", "ratio"))
#  diff.means[is.na(diff.means)] = "--"  #maybe only worry about in print function
  dimnames(diff.means) <- list(covnames, c("treat", "control", "unstd.diff", 
     "abs.std.diff", "ratio"))
  # Now we calculate balance on the restructured data
  diff.means.matched = matrix(NA, K, 5)
  #
  for (i in 1:K) {
    wts0 <- matched[treat==0]
    # separate means by group
    diff.means.matched[i, 1] <- mean(rawdata[treat == 1, i])
    diff.means.matched[i, 2] <- weighted.mean(rawdata[treat==0, i],w=wts0)
    # separate variances by group == only used as input to calculations below
    # these overwrite the variance above
    var.t[i] <- var(rawdata[treat == 1, i])
    var.c[i] <- wtd.var(rawdata[treat==0,i],weights=wts0)
    # difference in means
    diff.means.matched[i, 3] <- diff.means.matched[i, 1] - diff.means.matched[i, 2]
    # absolute standardized difference in means (denominator is stolen from
    # calculations on raw data above)
    diff.means.matched[i, 4] <- abs(diff.means.matched[i, 3])/std.denom[i]
    if(length(unique(rawdata[,covnames[i]]))>2){
      # just for binary
      # ratio of sds (treat over control:  should we change to comparison over inferential)
      diff.means.matched[i, 5] <- sqrt(var.c[i]/var.t[i])
  }
}
  #dimnames(diff.means.matched) <- list(covnames[-(K + 1)], c("treat", "control", "unstd.diff", 
  #                                                            "abs.std.diff", "ratio"))
dimnames(diff.means.matched) <- list(covnames, c("treat", "control", "unstd.diff", 
 "abs.std.diff", "ratio"))
  #
out <- list(diff.means.raw = diff.means, diff.means.matched = diff.means.matched, 
    covnames = covnames, binary = binary)
class(out) <- "balance"
return(out)
}


print.balance <- function(x, ..., combined=FALSE, digits= 2)
{
   if(combined==FALSE){
      cat("Balance Statistics for Unmatched Data\n")
      cat("--\n")
      print(round(x$diff.means.raw, digits=digits))
      cat("--\n")
      cat("\n")
      cat("Balance Statistics for Matched Data\n")
      cat("--\n")
      print(round(x$diff.means.matched, digits=digits), na.print="--")
      cat("--\n")
      cat("\n")
  }
  else{
   cat("Balance Statistics\n")
   cat("--\n") 
   print(round(cbind(x$diff.means.raw,x$diff.matched.raw)[,c(4,9,5,10)], 
     digits=digits), na.print="--")
}
}

### NEXT NEED TO FIGURE OUT HOW TO REVERSE THE ORDER OF THE COVARIATES

plot.balance <- function(x, longcovnames=NULL, which.covs="mixed",
    v.axis=TRUE, cex.main=1, cex.vars=1, cex.pts=1,
    mar=c(4, 3, 5.1, 2), plot=TRUE, x.max = NULL,...)
{
    # if which.covs = mixed then it plots all as std diffs
    # if which.covs = binary it only plots binary and as abs unstd diffs
    # if which.covs = cont it only plots non-binary and as abs std diffs
    #  
 
    covnames <- x$covnames
    if(!is.null(x.max)){
        x.range = c(0,x.max)
    }
    # if(which.covs=="binary") {
    #   cat("condition satisfied \n")
    # }

    # if plotting all, then use the standardized diff for all
    if(which.covs == "mixed"){
        pts <-  x$diff.means.raw[,4]                    # before matched.dat
        pts2 <- x$diff.means.matched[,4]                  # after matched
        K <- length(pts)
        idx <- 1:K
        main="Absolute Standardized Difference in Means"
    }
    #if plotting just binary use the unstandardized difference
    # for the plot make it the absolute value of
    if(which.covs == "binary"){
        pts <-  abs(x$diff.means.raw[x$binary==TRUE,3])      # before matched.dat
        pts2 <- abs(x$diff.means.matched[x$binary==TRUE,3])  # after matched
        K <- length(pts)
        idx <- 1:K
        main="Absolute Difference in Means"
        covnames = covnames[x$binary==TRUE]
    }
    #if plotting just continuous use the standardized difference
    if(which.covs == "cont"){
        pts <-  x$diff.means.raw[x$binary==FALSE,4]      # before matched
        pts2 <- x$diff.means.matched[x$binary==FALSE,4]  # after matched
        K <- length(pts)
        idx <- 1:K
        main="Absolute Standardized Difference in Means"
        covnames = covnames[x$binary==FALSE]
    }
    cat(pts,"\n")
    # tune the graphic console
    #par (mar=mar, mgp=mgp, oma=oma, tcl=tcl)

    par(mar = mar)
    if (is.null(longcovnames)) {
        longcovnames <- covnames
        maxchar <- max(sapply(longcovnames, nchar))
    }
    else {
        maxchar <- max(sapply(longcovnames, nchar))
    }
    min.mar <- par("mar")
    mar[2] <- max(min.mar[2], trunc(mar[2] + maxchar/10)) + mar[2] + 0.5
    par(mar = mar)

    ## now reverse the order of everything so the plot proceeds from
    ## to top to bottom with respect to original ordering of variables
    pts = rev(pts)
    pts2 = rev(pts2)
    longcovnames = rev(longcovnames)
    
    if(plot){
        # plot the estimates
        if(is.null(x.max)){
            plot(c(pts,pts2), c(idx,idx),
                xlim=c(0, max(c(pts,pts2))),
                bty="n", xlab="", ylab="",
                xaxt="n", yaxt="n", type="n",
                main=main, cex.main=cex.main)
        }
        if(!is.null(x.max)){
            plot(c(pts,pts2), c(idx,idx),
                bty="n", xlab="", ylab="",
                xaxt="n", yaxt="n", type="n",
                xlim=x.range,
                main=main, cex.main=cex.main)
        }
        abline(v=0, lty=2)
        points(pts, idx, cex=cex.pts)          # before matched
        points(pts2, idx, pch=19, cex=cex.pts) # after matched
        if (v.axis){
            axis(3)
        }
        if (is.null(longcovnames)){
            axis(2, at=1:K, labels=covnames[1:K],
                las=2, hadj=1, lty=0, cex.axis=cex.vars)
        }
        else{
            axis(2, at=1:K, labels=longcovnames[1:K],
                las=2, hadj=1, lty=0, cex.axis=cex.vars)
        }
    }
    else{
        plot(c(pts,pts2), c(idx,idx),
            bty="n", xlab="", ylab="",
            xaxt="n", yaxt="n", #xaxs="i",
            #yaxs="i",
            type="n", axes=FALSE,
            #ylim=c(max(idx)+.25, min(idx)-.25),
            #xlim=x.range,
            main="", cex.main=cex.main,...)
    }
    return(list("raw"=pts, "matched"=pts2))
}
