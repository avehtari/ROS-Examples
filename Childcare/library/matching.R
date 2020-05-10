## 2019 version of matching function

matching <- function(z, score, replace=FALSE){
  # argument z is the vector of indicators for treatment or control   #
  # argument score is the vector of the propensity scores in the      #
  # same order as z                                                   #
  # THIS FUNCTION REQUIRES THE INFERENTIAL GROUP TO SATISFY Z=1       #
  # Group satisfying Z=1 will remain intact and matches for them will #
  # be found from among those satisfying Z=0                          #
  #                                                                   #
  # the function (potentially) returns several things                 #
  # 1) match.ind: a vector of indices that the corresponding unit is  #
  #    matched to. The length is equal to the number of unique IDs    #
  # 2) cnts:  shows the number of times each unit will be used in any #
  #    subsequent analyses (1 for each treated unit and number of     #
  #    times used as a match for each control unit (equivalently the  # 
  #    number of treated units it is matched to)                      #         #          #
  # 3a) pairs:  indicator for each pair [only available for            #
  #    replace=TRUE]
  # OR
  # 3b) matches:  a matrix capturing which treated observations
  #     were matched to which controls [only for replace=FALSE]
  #                                                                   #
  # Ties are broken through random sampling so set seed if you want   #
  # to replicate results                                              #
  #####################################################################
  n <- length(score)
  nt <- sum(z)
  nc <- sum(1-z)
  ind.t <- c(1:n)[z==1]
  ind.c <- c(1:n)[z==0]
  cnts <- rep(0, n)
  cnts[z==1] = rep(1,nt)
  scorec <- score[z == 0]
  scoret <- score[z == 1]
  # matching with replacement
  if (replace){
    # calculate distances between all pairs of units
    dist = abs(outer(scoret,scorec,FUN="-"))
    # find the identify the controls with the minimum distance from
    # each treated -- if there are ties, randomly pick one
    mins = apply(dist,1,min)
    # create a matrix with 1's for control columns matching the minimum
    # distance for the corresponding treatment rows
    matches = dist - mins
    matches[matches!=0] = 1
    matches = 1 - matches
    # if more than one control observation is chosen as a match for a given
    # treated we randomly chose which column to retain
    if(sum(matches)>nt){
      # figure out which rows and then replace the multiple 1's with one
      # randomly chosen one
      for(i in c(1:nt)[apply(matches,1,sum)>1]){
        matches_i <-  c(1:nc)[matches[i,]==1]
        nmi <- length(matches_i)
        matches[i,matches_i] <- sample(c(1,rep(0,nmi-1)),nmi,replace=FALSE) 
      }
    }
    # now fill in matched and ind.mt and pairs and counts
    ind.cm <- matches %*% ind.c
    # now record counts
    cnts[z==0] <- apply(matches,2,sum)
    # match indicators -- shouldn't be used for analysis
    match.ind <- c(ind.t, ind.cm)
    out <- list(match.ind = match.ind, cnts = cnts, matches = matches)
  }
  #  matching *without* replacement  
  if (!replace){
    pairs = rep(NA,n)
    match.ind <- rep(0, n)
    tally <- 0
    for (i in ind.t) {
      ## DEAL WITH TIES IN A MORE PRINCIPLED WAY? -- can do by adding a second
      # argument to break ties that is random
      available <- (1:n)[(z == 0) & (match.ind == 0)]
      j <- available[order(abs(score[available] - score[i]))[1]]
      cnts[j] <- 1
      match.ind[i] <- j
      match.ind[j] <- i
      tally <- tally + 1
      pairs[c(i, j)] <- tally
    }
    #match.ind <- match.ind[match.ind!=0]
    out <- list(match.ind = match.ind, cnts = cnts, pairs = pairs)
  }
  return(out)
}


#pscores.fun <- function(treat=Z, outs=Y, covs=X){
#  #
#  N <- nrow(covs)
#  nouts <- 1 
#  ncovs <- ncol(covs)
#  #
#  # first set up places to store results
#  res <- matrix(0,nouts,2)
#  bal <- matrix(0,ncovs,2)
#  #
#  # estimate p-scores
#  dat <- cbind.data.frame(treat=treat,covs)
#  mod <- glm(dat,family=binomial(link="logit"))
#  qx <- predict(mod, type="response")#mod$linear 
#  #
#  ### Now Matching With Replacement
#  matchout <- matching(z=treat, score=qx, replace=TRUE)
#  #
#  ### and treatment effect estimation with robust s.e.'s
#  wts <- rep(1, N)
#  wts[treat == 0] <- matchout$cnts
#  res <- .wls.all2(cbind(rep(1, sum(wts > 0)), treat[wts > 0],covs[wts > 0,  ]), wts[wts > 0], outs[wts > 0], treat[wts > 0])
#  c(res[3],sqrt(res[2]))
#}


