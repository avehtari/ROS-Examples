## possible function to estimate treatment effects while properlly
## adjusting for the weights

wls.all2 <- function(X, w = wts, Y = y, treat = Trt)
{
  #
  # This produces coefficient estimates and both standard and robust variances 
  # estimates for regression with weights
  # the standard variance corresponds to a situation where an observation represents
  # the mean of w observations
  # the robust variance corresponds to a situation where weights represent 
  # probability or sampling weights
  #
  # first put together the necessary data inputs
  #
  nunits <-  sum(w > 0)
  k <-  ncol(X)
  ## now the weights, properly normed
  wn <-  w * (nunits/sum(w))
  W <-  diag(wn * (nunits/sum(wn)))
  #
  # x prime x inverse (including weights)
  vhat <-   - sweep.inv((t(X) %*% W %*% X))
  #
  # estimated regression coefficients and variance for just the treatment coefficient
  b <-  vhat %*% t(X) %*% W %*% Y
  MSE <-  c(t(Y) %*% W %*% Y - t(b) %*% t(X) %*% W %*% Y)/(nunits - k)
  var.std <-  (vhat * MSE)[2, 2]
  #
  ######  now for the robust variance calculations
  # now a matrix where each row represents the contribution to the score
  # for each observation
  U <-  c((Y - X %*% b) * wn) * X
  # finite sample adjustment
  qc <-  nunits/(nunits - 2)
  # the sum of outer products of each of the above score contributions for
  # each person is calculated here
  prodU <-  array(0, c(k, k, nunits))
  for(i in 1:nunits) {
    prodU[,  , i] <-  outer(U[i,  ], U[i,  ])
  }
  # putting it all together...
  Vrob <-  qc * vhat %*% apply(prodU, c(1, 2), sum) %*% vhat
  # and we pull off the variance just for the treatment effect 
  var.rob <-  Vrob[2, 2]
  ###############
  results <-  c(var.std, var.rob, b[2])
  results
}

sweep.inv <- function(G){
  # sweeps a symmetric matrix on all positions
  # (so inverts the matrix)
  for(i in 1:nrow(G)) {
    G <- sweep.oper(G, i)
  }
  G
}

sweep.oper <- function(G = theta, k = 1.){
  # k is the sweep position
  p <- dim(G)[1.]
  H <- G
  #first do generic elements (those that don't involve k)
  H[] <- 0.
  tmp <- matrix(G[, k], p, 1.) %*% matrix(G[, k], 1., p)
  #now replace the row and col with index=k 
  H <- G - tmp/G[k, k]
  H[, k] <- G[, k]/G[k, k]
  #now replace the (k,k) diagonal element 
  H[k,  ] <- G[, k]/G[k, k]
  # and we're done
  H[k, k] <- -1./G[k, k]
  H
}

inv.logit <- function(x){
  exp(x)/(1+exp(x))
}

# got this from a website to work with lmtest:coeftest
# but it doesn't seem to work
robust_se <- function(model){
    # get parameters
     x <- model.matrix(model)
     n <- nrow(x)
     k <- length(coef(model))
    # See pg 6 of Yannick - note that length k is num_pred + indicator
     dfc <- n / (n - k)
    # make sandwich
     u <- model$residuals
     bread <- solve(crossprod(x))
     meat <- t(x) %*% (dfc * diag(u^2)) %*% x
     v <- bread %*% meat %*% bread
  return(v)
}

invlogit
