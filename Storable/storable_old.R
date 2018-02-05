bayespolr <- 
function (formula, data, weights, start, ..., subset, na.action, 
    contrasts = NULL, Hess = TRUE, model = TRUE, method = c("logistic", 
        "probit", "cloglog", "cauchit"), prior.mean = 0, prior.scale = 2.5, 
    prior.df = 1, scaled = TRUE, prior.mean.for.cutpoints = 0, 
    prior.scale.for.cutpoints = 10, prior.df.for.cutpoints = 1, n.iter = 100) 
{
    logit <- function(p) log(p/(1 - p))
    dt.deriv <- function(x, mean, scale, df, log = TRUE, delta = 0.001) {
        (dt((x + delta - mean)/scale, df, log = log) - dt((x - 
            delta - mean)/scale, df, log = log))/(2 * delta)
    }
    fmin <- function(beta) {
        theta <- beta[pc + 1:q]
        gamm <- c(-100, cumsum(c(theta[1], exp(theta[-1]))), 
            100)
        eta <- offset
        if (pc > 0) 
            eta <- eta + drop(x %*% beta[1:pc])
        pr <- pfun(gamm[y + 1] - eta) - pfun(gamm[y] - eta)
        if (all(pr > 0)) 
            f <- -sum(wt * log(pr))
        else f <- Inf
        zeta <- cumsum(c(theta[1], exp(theta[-1])))
        zeta.centered <- zeta - sum(beta[1:pc] * colMeans(x))
        f <- f - sum(dt((zeta.centered - prior.mean.for.cutpoints)/prior.scale.for.cutpoints, 
            prior.df.for.cutpoints, log = TRUE))
        if (pc > 0) 
            f <- f - sum(dt((beta[1:pc] - prior.mean)/prior.scale, 
                prior.df, log = TRUE))
        return(f)
    }
    gmin <- function(beta) {
        jacobian <- function(theta) {
            k <- length(theta)
            etheta <- exp(theta)
            mat <- matrix(0, k, k)
            mat[, 1] <- rep(1, k)
            for (i in 2:k) mat[i:k, i] <- etheta[i]
            mat
        }
        theta <- beta[pc + 1:q]
        gamm <- c(-100, cumsum(c(theta[1], exp(theta[-1]))), 
            100)
        eta <- offset
        if (pc > 0) 
            eta <- eta + drop(x %*% beta[1:pc])
        pr <- pfun(gamm[y + 1] - eta) - pfun(gamm[y] - eta)
        p1 <- dfun(gamm[y + 1] - eta)
        p2 <- dfun(gamm[y] - eta)
        g1 <- if (pc > 0) 
            t(x) %*% (wt * (p1 - p2)/pr)
        else numeric(0)
        xx <- .polrY1 * p1 - .polrY2 * p2
        g2 <- -t(xx) %*% (wt/pr)
        g2 <- t(g2) %*% jacobian(theta)
        zeta <- cumsum(c(theta[1], exp(theta[-1])))
        zeta.centered <- zeta - sum(beta[1:pc] * colMeans(x))
        g2 <- g2 - dt.deriv(zeta.centered, prior.mean.for.cutpoints, 
            prior.scale.for.cutpoints, prior.df.for.cutpoints, 
            log = TRUE)
        if (pc > 0) 
            g1 <- g1 - dt.deriv(beta[1:pc], prior.mean, prior.scale, 
                prior.df, log = TRUE)
        if (all(pr) > 0) 
            c(g1, g2)
        else rep(NA, pc + q)
    }
    m <- match.call(expand.dots = FALSE)
    method <- match.arg(method)
    pfun <- switch(method, logistic = plogis, probit = pnorm, 
        cloglog = pgumbel, cauchit = pcauchy)
    dfun <- switch(method, logistic = dlogis, probit = dnorm, 
        cloglog = dgumbel, cauchit = dcauchy)
    if (is.matrix(eval.parent(m$data))) 
        m$data <- as.data.frame(data)
    m$start <- m$Hess <- m$method <- m$... <- NULL
    m[[1]] <- as.name("model.frame")
    m <- eval.parent(m)
    Terms <- attr(m, "terms")
    x <- model.matrix(Terms, m, contrasts)
    xint <- match("(Intercept)", colnames(x), nomatch = 0)
    n <- nrow(x)
    pc <- ncol(x)
    cons <- attr(x, "contrasts")
    if (xint > 0) {
        x <- x[, -xint, drop = FALSE]
        pc <- pc - 1
    }
    else warning("an intercept is needed and assumed")
    wt <- model.weights(m)
    if (!length(wt)) 
        wt <- rep(1, n)
    offset <- model.offset(m)
    if (length(offset) <= 1) 
        offset <- rep(0, n)
    y <- model.response(m)
    if (!is.factor(y)) 
        stop("response must be a factor")
    lev <- levels(y)
    if (length(lev) <= 2) 
        stop("response must have 3 or more levels")
    y <- unclass(y)
    q <- length(lev) - 1
    Y <- matrix(0, n, q)
    .polrY1 <- col(Y) == y
    .polrY2 <- col(Y) == y - 1
    if (missing(start)) {
        q1 <- length(lev)%/%2
        y1 <- (y > q1)
        X <- cbind(Intercept = rep(1, n), x)
        fit <- switch(method, logistic = bayesglm.fit(X, y1, 
            wt, family = binomial(), offset = offset, intercept = TRUE, 
            prior.mean = prior.mean, prior.scale = prior.scale, 
            prior.scale.for.intercept = 10, prior.df = prior.df, 
            scaled = scaled, control = glm.control(maxit = n.iter)), #####
          probit = bayesglm.fit(X, y1, wt, 
            family = binomial("probit"), offset = offset, intercept = TRUE, 
            prior.mean = prior.mean, prior.scale = prior.scale, 
            prior.scale.for.intercept = 10, prior.df = prior.df, 
            scaled = scaled, control = glm.control(maxit = n.iter)), #####
          cloglog = bayesglm.fit(X, y1, wt, 
            family = binomial("probit"), offset = offset, intercept = TRUE, 
            prior.mean = prior.mean, prior.scale = prior.scale, 
            prior.scale.for.intercept = 10, prior.df = prior.df, 
            scaled = scaled, control = glm.control(maxit = n.iter)), #####
          cauchit = bayesglm.fit(X, y1, wt, 
            family = binomial("cauchit"), offset = offset, intercept = TRUE, 
            prior.mean = prior.mean, prior.scale = prior.scale, 
            prior.scale.for.intercept = 10, prior.df = prior.df, 
            scaled = scaled, control = glm.control(maxit = n.iter))) #####
        if (!fit$converged) 
            stop("attempt for find suitable starting values failed")
        coefs <- fit$coefficients
        if (any(is.na(coefs))) {
            warning("design appears to be rank-deficient, so dropping some coefs")
            keep <- names(coefs)[!is.na(coefs)]
            coefs <- coefs[keep]
            x <- x[, keep[-1], drop = FALSE]
            pc <- ncol(x)
        }
        spacing <- logit((1:q)/(q + 1))
        if (method != "logit") 
            spacing <- spacing/1.7
        gammas <- -coefs[1] + spacing - spacing[q1]
        thetas <- c(gammas[1], log(diff(gammas)))
        start <- c(coefs[-1], thetas)
    }
    else if (length(start) != pc + q) 
        stop("'start' is not of the correct length")
    J <- NCOL(x)
    if (length(prior.mean) == 1) 
        prior.mean <- rep(prior.mean, J)
    if (length(prior.scale) == 1) {
        prior.scale <- rep(prior.scale, J)
        if (scaled == TRUE) {
            for (j in 1:J) {
                x.obs <- x[, j]
                x.obs <- x.obs[!is.na(x.obs)]
                num.categories <- length(unique(x.obs))
                if (num.categories == 2) {
                  prior.scale[j] <- prior.scale[j]/(max(x.obs) - 
                    min(x.obs))
                }
                else if (num.categories > 2) {
                  prior.scale[j] <- prior.scale[j]/(2 * sd(x.obs))
                }
            }
        }
    }
    if (length(prior.df) == 1) 
        prior.df <- rep(prior.df, J)
    if (length(prior.mean.for.cutpoints) == 1) 
        prior.mean.for.cutpoints <- rep(prior.mean.for.cutpoints, 
            q)
    if (length(prior.scale.for.cutpoints) == 1) 
        prior.scale.for.cutpoints <- rep(prior.scale.for.cutpoints, 
            q)
    if (length(prior.df.for.cutpoints) == 1) 
        prior.df.for.cutpoints <- rep(prior.df.for.cutpoints, 
            q)
    res <- optim(start, fmin, gmin, method = "BFGS", hessian = Hess, 
        ...)
    beta <- res$par[seq_len(pc)]
    theta <- res$par[pc + 1:q]
    zeta <- cumsum(c(theta[1], exp(theta[-1])))
    deviance <- 2 * res$value
    niter <- c(f.evals = res$counts[1], g.evals = res$counts[2])
    names(zeta) <- paste(lev[-length(lev)], lev[-1], sep = "|")
    if (pc > 0) {
        names(beta) <- colnames(x)
        eta <- drop(x %*% beta)
    }
    else {
        eta <- rep(0, n)
    }
    cumpr <- matrix(pfun(matrix(zeta, n, q, byrow = TRUE) - eta), 
        , q)
    fitted <- t(apply(cumpr, 1, function(x) diff(c(0, x, 1))))
    dimnames(fitted) <- list(row.names(m), lev)
    fit <- list(coefficients = beta, zeta = zeta, deviance = deviance, 
        fitted.values = fitted, lev = lev, terms = Terms, df.residual = sum(wt) - 
            pc - q, edf = pc + q, n = sum(wt), nobs = sum(wt), 
        call = match.call(), method = method, convergence = res$convergence, 
        niter = niter)
    if (Hess) {
        dn <- c(names(beta), names(zeta))
        H <- res$hessian
        dimnames(H) <- list(dn, dn)
        fit$Hessian <- H
    }
    if (model) 
        fit$model <- m
    fit$na.action <- attr(m, "na.action")
    fit$contrasts <- cons
    fit$xlevels <- .getXlevels(Terms, m)
    class(fit) <- "polr"
    fit
}

bayesglm.fit <-
function (x, y, weights = rep(1, nobs), start = NULL, etastart = NULL, 
    mustart = NULL, offset = rep(0, nobs), family = gaussian(), 
    control = glm.control(), intercept = TRUE, prior.mean = 0, 
    prior.scale = 2.5, prior.scale.for.intercept = 10, prior.df = 1, 
    scaled = TRUE) 
{
    J <- NCOL(x)
    if (length(prior.mean) == 1) 
        prior.mean <- rep(prior.mean, J)
    if (length(prior.scale) == 1) {
        prior.scale <- rep(prior.scale, J)
        if (scaled == TRUE) {
            for (j in 1:J) {
                x.obs <- x[, j]
                x.obs <- x.obs[!is.na(x.obs)]
                num.categories <- length(unique(x.obs))
                if (num.categories == 2) {
                  prior.scale[j] <- prior.scale[j]/(max(x.obs) - 
                    min(x.obs))
                }
                else if (num.categories > 2) {
                  prior.scale[j] <- prior.scale[j]/(2 * sd(x.obs))
                }
            }
        }
    }
    if (is.numeric(prior.scale.for.intercept) & intercept) 
        prior.scale[1] <- prior.scale.for.intercept
    if (length(prior.df) == 1) 
        prior.df <- rep(prior.df, J)
    x <- as.matrix(x)
    xnames <- dimnames(x)[[2]]
    ynames <- if (is.matrix(y)) 
        rownames(y)
    else names(y)
    conv <- FALSE
    nobs <- NROW(y)
    nvars <- ncol(x)
    EMPTY <- nvars == 0
    if (is.null(weights)) 
        weights <- rep.int(1, nobs)
    if (is.null(offset)) 
        offset <- rep.int(0, nobs)
    variance <- family$variance
    dev.resids <- family$dev.resids
    aic <- family$aic
    linkinv <- family$linkinv
    mu.eta <- family$mu.eta
    if (!is.function(variance) || !is.function(linkinv)) 
        stop("'family' argument seems not to be a valid family object")
    valideta <- family$valideta
    if (is.null(valideta)) 
        valideta <- function(eta) TRUE
    validmu <- family$validmu
    if (is.null(validmu)) 
        validmu <- function(mu) TRUE
    if (is.null(mustart)) {
        eval(family$initialize)
    }
    else {
        mukeep <- mustart
        eval(family$initialize)
        mustart <- mukeep
    }
    if (EMPTY) {
        eta <- rep.int(0, nobs) + offset
        if (!valideta(eta)) 
            stop("invalid linear predictor values in empty model")
        mu <- linkinv(eta)
        if (!validmu(mu)) 
            stop("invalid fitted means in empty model")
        dev <- sum(dev.resids(y, mu, weights))
        w <- ((weights * mu.eta(eta)^2)/variance(mu))^0.5
        residuals <- (y - mu)/mu.eta(eta)
        good <- rep(TRUE, length(residuals))
        boundary <- conv <- TRUE
        coef <- numeric(0)
        iter <- 0
    }
    else {
        coefold <- NULL
        eta <- if (!is.null(etastart)) 
            etastart
        else if (!is.null(start)) 
            if (length(start) != nvars) 
                stop(gettextf("length of 'start' should equal %d and correspond to initial coefs for %s", 
                  nvars, paste(deparse(xnames), collapse = ", ")), 
                  domain = NA)
            else {
                coefold <- start
                offset + as.vector(if (NCOL(x) == 1) 
                  x * start
                else x %*% start)
            }
        else family$linkfun(mustart)
        mu <- linkinv(eta)
        if (!(validmu(mu) && valideta(eta))) 
            stop("cannot find valid starting values: please specify some")
        devold <- sum(dev.resids(y, mu, weights))
        boundary <- conv <- FALSE
        prior.sd <- prior.scale
        for (iter in 1:control$maxit) {
            good <- weights > 0
            varmu <- variance(mu)[good]
            if (any(is.na(varmu))) 
                stop("NAs in V(mu)")
            if (any(varmu == 0)) 
                stop("0s in V(mu)")
            mu.eta.val <- mu.eta(eta)
            if (any(is.na(mu.eta.val[good]))) 
                stop("NAs in d(mu)/d(eta)")
            good <- (weights > 0) & (mu.eta.val != 0)
            if (all(!good)) {
                conv <- FALSE
                warning("no observations informative at iteration ", 
                  iter)
                break
            }
            z <- (eta - offset)[good] + (y - mu)[good]/mu.eta.val[good]
            w <- sqrt((weights[good] * mu.eta.val[good]^2)/variance(mu)[good])
            ngoodobs <- as.integer(nobs - sum(!good))
            z.star <- c(z, prior.mean)
            x.star <- rbind(x, diag(NCOL(x)))
            if (intercept) 
                x.star[NROW(x) + 1, ] <- colMeans(x)
            w.star <- c(w, 1/prior.sd)
            good.star <- c(good, rep(TRUE, NCOL(x)))
            ngoodobs.star <- ngoodobs + NCOL(x)
            fit <- .Fortran("dqrls", qr = x.star[good.star, ] * 
                w.star, n = ngoodobs.star, p = nvars, y = w.star * 
                z.star, ny = as.integer(1), tol = min(1e-07, 
                control$epsilon/1000), coefficients = double(nvars), 
                residuals = double(ngoodobs.star), effects = double(ngoodobs.star), 
                rank = integer(1), pivot = 1:nvars, qraux = double(nvars), 
                work = double(2 * nvars), PACKAGE = "base")
            if (any(!is.finite(fit$coefficients))) {
                conv <- FALSE
                warning("non-finite coefficients at iteration ", 
                  iter)
                break
            }
            coefs.hat <- fit$coefficients
            V.coefs <- chol2inv(fit$qr[1:ncol(x.star), 1:ncol(x.star), 
                drop = FALSE])
            prior.sd <- ifelse(prior.df == Inf, prior.scale, 
                sqrt(((coefs.hat - prior.mean)^2 + diag(V.coefs) + 
                  prior.df * prior.scale^2)/(1 + prior.df)))
            start[fit$pivot] <- fit$coefficients
            eta <- drop(x %*% start)
            mu <- linkinv(eta <- eta + offset)
            dev <- sum(dev.resids(y, mu, weights))
            if (control$trace) 
                cat("Deviance =", dev, "Iterations -", iter, 
                  "\n")
            boundary <- FALSE
            if (!is.finite(dev)) {
                if (is.null(coefold)) 
                  stop("no valid set of coefficients has been found: please supply starting values", 
                    call. = FALSE)
                warning("step size truncated due to divergence", 
                  call. = FALSE)
                ii <- 1
                while (!is.finite(dev)) {
                  if (ii > control$maxit) 
                    stop("inner loop 1; cannot correct step size")
                  ii <- ii + 1
                  start <- (start + coefold)/2
                  eta <- drop(x %*% start)
                  mu <- linkinv(eta <- eta + offset)
                  dev <- sum(dev.resids(y, mu, weights))
                }
                boundary <- TRUE
                if (control$trace) 
                  cat("Step halved: new deviance =", dev, "\n")
            }
            if (!(valideta(eta) && validmu(mu))) {
                if (is.null(coefold)) 
                  stop("no valid set of coefficients has been found: please supply starting values", 
                    call. = FALSE)
                warning("step size truncated: out of bounds", 
                  call. = FALSE)
                ii <- 1
                while (!(valideta(eta) && validmu(mu))) {
                  if (ii > control$maxit) 
                    stop("inner loop 2; cannot correct step size")
                  ii <- ii + 1
                  start <- (start + coefold)/2
                  eta <- drop(x %*% start)
                  mu <- linkinv(eta <- eta + offset)
                }
                boundary <- TRUE
                dev <- sum(dev.resids(y, mu, weights))
                if (control$trace) 
                  cat("Step halved: new deviance =", dev, "\n")
            }
            if (iter > 1 & abs(dev - devold)/(0.1 + abs(dev)) < 
                control$epsilon) {
                conv <- TRUE
                coef <- start
                break
            }
            else {
                devold <- dev
                coef <- coefold <- start
            }
        }
        if (!conv) 
            warning("algorithm did not converge")
        if (boundary) 
            warning("algorithm stopped at boundary value")
        eps <- 10 * .Machine$double.eps
        if (family$family == "binomial") {
            if (any(mu > 1 - eps) || any(mu < eps)) 
                warning("fitted probabilities numerically 0 or 1 occurred")
        }
        if (family$family == "poisson") {
            if (any(mu < eps)) 
                warning("fitted rates numerically 0 occurred")
        }
        if (fit$rank < nvars) 
            coef[fit$pivot][seq(fit$rank + 1, nvars)] <- NA
        xxnames <- xnames[fit$pivot]
        residuals <- rep.int(NA, nobs)
        residuals[good] <- z - (eta - offset)[good]
        fit$qr <- as.matrix(fit$qr)
        nr <- min(sum(good), nvars)
        if (nr < nvars) {
            Rmat <- diag(nvars)
            Rmat[1:nr, 1:nvars] <- fit$qr[1:nr, 1:nvars]
        }
        else Rmat <- fit$qr[1:nvars, 1:nvars]
        Rmat <- as.matrix(Rmat)
        Rmat[row(Rmat) > col(Rmat)] <- 0
        names(coef) <- xnames
        colnames(fit$qr) <- xxnames
        dimnames(Rmat) <- list(xxnames, xxnames)
    }
    names(residuals) <- ynames
    names(mu) <- ynames
    names(eta) <- ynames
    wt <- rep.int(0, nobs)
    wt[good] <- w^2
    names(wt) <- ynames
    names(weights) <- ynames
    names(y) <- ynames
    wtdmu <- if (intercept) 
        sum(weights * y)/sum(weights)
    else linkinv(offset)
    nulldev <- sum(dev.resids(y, wtdmu, weights))
    n.ok <- nobs - sum(weights == 0)
    nulldf <- n.ok - as.integer(intercept)
    rank <- if (EMPTY) 
        0
    else fit$rank
    resdf <- n.ok - rank
    aic.model <- aic(y, n, mu, weights, dev) + 2 * rank
    list(coefficients = coef, residuals = residuals, fitted.values = mu, 
        effects = if (!EMPTY) fit$effects, R = if (!EMPTY) Rmat, 
        rank = rank, qr = if (!EMPTY) structure(fit[c("qr", "rank", 
            "qraux", "pivot", "tol")], class = "qr"), family = family, 
        linear.predictors = eta, deviance = dev, aic = aic.model, 
        null.deviance = nulldev, iter = iter, weights = wt, prior.weights = weights, 
        df.residual = resdf, df.null = nulldf, y = y, converged = conv, 
        boundary = boundary)
}

# R code for analysis of storable votes for the book

data.2player <- read.csv ("2playergames.csv")
data.3player <- read.csv ("3playergames.csv")
data.6player <- read.csv ("6playergames.csv")

# Simple analysis using data from just one person

subset <- data.2player[,"person"]==401
y <- data.2player[subset,"vote"]
x <- data.2player[subset,"value"]
fit.1 <- bayespolr (factor(y) ~ x)
display (fit.1)
