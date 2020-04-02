#' ---
#' title: "Regression and Other Stories: Student"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Models for regression coefficients. See Chapter 12 in
#' Regression and Other Stories.
#' 
#' -------------
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(message=FALSE, error=FALSE, warning=FALSE, comment=NA)
# switch this to TRUE to save figures in separate files
savefigs <- FALSE

#' **Load packages**
library("rprojroot")
root<-has_dirname("ROS-Examples")$make_fix_file()
library("rstanarm")
library("loo")
library("ggplot2")
library("bayesplot")
theme_set(bayesplot::theme_default(base_family = "sans"))

#' Set random seed for reproducability
SEED <- 2132

#+ eval=FALSE, include=FALSE
# grayscale figures for the book
if (savefigs) color_scheme_set(scheme = "gray")

#' In this section we consider regression with more than handful of
#' predictors. We demonstrate the usefulness of standardization of
#' predictors and models for regression coefficients.
#'
#' We demonstrate with data from Portuguese students and their final
#' period math grade.  We predict the third period math grade given
#' student's school, student's sex, student's age, student's home
#' address type, family size, parent's cohabitation status, mother's
#' education, father's education, home to school travel time, weekly
#' study tie, number of past class failures, extra educational
#' support, extra paid classes within the course subject,
#' extra-curricular activities, attended nursery school, wants to take
#' higher education, Internet access at home, with a romantic
#' relationship, quality of family relationships, free time after
#' school, going out with friends, workday alcohol consumption,
#' weekend alcohol consumption, current health status, and number of
#' school absences.
#' 

#' **Load data**
# use the merged data with students having both math and Portuguese language grades
data <- read.table(root("Student/data","student-merged.csv"),sep=";",header=TRUE)
# select only students with non-zero third year math grade
data <- data[data$G3mat>0,]
# We simplify by dropping categorical variables and present all
# ordinal predictors as numeric values. 
data[,1:26] <- apply(t(1:26),2,function (x) { as.numeric(data[,x]) })
# Pick columns to make models for third grade math grade.
data_G3mat <- as.data.frame(data[,c("G3mat","school","sex","age","address","famsize","Pstatus","Medu","Fedu","traveltime","studytime","failures","schoolsup","famsup","paid","activities", "nursery", "higher", "internet", "romantic","famrel","freetime","goout","Dalc","Walc","health","absences")])
n <- nrow(data_G3mat)
p <- ncol(data_G3mat)-1

#' ## Default weak prior on original coefficients
#' 
#' **Fit a regression model with default weak priors**
fit0 <- stan_glm(G3mat ~ ., data = data_G3mat, seed = SEED, refresh=0)

#' **Plot posterior marginals of coefficients**
p0 <- mcmc_areas(as.matrix(fit0), pars=vars(-'(Intercept)',-sigma),
                 prob_outer=0.95, area_method = "scaled height") +
  xlim(c(-3.2,3.2))
p0

#' Figure 1 shows that without standardization of predictors it looks
#' like there is different amount of uncertainty on the relevance of
#' the predictors. For example, it looks like absences has really
#' small relevance and high certainty.
#' 

#' Standardize all predictors for easier comparison of relevances as
#' dicussed in Section 12.1.
datastd <- data
datastd[,1:26] <- scale(data[,1:26])
datastd_G3mat <- as.data.frame(datastd[,c("G3mat","school","sex","age","address","famsize","Pstatus","Medu","Fedu","traveltime","studytime","failures","schoolsup","famsup","paid","activities", "nursery", "higher", "internet", "romantic","famrel","freetime","goout","Dalc","Walc","health","absences")])

#' ## Default weak prior on coefficients
#' 
#' **Fit a regression model with default weak priors**
fit1 <- stan_glm(G3mat ~ ., data = datastd_G3mat, seed = SEED, refresh=0)

#' **Plot posterior marginals of coefficients**
p1 <- mcmc_areas(as.matrix(fit1), pars=vars(-'(Intercept)',-sigma),
                 prob_outer=0.9, area_method = "scaled height") +
  xlim(c(-1.2,1.2))
p1

#' Figure 2 shows that after all predictors (including binary) have
#' standardized to have equal standard deviation the uncertainties on
#' the relevances are similar. For example, it is now easier to see
#' that compared to other predictors, absences has relatively high
#' relevance with high certainty. As we have many potential
#' predictors, we may worry that maybe some of them are not actually
#' predictive but add just noise to the model. First we can compare
#' Bayesian $R^2$ and LOO $R^2$ and the effective number of parameters
#' obtained from LOO log score.
#' 

#' **Compare Bayesian $R^2$ and LOO $R^2$**
round(median(bayes_R2(fit1)), 2)
round(median(loo_R2(fit1)), 2)

#' **Compute LOO log score**
(loo1 <- loo(fit1))

#' Medians of Bayesian $R^2$ and LOO $R^2$ are quite different, and p_loo
#' is approximately 26, which indicates that the model is fitting to
#' all predictors. The default prior in rstanarm is N(0,2.5)
#' independently for each coefficients. With many predictors it can be
#' useful to think more carefully what information we have and use
#' better models for regression coefficients.
#'
#' If the predictors have been standardized to have standard deviation
#' 1 and we have N(0,2.5) independent priors on regression
#' coefficients, then that prior implies that variance of the prior
#' predictive distribution is ${26 * 2.5^2 + \sigma^2} = 162.5 +
#' \sigma$. The default prior for $\sigma$ is exponential scaled to
#' have mean equal to data standard deviation which in this case is
#' approximately 3.3 which is much less than 162.5. We can examine
#' what does this mean as a prior for explained variance $R^2$.
#' 

#' **Bayesian $R^2$ distribution**
ggplot() + geom_histogram(aes(x=bayes_R2(fit1)), breaks=seq(0,1,length.out=100)) +
  xlim(c(0,1)) +
  scale_y_continuous(breaks=NULL) +
  labs(x="Bayesian R^2", y="")

#' **Prior predictive checking by looking at the prior on Bayesian $R^2$**</br>
ppR2<-numeric()
for (i in 1:10000) {
  muvar <- var(as.matrix(datastd_G3mat[,2:27]) %*% rnorm(26)*2.5)
  sigma2 <- rexp(1,rate=0.3)^2;
  ppR2[i] <- muvar/(muvar+sigma2)
}
ggplot()+geom_histogram(aes(x=ppR2), breaks=seq(0,1,length.out=50)) +
  xlim(c(0,1)) +
  scale_y_continuous(breaks=NULL) +
  labs(x="Prior predictive Bayesian R^2",y="")

#' Figure 3 shows that with the default prior on regression
#' coefficients and $\sigma$, the implied prior on $R^2$ is strongly
#' favoring larger values and thus is favoring overfitted models.
#'
#' What would be more sensible prior models for regression
#' coefficients when we have many predictors? It is very unlikely that
#' all predictors would be strongly related to the outcome. We may
#' assume either many predictors having small relevance each or only
#' some of the predictors having high relevance and the rest of the
#' predictors having negligible relevance. We present regression
#' coefficient prior models for both cases and in the end of section
#' discuss other alternatives.
#'
#' If we assume that many predictors may have small relevance each, we
#' can scale the independent priors so that the sum of the prior
#' variance stays reasonable. In this case we have 26 six predictors
#' and could have a prior guess that explained variance is 0.3. Then a
#' simple approach would to use for regression coefficients a scaled
#' prior $\mathrm{N}(0, \mathrm{sd}(y)*\sqrt{1/26}*\sqrt{0.3})$ and for
#' $\sigma$ and exponential with mean set to $\sqrt{0.7} *
#' \mathrm{sd}(y)$. Then the expected prior predictive variance is
#' approximately the same as the data variance and the implied prior
#' on explained variance $R^$ is more evenly distributed as shown in
#' Figure 4. This simple approach still has a bit too much mass on
#' values very near 0 and 1, but this not a problem if the data is not
#' supporting that extreme values. The implied prior on $R^2$ could
#' further improved by changing the prior for $\sigma$ and using a
#' joint prior on regression coefficients and $\sigma$.
#' 

#' ## Weakly informative prior scaled with the number of covariates
#' 
#' **Prior predictive checking by looking at the prior on Bayesian $R^2$**
ppR2<-numeric()
for (i in 1:10000) {
  muvar <- var(as.matrix(datastd_G3mat[,2:27]) %*% rnorm(26, sd=sd(data$G3mat)/sqrt(26)*sqrt(0.3)))
  sigma2 <- 0.7*rexp(1, rate=1/sd(data$G3mat))^2
  ppR2[i] <- muvar/(muvar+sigma2)
}
ggplot()+geom_histogram(aes(x=ppR2), breaks=seq(0,1,length.out=50)) +
  xlim(c(0,1)) +
  scale_y_continuous(breaks=NULL) +
  labs(x="Prior predictive Bayesian R^2",y="")

#' **Fit a regression model with a weakly informative prior scaled with the number of covariates**
fit2 <- stan_glm(G3mat ~ ., data = datastd_G3mat, seed = SEED,
                 prior=normal(scale=sd(data$G3mat)/sqrt(26)*sqrt(0.3), autoscale=FALSE),
                 refresh=0)

#' When we compare Bayesian $R^2$ and LOO $R^2$, we see the difference
#' is much smaller and LOO $R^2$ has improved slightly.
#'
#' **Compare Bayesian $R^2$ and LOO $R^2$**
round(median(loo_R2(fit2)), 2)
round(median(bayes_R2(fit2)), 2)

#' **Bayesian $R^2$ distribution**
ggplot()+geom_histogram(aes(x=bayes_R2(fit2)), breaks=seq(0,1,length.out=100)) +
  xlim(c(0,1)) +
  scale_y_continuous(breaks=NULL) +
  labs(x="Bayesian R^2",y="")

#' When we make model comparison using LOO log score, the difference
#' is clear although small.
#' 

#' **Compute LOO log score**
(loo2 <- loo(fit2))

#' **Compare models**
loo_compare(loo1,loo2)

#' **Plot posterior marginals of coefficients**
p2 <- mcmc_areas(as.matrix(fit2), pars=vars(-'(Intercept)',-sigma),
                 prob_outer=0.9, area_method = "scaled height") +
  xlim(c(-1.2,1.2))
p2

#' Figure 5 shows also posterior marginals of coefficients, which are
#' slightly more concentrated than for the previous model in Figure 2.

#' We may also assume that only some of the predictors having high
#' relevance and the rest of the predictors having negligible
#' relevance. One possibility for modeling this assumption is
#' regularized horseshoe prior (the name comes from implied U-shape
#' prior on the amount of shrinkage for coefficients).

#' ## Weakly informative prior assuming only some covariates are relevant
#'
#' We define global scale parameter of the regularized horseshoe prior
#' by assuming that the expected number of relevant predictors is near
#' p0=6 and the expected residual standard deviation is near $\sigma$.
p0 <- 6
sigma <- sd(fit1$residuals)
global_scale <- p0 / (p - p0) * sigma / sqrt(n)
slab_scale <- sd(data$G3mat)/sqrt(p0)*sqrt(0.3)

ppR2<-numeric()
for (i in 1:10000) {
  z <- rnorm(p)
  lambda <- rcauchy(p)
  tau <- rcauchy(1, scale = global_scale*2)
  caux <- 1/rgamma(1, shape=0.5, rate=0.5)
  c <-  slab_scale * sqrt(caux)
  lambda_tilde <- sqrt(c^2 * lambda^2 / (c^2 + tau^2*lambda^2))
  beta <- rnorm(p) * lambda_tilde * tau
  muvar <- var(as.matrix(datastd_G3mat[,2:27]) %*% beta)
  sigma2 <- 0.7*rexp(1,rate=0.3)^2;
  ppR2[i] <- muvar/(muvar+sigma2)
}
ggplot()+geom_histogram(aes(x=ppR2), breaks=seq(0,1,length.out=50)) +
  xlim(c(0,1)) +
  scale_y_continuous(breaks=NULL) +
  labs(x="Prior predictive Bayesian R^2",y="")

#' Figure 6 shows that the regularized horseshoe prior with sensible
#' parameters also implies more evenly distributed prior on explained
#' variance $R^2$ than the default wide prior. The prior is still
#' having a bit too much probability mass near 1, which could be fixed
#' with a zero avoiding prior on $\sigma$.
#' 

#' **Fit a regression model with regularized horseshoe prior**
fit3 <- stan_glm(G3mat ~ ., data = datastd_G3mat, seed = SEED,
                 prior=hs(global_scale=global_scale, slab_scale=slab_scale), refresh=0)

#' When we compare Bayesian $R^2$ and LOO $R^2$, we see the difference
#' is small and LOO $R^2$ is better than with the wide prior.
#'
#' **Compare Bayesian $R^2$ and LOO $R^2$**
round(median(loo_R2(fit3)), 2)
round(median(bayes_R2(fit3)), 2)

#' When we compare models using LOO log score, the difference to first
#' model is small. There is no difference in predictive performance
#' between scaled normal and regularized horseshoe, and thus data is
#' not informative enough to tell which regression coefficient model
#' is better.
#' 

#' **Compute LOO log score**
(loo3 <- loo(fit3))

#' **Compare models**
loo_compare(loo1,loo3)
loo_compare(loo2,loo3)

#' **Bayesian $R^2$ distribution**
ggplot()+geom_histogram(aes(x=bayes_R2(fit3)), breaks=seq(0,1,length.out=100)) +
  xlim(c(0,1)) +
  scale_y_continuous(breaks=NULL) +
  labs(x="Bayesian R^2",y="")

#' **Plot posterior marginals of coefficients**
mcmc_areas(as.matrix(fit3), pars=vars(-'(Intercept)',-sigma),
           prob_outer=0.9, area_method = "scaled height") +
  xlim(c(-1.2,1.2))

#' Figure 7 shows that the regularized horseshoe prior has the benefit
#' of shrinking the posterior for many regression coefficients more
#' tightly towards 0, making it easier to see the most relevant
#' predictors. By eyeballing we see that failures, school support,
#' going out and the number of absences are the most relevant
#' predictors. We can test how well good predictions we could get if
#' we only use those predictors.
#' 

#' ## Subset of covariates
#'

#' Fit a regression model with subset of covariates and default weak
#' prior on coefficients
fit4 <- stan_glm(G3mat ~ failures + schoolsup + goout + absences, data = datastd_G3mat,
                 seed = SEED, refresh=0)

#' When we compare Bayesian $R^2$ and LOO $R^2$, we see the difference
#' is small and there is less overfit. LOO $R^2$ is such slightly
#' smaller than for models with all predictors and the prediction
#' performance can not improved much by adding more predictors. Note
#' that this hold for this data, and by observing more students it
#' might be possible to learn regression coefficients for other
#' predictors with sufficient small uncertainty so that predictions for
#' new students could be improved.
#' 

#' **Compare Bayesian $R^2$ and LOO $R^2$**
round(median(loo_R2(fit4)), 2)
round(median(bayes_R2(fit4)), 2)

#' **Bayesian $R^2$ distribution**
ggplot()+geom_histogram(aes(x=bayes_R2(fit4)), breaks=seq(0,1,length.out=100)) +
  xlim(c(0,1)) +
  scale_y_continuous(breaks=NULL) +
  labs(x="Bayesian R^2",y="")

#' **Compute LOO log score**
(loo4 <- loo(fit4))

#' **Compare models**
loo_compare(loo3,loo4)
loo_compare(loo1,loo4)

#' **Plot posterior marginals of coefficients**
mcmc_areas(as.matrix(fit4), pars=vars(-'(Intercept)',-sigma),
           prob_outer=0.99, area_method = "scaled height") +
  xlim(c(-1.3,0.1))


#' ## Other models for regression coefficients
#'
#' In the end add short discussion of BenG prior on $R^2$ and Lasso.
#' 
