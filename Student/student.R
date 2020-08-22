#' ---
#' title: "Regression and Other Stories: Student"
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

#' Models for regression coefficients. See Chapter 12 in
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
library("rstanarm")
library("rstantools")
library("loo")
library("ggplot2")
library("bayesplot")
theme_set(bayesplot::theme_default(base_family = "sans"))

#' Set random seed for reproducibility
SEED <- 2132

#+ eval=FALSE, include=FALSE
# grayscale figures for the book
if (savefigs) color_scheme_set(scheme = "gray")

#' Here we consider regression with more than handful of
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

#' #### Load data
# Use the merged data with students having both math and Portuguese language grades
data <- read.csv(root("Student/data","student-merged.csv"))
head(data)
grades <- c("G1mat","G2mat","G3mat","G1por","G2por","G3por")
predictors <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","traveltime","studytime","failures","schoolsup","famsup","paid","activities", "nursery", "higher", "internet", "romantic","famrel","freetime","goout","Dalc","Walc","health","absences")
p <- length(predictors)

#' #### Data for predicting the final math grade
#' 
#' Pick columns to make models for third period mathematics grade G3mat and
#' select only students with non-zero math grade
#' (use G3por to make a model for third grade Portuguese language grade).
data_G3mat <- subset(data, subset=G3mat>0, select=c("G3mat",predictors))
n <- nrow(data_G3mat)

#' ## Default weak prior on original coefficients
#' 
#' #### Fit a regression model with default weak priors
#' 
#' Dot (.) in the formula means all other columns execpt what is
#' already on the left of ~.
fit0 <- stan_glm(G3mat ~ ., data = data_G3mat, seed = SEED, refresh=0)

#' #### Plot posterior marginals of coefficients
p0 <- mcmc_areas(as.matrix(fit0), pars=vars(-'(Intercept)',-sigma),
                 prob_outer=0.95, area_method = "scaled height") +
  xlim(c(-3.2,2.4))
p0 <- p0 + scale_y_discrete(limits = rev(levels(p0$data$parameter)))
p0
#+ eval=FALSE, include=FALSE
if (savefigs)
  ggsave(root("Student/figs","student_fit0_mcmc_areas.pdf"), p0, height=5, width=5, colormodel="gray")

#' The above figure shows that without standardization of predictors,
#' it looks like there is a different amount of uncertainty on the
#' relevance of the predictors. For example, it looks like `absences`
#' has really small relevance and high certainty.
#' 

#' Standardize all predictors for easier comparison of relevances as
#' discussed in Section 12.1.
datastd_G3mat <- data_G3mat
datastd_G3mat[,predictors] <-scale(data_G3mat[,predictors])
                   
#' ## Default weak prior on coefficients
#' 
#' #### Fit a regression model with default weak priors
fit1 <- stan_glm(G3mat ~ ., data = datastd_G3mat, seed = SEED, refresh=0)

#' #### Plot posterior marginals of coefficients
p1 <- mcmc_areas(as.matrix(fit1), pars=vars(-'(Intercept)',-sigma),
                 prob_outer=0.95, area_method = "scaled height") +
  xlim(c(-1.2,0.8))
p1 <- p1 + scale_y_discrete(limits = rev(levels(p1$data$parameter)))
p1
#+ eval=FALSE, include=FALSE
if (savefigs)
  ggsave(root("Student/figs","student_fit1_mcmc_areas.pdf"), p1, height=5, width=5, colormodel="gray")

#' The above figure shows that after all predictors have been
#' standardized to have equal standard deviation, the uncertainties on
#' the relevances are similar. For example, it is now easier to see
#' that `absences` has relatively high relevance compared to other
#' predictors in the model.
#' 

#' #### Compare Bayesian $R^2$ and LOO $R^2$
round(median(bayes_R2(fit1)), 2)
round(median(loo_R2(fit1)), 2)

#' #### Compute LOO log score
(loo1 <- loo(fit1))

#' Medians of Bayesian $R^2$ and LOO $R^2$ are quite different, and p_loo
#' is approximately 26, which indicates that the model is fitting to
#' all predictors. 
#'
#' If the predictors have been standardized to have standard deviation
#' 1 and we give the regression coefficients independent normal priors
#' with mean 0 and standard deviation 2.5, this implies that the prior
#' standard deviation of the modeled predictive means is $2.5
#' \sqrt{26} = 12.7$. The default prior for $\sigma$ is an exponential
#' distribution, scaled to have mean equal to data standard deviation
#' which in this case is approximately 3.3 which is much less than
#' 12.7. We can simulate from these prior distributions and examine
#' what is the corresponding prior distribution for explained variance
#' $R^2$.
#' 

#' #### Bayesian $R^2$ distribution
ggplot() + geom_histogram(aes(x=bayes_R2(fit1)), breaks=seq(0,1,length.out=100)) +
  xlim(c(0,1)) +
  scale_y_continuous(breaks=NULL) +
  labs(x="Bayesian R^2", y="")

#' #### Prior predictive checking by looking at the prior on Bayesian $R^2$</br>
ppR2<-numeric()
for (i in 1:4000) {
  sigma2 <- rexp(1,rate=0.3)^2;
  muvar <- var(as.matrix(datastd_G3mat[,2:27]) %*% rnorm(26)*2.5)
  ppR2[i] <- muvar/(muvar+sigma2)
}
ggplot()+geom_histogram(aes(x=ppR2), breaks=seq(0,1,length.out=50)) +
  xlim(c(0,1)) +
  scale_y_continuous(breaks=NULL) +
  labs(x="Prior predictive Bayesian R^2",y="")

pp1 <- mcmc_hist(data.frame(Prior=ppR2,Posterior=bayes_R2(fit1)),
                 breaks=seq(0,1,length.out=100),
                 facet_args = list(nrow = 2)) +
  facet_text(size = 13) +
  scale_x_continuous(limits = c(0,1), expand = c(0, 0),
                     labels = c("0","0.25","0.5","0.75","1")) +
  theme(axis.line.y = element_blank()) +
  xlab("Bayesian R^2")
pp1
#+ eval=FALSE, include=FALSE
if (savefigs)
  ggsave(root("Student/figs","student_fit1_R2.pdf"), pp1, height=3, width=3, colormodel="gray")

#' The above figure shows that with the default prior on regression
#' coefficients and $\sigma$, the implied prior distribution for $R^2$
#' is strongly favoring larger values and thus is favoring overfitted
#' models. The priors often considered as weakly informative for
#' regression coefficients turn out to be in multiple predictor case
#' highly informative for the explained variance.
#'

#' ## Weakly informative prior scaled with the number of covariates
#' 
#' #### Prior predictive checking by looking at the prior on Bayesian $R^2$
ppR2<-numeric()
for (i in 1:4000) {
  sigma2 <- 0.7*rexp(1, rate=1/sd(datastd_G3mat$G3mat))^2
  muvar <- var(as.matrix(datastd_G3mat[,2:27]) %*% rnorm(26, sd=sd(datastd_G3mat$G3mat)/sqrt(26)*sqrt(0.3)))
  ppR2[i] <- muvar/(muvar+sigma2)
}
ggplot()+geom_histogram(aes(x=ppR2), breaks=seq(0,1,length.out=50)) +
  xlim(c(0,1)) +
  scale_y_continuous(breaks=NULL) +
  labs(x="Prior predictive Bayesian R^2",y="")

#' #### Fit a regression model with a weakly informative prior scaled with the number of covariates
fit2 <- stan_glm(G3mat ~ ., data = datastd_G3mat, seed = SEED,
                 prior=normal(scale=sd(datastd_G3mat$G3mat)/sqrt(26)*sqrt(0.3),
                              autoscale=FALSE),
                 refresh=0)

#' When we compare Bayesian $R^2$ and LOO $R^2$, we see the difference
#' is much smaller and LOO $R^2$ has improved slightly.
#'
#' #### Compare Bayesian $R^2$ and LOO $R^2$
round(median(loo_R2(fit2)), 2)
round(median(bayes_R2(fit2)), 2)

#' #### Bayesian $R^2$ distribution
ggplot()+geom_histogram(aes(x=bayes_R2(fit2)), breaks=seq(0,1,length.out=100)) +
  xlim(c(0,1)) +
  scale_y_continuous(breaks=NULL) +
  labs(x="Bayesian R^2",y="")

pp2 <- mcmc_hist(data.frame(Prior=ppR2,Posterior=bayes_R2(fit2)),
                 breaks=seq(0,1,length.out=100),
                 facet_args = list(nrow = 2)) +
  facet_text(size = 13) +
  scale_x_continuous(limits = c(0,1), expand = c(0, 0),
                     labels = c("0","0.25","0.5","0.75","1")) +
  theme(axis.line.y = element_blank()) +
  xlab("Bayesian R^2")
pp2
#+ eval=FALSE, include=FALSE
if (savefigs)
  ggsave(root("Student/figs","student_fit2_R2.pdf"), pp2, height=3, width=3, colormodel="gray")

#' Comparison of the LOO log score reveals that the new model has
#' better leave-one-out prediction.
#' 

#' #### Compute LOO log score
(loo2 <- loo(fit2))

#' #### Compare models
loo_compare(loo1,loo2)

#' #### Plot posterior marginals of coefficients
p2 <- mcmc_areas(as.matrix(fit2), pars=vars(-'(Intercept)',-sigma),
                 prob_outer=0.95, area_method = "scaled height") +
  xlim(c(-1.2,0.8))
p2 <- p2 + scale_y_discrete(limits = rev(levels(p2$data$parameter)))
p2
#+ eval=FALSE, include=FALSE
if (savefigs)
  ggsave(root("Student/figs","student_fit2_mcmc_areas.pdf"), p2, height=5, width=5, colormodel="gray")

#' The above figure shows the posterior distributions of coefficients,
#' which are slightly more concentrated than for the previous model.
#' 

#' ## Weakly informative prior assuming only some covariates are relevant
#'
#' We next use regularized horseshoe pruior, assuming that the
#' expected number of relevant predictors is near $p_0=6$ and the
#' prior scale for the relevant predictors is chosen as in the
#' previous model but using $p_0$ for scaling. We can then simulate
#' from this prior and examine the corresponding prior for $R^2$
p0 <- 6
slab_scale <- sd(datastd_G3mat$G3mat)/sqrt(p0)*sqrt(0.3)
#
ppR2<-numeric()
for (i in 1:4000) {
  sigma2 <- 0.7*rexp(1,rate=1/sd(datastd_G3mat$G3mat))^2;
  global_scale <- p0 / (p - p0) * sqrt(sigma2) / sqrt(n)
  z <- rnorm(p)
  lambda <- rcauchy(p)
  tau <- rcauchy(1, scale = global_scale)
  caux <- 1/rgamma(1, shape=0.5, rate=0.5)
  c <-  slab_scale * sqrt(caux)
  lambda_tilde <- sqrt(c^2 * lambda^2 / (c^2 + tau^2*lambda^2))
  beta <- rnorm(p) * lambda_tilde * tau
  muvar <- var(as.matrix(datastd_G3mat[,2:27]) %*% beta)
  ppR2[i] <- muvar/(muvar+sigma2)
}
ggplot()+geom_histogram(aes(x=ppR2), breaks=seq(0,1,length.out=50)) +
  xlim(c(0,1)) +
  scale_y_continuous(breaks=NULL) +
  labs(x="Prior predictive Bayesian R^2",y="")

#' The above figure shows that the regularized horseshoe prior with
#' sensible parameters implies a more cautious prior on explained
#' variance $R^2$ than is implicitly assumed by the default wide
#' prior. The horseshoe prior favors simpler models, but is quite flat
#' around most $R^2$ values.
#' 

#' #### Fit a regression model with regularized horseshoe prior</br>
p0 <- 6
slab_scale <- sd(datastd_G3mat$G3mat)/sqrt(p0)*sqrt(0.3)
# global scale without sigma, as the scaling by sigma happens in stan_glm
global_scale <- p0 / (p - p0) / sqrt(n)
fit3 <- stan_glm(G3mat ~ ., data = datastd_G3mat, seed = SEED,
                 prior=hs(global_scale=global_scale, slab_scale=slab_scale),
                 refresh=0)

#' #### Compare Bayesian $R^2$ and LOO $R^2$
round(median(loo_R2(fit3)), 2)
round(median(bayes_R2(fit3)), 2)

#' When we compare models using LOO log score, the new model is better
#' than the default prior model, but there is no difference compared
#' the model with normal prior scaled with the predictors. It is
#' common that the data do not have strong information about how many
#' predictors are relevant and then different types of priors can
#' produce similar predictive accuracies.
#' 

#' #### Compute LOO log score
(loo3 <- loo(fit3))

#' #### Compare models
loo_compare(loo1,loo3)
loo_compare(loo2,loo3)

#' #### Bayesian $R^2$ distribution
ggplot()+geom_histogram(aes(x=bayes_R2(fit3)), breaks=seq(0,1,length.out=100)) +
  xlim(c(0,1)) +
  scale_y_continuous(breaks=NULL) +
  labs(x="Bayesian R^2",y="")

pp3 <- mcmc_hist(data.frame(Prior=ppR2,Posterior=bayes_R2(fit3)),
                 breaks=seq(0,1,length.out=100),
                 facet_args = list(nrow = 2)) +
  facet_text(size = 13) +
  scale_x_continuous(limits = c(0,1), expand = c(0, 0),
                     labels = c("0","0.25","0.5","0.75","1")) +
  theme(axis.line.y = element_blank()) +
  xlab("Bayesian R^2")
pp3
#+ eval=FALSE, include=FALSE
if (savefigs)
  ggsave(root("Student/figs","student_fit3_R2.pdf"), pp3, height=3, width=3, colormodel="gray")


#' #### Plot posterior marginals of coefficients
p3 <- mcmc_areas(as.matrix(fit3), pars=vars(-'(Intercept)',-sigma),
                 prob_outer=0.95, area_method = "scaled height") +
  xlim(c(-1.2,0.8))
p3 <- p3 + scale_y_discrete(limits = rev(levels(p3$data$parameter)))
p3
#+ eval=FALSE, include=FALSE
if (savefigs)
  ggsave(root("Student/figs","student_fit3_mcmc_areas.pdf"), p3, height=5, width=5, colormodel="gray")

#' The above figure shows that the regularized horseshoe prior has the
#' benefit of shrinking the posterior for many regression coefficients
#' more tightly towards 0, making it easier to see the most relevant
#' predictors. Failures, school support, going out, and the number of
#' absences appear to be the most relevant predictors.
#' 

#' ## Subset of covariates
#'

#' Fit a regression model with subset of covariates and default weak
#' prior on coefficients
fit4 <- stan_glm(G3mat ~ failures + schoolsup + goout + absences,
                 data = datastd_G3mat, seed = SEED, refresh=0)

#' When we compare Bayesian $R^2$ and LOO $R^2$, we see the difference
#' is small and there is less overfit than when using all predictors
#' with wide prior. LOO $R^2$ is just slightly smaller than for models
#' with all predictors and better priors. The prediction performance
#' can not improved much by adding more predictors. Note that by
#' observing more students it might be possible to learn regression
#' coefficients for other predictors with sufficient small uncertainty
#' so that predictions for new students could be improved.
#' 

#' #### Compare Bayesian $R^2$ and LOO $R^2$
round(median(loo_R2(fit4)), 2)
round(median(bayes_R2(fit4)), 2)

#' #### Bayesian $R^2$ distribution
ggplot()+geom_histogram(aes(x=bayes_R2(fit4)), breaks=seq(0,1,length.out=100)) +
  xlim(c(0,1)) +
  scale_y_continuous(breaks=NULL) +
  labs(x="Bayesian R^2",y="")

#' #### Compute LOO log score
(loo4 <- loo(fit4))

#' #### Compare models
loo_compare(loo3,loo4)
loo_compare(loo2,loo4)
loo_compare(loo1,loo4)

#' #### Plot posterior marginals of coefficients
p4 <- mcmc_areas(as.matrix(fit4), pars=vars(-'(Intercept)',-sigma),
                 prob_outer=0.99, area_method = "scaled height") +
  xlim(c(-1.3,0.1))
p4 <- p4 + scale_y_discrete(limits = rev(levels(p4$data$parameter)))
p4
