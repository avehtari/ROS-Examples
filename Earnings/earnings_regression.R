#' ---
#' title: "Regression and Other Stories: Earnings"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Predict respondents' yearly earnings using survey data from
#' 1990. See Chapters 6, 9 and 12 in Regression and Other Stories.
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
options(mc.cores = parallel::detectCores())
library("ggplot2")
library("bayesplot")
theme_set(bayesplot::theme_default(base_family = "sans"))
#+ eval=FALSE, include=FALSE
# grayscale figures for the book
if (savefigs) color_scheme_set(scheme = "gray")

#' Set random seed for reproducability
SEED <- 7783

#' **Load data**
earnings_all <- read.csv(root("Earnings/data","earnings.csv"))
earnings_all$positive <- earnings_all$earn > 0
# scale earnings to thousands of dollars
earnings_all$earnk <- earnings_all$earn/1000 
# only non-zero earnings
earnings <- earnings_all[earnings_all$positive, ]
n <- nrow(earnings)
height_jitter_add <- runif(n, -.2, .2)

#' ### Normal linear regression

#' **Model on 1k dollars scale**
#' 
#' The option `refresh = 0` supresses the default Stan sampling
#' progress output. This is useful for small data with fast
#' computation. For more complex models and bigger data, it can be
#' useful to see the progress.
fit_1 <- stan_glm(earnk ~ height, data = earnings,
                  seed = SEED, refresh = 0)
print(fit_1)
#' for plotting scale back to dollar scale
coef1 <- coef(fit_1)*1000

#' **Plot linear model**
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Earnings/figs","heights1a.pdf"), height=8.5, width=11)
#+
par(mar=c(6,6,4,2)+.1)
plot(earnings$height + height_jitter_add, earnings$earn, xlab="height", ylab="earnings",
     cex=.8, cex.lab=3, pch=20, cex.axis=3, yaxt="n", mgp=c(4,1.5,0),
     col="gray10",  cex.main=3, main="Fitted linear model")
abline(coef1, lwd=2)
abline(0,0,col="gray")
axis(2, c(0,100000,200000), c("0","100000","200000"), mgp=c(4,1.1,0),cex.axis=3)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' **Plot linear model, ggplot version**
gg_earnings <- ggplot(earnings, aes(x = height + height_jitter_add, y = earn)) +
  geom_point(alpha = 0.75) +
  geom_hline(yintercept = 0, color = "darkgray") +
  geom_abline(intercept = coef1[1], slope = coef1[2], size = 1) +
  labs(x = "height", y = "earnings",
       title = "Fitted linear model")
gg_earnings

#' **Plot extrapolation**
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Earnings/figs","heights1b.pdf"), height=8.5, width=11)
#+
par(mar=c(6,6,4,2)+.1)
plot(xlim=c(0,max(earnings$height)), ylim=c(-70000,200000), earnings$height + height_jitter_add, earnings$earn, xlab="height", ylab="earnings", cex=.8, cex.lab=3, pch=20, cex.axis=3, yaxt="n", mgp=c(4,1.5,0), col="gray10", cex.main=3, main="Extrapolation")
abline(coef1, lwd=2)
abline(0,0,col="gray")
intercept <- coef1[1]
axis(2, c(intercept,0,100000,200000), c(round(intercept,-2),"0","100000","200000"), mgp=c(4,1.1,0),cex.axis=3)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' **Plot extrapolation, ggplot version, modifying the gg_earnings object
#' we already created**
gg_earnings +
  ylim(-70000, 200000) +
  xlim(0, 80) +
  labs(title = "Extrapolation")


#' **Include male/female**
#+ results='hide'
fit_2 <- stan_glm(earnk ~ height + male, data = earnings,
                  seed = SEED)
#+
print(fit_2)
#' for plotting scale back to dollar scale
coef2 <- coef(fit_2)*1000

#' **Plot linear model with male/female**
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Earnings/figs","heights2.pdf"), height=8.5, width=11)
#+
par(mar=c(6,6,5,2)+.1)
plot(range(earnings$height), range(predict(fit_2)*1000), type="n", xlab="height", ylab="predicted earnings", cex=.8, cex.lab=3, pch=20, cex.axis=3, mgp=c(4,1.5,0), yaxt="n", col="gray10",
      cex.main=3, main="Fitted regression, displayed as\nseparate lines for men and women", bty="l")
axis(2, c(20000,30000), cex.axis=3)
abline(coef2[1], coef2[2], col="red", lwd=2)
text(68, coef2[1] + coef2[2]*65, "women:\ny = -11 000 + 450x", cex=3, adj=0, col="red")
abline(coef2[1]+coef2[3], coef2[2], col="blue", lwd=2)
text(68, coef2[1]+coef2[3] + coef2[2]*65, "men:\ny = -2 000 + 450x", cex=3, adj=0, col="blue")
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' **Include male/female, ggplot version**
ggplot(earnings, aes(height, earn)) +
  geom_blank() +
  geom_abline(
    intercept = c(coef2[1], coef2[1] + coef2[3]),
    slope = coef2[2],
    color = c("red", "blue")
  ) +
  coord_cartesian(
    ylim = range(predict(fit_2)*1000),
    xlim = range(earnings$height)
  ) +
  annotate(
    "text",
    x = c(68, 68),
    y = c(coef2[1] + coef2[2] * 65, coef2[1] + coef2[3] + coef2[2] * 65),
    label = c("women:\ny = -11 000 + 450x", "men:\ny = -2 000 + 450x"),
    color = c("red", "blue"),
    size = 5, hjust = 0
  ) +
  labs(
    x = "height",
    y = "predicted earnings",
    title = "Fitted regression, displayed as\nseparate lines for men and women"
  )


#' **Include interaction**
#+ results='hide'
fit_3 <- stan_glm(earnk ~ height + male + height:male, data = earnings,
                  seed = SEED, refresh = 0)
#+
print(fit_3)
#' for plotting scale back to dollar scale
coef3 <- coef(fit_3)*1000

#' **Plot linear model with interaction**
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Earnings/figs","heights3.pdf"), height=8.5, width=11)
#+
par(mar=c(6,6,5,2)+.1)
plot(range(earnings$height), range(predict(fit_3)*1000), type="n", xlab="height", ylab="predicted earnings", cex=.8, cex.lab=3, pch=20, cex.axis=3, mgp=c(4,1.5,0), yaxt="n", col="gray10", cex.main=3, main="Fitted regression with interactions,\nseparate lines for men and women", bty="l")
axis(2, c(20000,30000), cex.axis=3)
abline(coef3[1], coef3[2], col="red", lwd=2)
text(62, coef3[1] + coef3[2]*80, "women:\ny = -7 000 + 180x", cex=3, adj=0, col="red")
abline(coef3[1]+coef3[3], coef3[2]+coef3[4], col="blue", lwd=2)
text(68, coef3[1]+coef3[3] + (coef3[2]+coef3[4])*66, "men:\ny = -22 000 + 740x", cex=3, adj=0, col="blue")
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' **Include interaction, ggplot version**
ggplot(earnings, aes(height, earn)) +
  geom_blank() +
  geom_abline(
    intercept = c(coef3[1], coef3[1] + coef3[3]),
    slope = c(coef3[2], coef3[2] + coef3[4]),
    color = c("red", "blue")
  ) +
  coord_cartesian(
    ylim = range(predict(fit_3)*1000),
    xlim = range(earnings$height)
  ) +
  annotate(
    "text",
    x = c(62, 68),
    y = c(coef3[1] + coef3[2] * 80, coef3[1]+coef3[3] + (coef3[2]+coef3[4])*66),
    label = c("women:\ny = -7 000 + 180x", "men:\ny = -22 000 + 740x"),
    color = c("red", "blue"),
    size = 5, hjust = 0
  ) +
  labs(
    x = "height",
    y = "predicted earnings",
    title = "Fitted regression with interactions,\nseparate lines for men and women"
  )

#' ### Linear regression on log scale

#' **Models on log scale**
earnings$log_earn <- log(earnings$earn)
#+ results='hide'
logmodel_1 <- stan_glm(log_earn ~ height, data = earnings,
                       seed = SEED, refresh = 0)
#+
print(logmodel_1, digits=2)

#' **Model on log10 scale**
earnings$log10_earn <- log10(earnings$earn)
#+ results='hide'
log10model_1 <- stan_glm(log10_earn ~ height, data = earnings,
                         seed = SEED, refresh = 0)
#+
print(log10model_1, digits=3)

#' **Model on log scale with two predictors**
#+ results='hide'
logmodel_2 <- stan_glm(log_earn ~ height + male, data = earnings,
                       seed = SEED, refresh = 0)
#+
print(logmodel_2, digits=2)

#' **Model on log scale for the target and one predictor**
earnings$log_height <- log(earnings$height)
#+ results='hide'
loglogmodel_2 <- stan_glm(log_earn ~ log_height + male, data = earnings,
                          seed = SEED, refresh = 0)
#+
print(loglogmodel_2, digits=2)

#' **Model on log scale with two predictors and interaction**
logmodel_3 <- stan_glm(log_earn ~ height + male + height:male, data = earnings,
                       seed = SEED, refresh = 0)
#+
print(logmodel_3, digits=2)

#' **Model on log scale with standardized interaction**
earnings$z_height <- with(earnings, (height - mean(height))/sd(height))
#+ results='hide'
logmodel_3a <- stan_glm(log_earn ~ z_height + male + z_height:male,
                        data = earnings, seed = SEED, refresh = 0)
#+
print(logmodel_3a, digits=2)

#' ### Uncertainty

#' **Posterior uncertainty for log model**
sims <- as.matrix(logmodel_2)
n_sims <- nrow(sims)

#' **Plot posterior draws of linear model on log scale**
#+ eval=FALSE, include=FALSE
postscript(root("Earnings/figs","heights.log1a.ps"), horizontal=TRUE)
#+
par(mar=c(6,6,4,2)+.1)
plot(earnings$height + runif(n,-.2,.2), earnings$log_earn, xlab="height", ylab="log (earnings)", cex=.8, cex.lab=3, pch=20, cex.axis=3, yaxt="n", mgp=c(4,1.5,0), col="gray10",
      cex.main=3, main="Log regression, plotted on log scale")
axis(2, seq(6,12,2), mgp=c(4,1.1,0),cex.axis=3)
sims_display <- sample(n_sims, 10)
for (i in sims_display){
  curve(sims[i,1] + sims[i,2]*x, lwd=0.5, col="gray30", add=TRUE)
}
curve(coef(logmodel_2)[1] + coef(logmodel_2)[2]*x, add=TRUE)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' **Plot posterior draws of linear model on log scale, ggplot version**
sims_display <- sample(n_sims, 10)
ggplot(earnings, aes(height, log_earn)) +
  geom_jitter(height = 0, width = 0.25) +
  geom_abline(
    intercept = sims[sims_display, 1],
    slope = sims[sims_display, 2],
    color = "darkgray"
  ) +
  geom_abline(
    intercept = coef(logmodel_2)[1],
    slope = coef(logmodel_2)[2]
  ) +
  labs(
    x = "height",
    y = "log(earnings)",
    title = "Log regression, plotted on log scale"
  )

#' ### Posterior predictive checking

#' **Posterior predictive checking for model in linear scale**
yrep_1 <- posterior_predict(fit_1)
n_sims <- nrow(yrep_1)
sims_display <- sample(n_sims, 100)
ppc_1 <- ppc_dens_overlay(earnings$earnk, yrep_1[sims_display,])
#' **Posterior predictive checking for model in log scale**
yrep_log_1 <- posterior_predict(logmodel_1)
n_sims <- nrow(yrep_log_1)
sims_display <- sample(n_sims, 100)
ppc_log_1 <- ppc_dens_overlay(earnings$log_earn, yrep_log_1[sims_display,])
(bpg <- bayesplot_grid(
  ppc_1, ppc_log_1,
  grid_args = list(ncol = 2),
  titles = c("earn", "log(earn)")
))
#+ eval=FALSE, include=FALSE
ggsave(root("Earnings/figs","earnings_ppc.pdf"), bpg, height=3, width=9)

#' **Posterior predictive checking for model in linear scale**
yrep_2 <- posterior_predict(fit_2)
n_sims <- nrow(yrep_2)
sims_display <- sample(n_sims, 100)
ppc_dens_overlay(earnings$earnk, yrep_2[sims_display,])

#' **Posterior predictive checking for model in log scale**
yrep_log_2 <- posterior_predict(logmodel_2)
n_sims <- nrow(yrep_log_2)
sims_display <- sample(n_sims, 100)
ppc_dens_overlay(earnings$log_earn, yrep_log_2[sims_display,])

#' **Posterior predictive checking for model in log-log scale**
yrep_loglog_2 <- posterior_predict(loglogmodel_2)
n_sims <- nrow(yrep_loglog_2)
sims_display <- sample(n_sims, 100)
ppc_dens_overlay(earnings$log_earn, yrep_loglog_2[sims_display,])
