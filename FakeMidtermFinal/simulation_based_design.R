#' ---
#' title: "Regression and Other Stories: Fake dataset of a randomized experiment on student grades"
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

#' Fake dataset of a randomized experiment on student grades. See
#' Chapter 16 in Regression and Other Stories.
#' 
#' -------------
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(message=FALSE, error=FALSE, warning=FALSE, comment=NA)
# switch this to TRUE to save figures in separate files
savefigs <- FALSE

#' #### Load packages
library("rstanarm")

#' #### First simulation
n <- 100
y_if_control <- rnorm(n, 60, 20)
y_if_treated <- y_if_control + 5
z <- sample(rep(c(0,1), n/2))
y <- ifelse(z==1, y_if_treated, y_if_control)
fake <- data.frame(y, z)
diff <- mean(y[z==1]) - mean(y[z==0])
se_diff <- sqrt(sd(y[z==0])^2/sum(z==0) + sd(y[z==1])^2/sum(z==1))
print(c(diff, se_diff), digits=2)
fit_1a <- stan_glm(y ~ z, data=fake, refresh=0)
print(fit_1a)

fake$x <- rnorm(n, 50, 20)
fit_1b <- stan_glm(y ~ z + x, data=fake, refresh=0)
print(fit_1b)

#' #### Simulation with realistic pre-test
n <- 100
true_ability <- rnorm(n, 50, 16)
x <- true_ability + rnorm(n, 0, 12)
y_if_control <- true_ability + rnorm(n, 0, 12) + 10
y_if_treated <- y_if_control + 5
z <- sample(rep(c(0,1), n/2))
y <- ifelse(z==1, y_if_treated, y_if_control)
fake_2 <- data.frame(x, y, z)
fit_2a <- stan_glm(y ~ z, data=fake_2, refresh=0)
fit_2b <- stan_glm(y ~ z + x, data=fake_2, refresh=0)
print(fit_2a)
print(fit_2b)

#' #### Simulating selection bias in treatment assignment
invlogit <- plogis
z <- rbinom(n, 1, invlogit(-(x-50)/20))

pdf("design_bias.pdf", height=4, width=6)
par(mar=c(3, 3, 2, 1), mgp=c(1.7, .5, 0), tck=-.01)
plot(x, ifelse(z==1, .99, .01), ylim=c(0,1), xlab="Pre-test score", ylab="Pr (z=1)", xaxt="n", yaxt="n", yaxs="i", bty="l", pch=20)
axis(1, seq(0,100,20))
axis(2, seq(0,1,.5))
text(45, 0.93, "(assigned to treatment group, z=1)", col="gray40")
text(55, 0.07, "(assigned to control group, z=0)", col="gray40")
curve(invlogit(-(x-50)/20), add=TRUE)
dev.off()

y <- ifelse(z==1, y_if_treated, y_if_control)
fake_3 <- data.frame(x, y, z)

Experiment <- function(n) {
  true_ability <- rnorm(n, 50, 16)
  x <- true_ability + rnorm(n, 0, 12)
  y_if_control <- true_ability + rnorm(n, 0, 12) + 10
  y_if_treated <- y_if_control + 5
  z <- rbinom(n, 1, invlogit(-(x-50)/20))
  y <- ifelse(z==1, y_if_treated, y_if_control)
  fake_3 <- data.frame(x, y, z)
  fit_3a <- stan_glm(y ~ z, data=fake_3, refresh=0)
  fit_3b <- stan_glm(y ~ z + x, data=fake_3, refresh=0)
  inferences <- rbind(c(coef(fit_3a)["z"], se(fit_3a)["z"]), c(coef(fit_3b)["z"], se(fit_3b)["z"]))
  return(inferences)
}

n <- 100
n_loop <- 50
results <- array(NA, c(n_loop, 2, 2), dimnames=list(1:n_loop, c("Simple", "Adjusted"), c("Estimate", "SE")))
for (loop in 1:n_loop){
  results[loop,,] <- Experiment(n)
}
results_avg <- apply(results, c(2,3), mean)
