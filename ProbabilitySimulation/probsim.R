#' ---
#' title: "Regression and Other Stories: Simulation"
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

#' Simulation of probability models. See Chapter 5 in
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

#' #### Simulate how many girls in 400 births?
n_girls <- rbinom(1, 400, 0.488)
print(n_girls)

#' #### Repeat simulation 1000 times
n_sims <- 1000
n_girls <- rep(NA, n_sims)
for (s in 1:n_sims){
  n_girls[s] <- rbinom(1, 400, 0.488)}

#' #### Plot
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("ProbabilitySimulation/figs","girls1.pdf"), height=3.5, width=5.5)
#+
par(mar=c(3,3,1,1),  mgp=c(1.5,.5,0), tck=-.01)
hist(n_girls, main="", xaxt="n", yaxt="n")
axis (1, seq(150,250,25), mgp=c(1.5,.5,0))
axis (2, seq(0,200,100), mgp=c(1.5,.5,0))
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Accounting for twins
birth_type <- sample(c("fraternal twin","identical twin","single birth"),
  size=400, replace=TRUE, prob=c(1/125, 1/300, 1 - 1/125 - 1/300))
girls <- rep(NA, 400)
for (i in 1:400){
  if (birth_type[i]=="single birth"){
    girls[i] <- rbinom(1, 1, 0.488)}
  else if (birth_type[i]=="identical twin"){
    girls[i] <- 2*rbinom(1, 1, 0.495)}
  else if (birth_type[i]=="fraternal twin"){
    girls[i] <- rbinom(1, 2, 0.495)}
}
n_girls <- sum(girls)

girls <- ifelse(birth_type=="single birth", rbinom(400, 1, 0.488),
  ifelse(birth_type=="identical twins", 2*rbinom(400, 1, 0.495),
  rbinom(400, 2, 0.495)))

#' #### Repeat 1000 times
n_girls <- rep(NA, n_sims)
for (s in 1:n_sims){
  birth_type <- sample(c("fraternal twin","identical twin","single birth"),
    size=400, replace=TRUE, prob=c(1/125, 1/300, 1 - 1/125 - 1/300))
  girls <- rep(NA, 400)
  for (i in 1:400){
    if (birth_type[i]=="single birth"){
      girls[i] <- rbinom(1, 1, 0.488)}
    else if (birth_type[i]=="identical twin"){
      girls[i] <- 2*rbinom(1, 1, 0.495)}
    else if (birth_type[i]=="fraternal twin"){
      girls[i] <- rbinom(1, 2, 0.495)}
  }
  n_girls[s] <- sum(girls)
}

#' #### Plot
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("ProbabilitySimulation/figs","girls2.pdf"), height=3.5, width=5.5)
#+
par(mar=c(3,3,1,1),  mgp=c(1.5,.5,0), tck=-.01)
hist (n_girls, main="", xaxt="n", yaxt="n", mgp=c(1.5,.5,0))
axis (1, seq(150,250,25), mgp=c(1.5,.5,0))
axis (2, seq(0,200,100), mgp=c(1.5,.5,0))
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Simulation of continuous and mixed discrete/continuous models
n_sims <- 1000
y1 <- rnorm(n_sims, 3, 0.5)
y2 <- exp(y1)
y3 <- rbinom(n_sims, 20, 0.6)
y4 <- rpois(n_sims, 5)

#' #### Plot
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("ProbabilitySimulation/figs","4dists.pdf"), height=7, width=10)
#+
par(mar=c(4,3,4,3),  mgp=c(1.5,.5,0), tck=-.01)
par(mfrow=c(2,2))
hist(y1, breaks=seq(floor(min(y1)), ceiling(max(y1)), 0.2), main="1000 draws from normal dist with dist. with mean 3, sd 0.5")
hist(y2, breaks=seq(0, ceiling(max(y2)) + 5, 5),  main="1000 draws from corresponding lognormal dist.")
hist(y3, breaks=seq(-0.5, 20.5, 1), main="1000 draws from binomial dist. with 20 tries, probability 0.6")
hist(y4, breaks=seq(-0.5, max(y4) + 1, 1), main="1000 draws from Poisson dist. with mean 5")
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Generate the height of one randomly chosen adult
male <- rbinom(1, 1, 0.48)
height <- ifelse(male==1, rnorm(1, 69.1, 2.9), rnorm(1, 64.5, 2.7))

#' #### Select 10 adults at random
N <- 10
male <- rbinom(N, 1, 0.48)
height <- ifelse(male==1, rnorm(N, 69.1, 2.9), rnorm(N, 64.5, 2.7))
avg_height <- mean(height)
print(avg_height)

#' #### Repeat the simulation 1000 times
n_sims <- 1000
avg_height <- rep(NA, n_sims)
for (s in 1:n_sims){
  N <- 10
  male <- rbinom(N, 1, 0.48)
  height <- ifelse(male==1, rnorm(N, 69.1, 2.9), rnorm(N, 64.5, 2.7))
  avg_height[s] <- mean(height)
}
hist(avg_height, main="Dist of avg height of 10 adults")

#' #### The maximum height of the 10 people
max_height <- rep(NA, n_sims)
n_sims <- 1000
avg_height <- rep(NA, n_sims)
for (s in 1:n_sims){
  N <- 10
  male <- rbinom(N, 1, 0.48)
  height <- ifelse(male==1, rnorm(N, 69.1, 2.9), rnorm(N, 64.5, 2.7))
  avg_height[s] <- mean(height)
  max_height[s] <- max(height)
}
hist(max_height, main="Dist of max height of 10 adults")
