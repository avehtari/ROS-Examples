setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/Congress")
library("arm")
library("rstanarm")

# Simulation example from section 7.2 of ARM

congress <- vector("list", 49)
for (i in 1:49){
  year <- 1896 + 2*(i-1)
  file <- paste("cong3/", year, ".asc", sep="")
  data_year <- matrix(scan(file), byrow=TRUE, ncol=5)
  data_year <- cbind(rep(year, nrow(data_year)), data_year)
  congress[[i]] <- data_year
}

i86 <- (1986-1896)/2 + 1
cong86 <- congress[[i86]]
cong88 <- congress[[i86+1]]
cong90 <- congress[[i86+2]]

v86 <- cong86[,5]/(cong86[,5]+cong86[,6])
bad86 <- cong86[,5]==-9 | cong86[,6]==-9
v86[bad86] <- NA
contested86 <- v86>.1 & v86<.9
inc86 <- cong86[,4]
inc86[inc86 == -9] <- 0

v88 <- cong88[,5]/(cong88[,5]+cong88[,6])
bad88 <- cong88[,5]==-9 | cong88[,6]==-9
v88[bad88] <- NA
contested88 <- v88>.1 & v88<.9
inc88 <- cong88[,4]
inc88[inc88 == -9] <- 0

v90 <- cong90[,5]/(cong90[,5]+cong90[,6])
bad90 <- cong90[,5]==-9 | cong90[,6]==-9
v90[bad90] <- NA
contested90 <- v90>.1 & v90<.9
inc90 <- cong90[,4]
inc90[inc90 == -9] <- 0

# cleaned

uncontested_adj <- function(vote){
  vote[vote < 0.1] <- 0.25
  vote[vote > 0.9] <- 0.75
  return(vote)
}
v86_adj <- uncontested_adj(v86)
v88_adj <- uncontested_adj(v88)
v90_adj <- uncontested_adj(v90)

# regression predicting 1988 from 1986

data_88 <- data.frame(vote=v88_adj, past_vote=v86_adj, inc=inc88)
fit_88 <- stan_glm(vote ~ past_vote + inc, data=data_88)
print(fit_88, digits=2)

data_90 <- data.frame(past_vote=v88_adj, incumbency=inc90)
pred_90 <- posterior_predict(fit_88, data=data_90)



# pull out the estimate of beta, its covariance matrix, and the estimated residual sd

n <- sum(!is.na(vote_88+vote_86+incumbency_88))
k <- 3
beta_hat <- coef(fit_88)
V_beta <- summary(fit_88,correlation=TRUE)$cov.unscaled
sd_hat <- sigma.hat(fit_88)

# Crude estimate of how many Dems will win

n_new <- length(v88)
v88_adjusted <- ifelse(v88<.1, .25, ifelse(v88>.9, .75, v88))
v88_adjusted <- ifelse(is.na(v88), .5, v88_adjusted)
X_new <- cbind(rep(1,n_new), v88_adjusted, inc90)
y_hat_new <- as.vector(beta_hat %*% t(X_new))
dems_new_crude <- sum(y_hat_new[!is.na(v90)] > .5) +
  .5*sum(y_hat_new[!is.na(v90)]==.5)
print(dems_new_crude)

# Now, more appropriate simulation-based predictions for 1990 from 1988

# First, get simulations from the post distribution of beta and sigma
n_sims <- 1000
sigma <- rep(NA, n_sims)
beta <- array(NA, c(n_sims,k))
dimnames(beta) <- list(NULL, names(beta_hat))
for(s in 1:n_sims){
  sigma[s] <- sd_hat*sqrt((n-k)/rchisq(1,n-k))
  beta[s,] <- mvrnorm(1, beta_hat, V_beta*sigma[s]^2)
}

# Now, set up the simulations for all the districts
y_new <- array(NA, c(n_sims,n_new))
ok <- contested90 & !is.na(v90)
y_new[,ok] <- beta %*% t(X_new[ok,]) + rnorm(n_sims*sum(ok), 0, sigma)
y_new[,v90<.1] <- 0
y_new[,v90>.9] <- 1
y_new[,is.na(v90)] <- NA

# How many districts are contested?
print(sum(ok))

# Transform from continuous vote into Democratic or Republican seat
winner_new <- ifelse(y_new>.5, "Democrat", "Republican")
dems_new <- rep(NA, n_sims)

# For each simulation, sum over all the districts
for(s in 1:n_sims){
  dems_new[s] <- sum(winner_new[s,]=="Democrat", na.rm=TRUE)
}

# Finally:  our posterior mean and sd of how many districts the Dems will win
print(c(mean(dems_new), sqrt(var(dems_new))))

# Just for laffs, you can sum in the other direction and get Pr(Dem win) for each district
prob_dem_new <- rep(NA, n_new)
for(i in 1:n_new){
  prob_dem_new[i] <- mean(winner_new[,i]=="Democrat")
}
