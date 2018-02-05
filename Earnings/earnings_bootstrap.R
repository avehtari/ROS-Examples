setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/Earnings")
library("arm")

## classical regressions and graphs for earnings example

earnings_clean <- read.csv("earnings.csv")
n <- nrow(earnings_clean)

earn <- earnings_clean$earn
height <- earnings_clean$height
male <- earnings_clean$male

colnames(height_data) <- c("ID", "earn", "height", "male")
print(height_data[1:10,])

print(mean(height[male]) / mean(height[!male]))

n <- length(height)
boot <- sample(n, replace=TRUE)
height_boot <- height[boot]
male_boot <- male[boot]
ratio_boot <- mean(height_boot[male_boot]) / mean(height_boot[!male_boot])

Boot_ratio <- function(data){
  n <- nrow(data)
  boot <- sample(n, replace=TRUE)
  height_boot <- data$height[boot]
  male_boot <- data$male[boot]
  ratio_boot <- mean(height_boot[male_boot]) / mean(height_boot[!male_boot])
  return(ratio_boot)
}

n_sims <- 100
output <- replicate(n_sims, Boot_ratio(data=earnings_clean))

hist(output)
print(sd(output))
