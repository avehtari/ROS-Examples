setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/FrenchElection")
library("arm")
ps <- read.table("ps_primaire.txt", header=TRUE)
J <- nrow(ps)
n1 <- ps[J,2]
n2 <- ps[J,3] - ps[J,2]
p1 <- ps[1:(J-1),2]/n1
p2 <- (ps[1:(J-1),3] - ps[1:(J-1),2])/n2
diff <- p2 - p1
se_diff <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
round(cbind(diff, se_diff)*10000)
print(cbind(p1*n2, p2*n2))
