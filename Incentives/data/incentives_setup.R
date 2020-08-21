library("rprojroot")
root<-has_dirname("ROS-Examples")$make_fix_file()

data <- read.table(root("Incentives/data","incentives_full.txt"),
                         header=TRUE)
rr_diff <- data$r.dif * 100
value <- data$v.dif
prepay <- data$t + 0.5
gift  <- data$f + 0.5
burden <- data$b + 0.5
incentives <- data.frame(rr_diff, value, prepay, gift, burden)
incentives <- incentives[!is.na(rr_diff),]
write.csv(incentives, root("Incentives/data","incentives.csv"), row.names=FALSE)
