library("rprojroot")
root<-has_dirname("ROS-Examples")$make_fix_file()

#' **Load and pre-process data**
congress_list <- vector("list", 49)
for (i in 1:49){
  year <- 1896 + 2*(i-1)
  file <- root("Congress/data",paste(year, ".asc", sep=""))
  data_year <- matrix(scan(file), byrow=TRUE, ncol=5)
  data_year <- cbind(rep(year, nrow(data_year)), data_year)
  dvote <- data_year[,5]
  rvote <- data_year[,6]
  vote <- dvote/(dvote + rvote)
  inc <- data_year[,4]
  inc[inc == -9] <- 0
  missing <- dvote== -9 | rvote == -9
  vote[missing & inc== -1] <- 0
  vote[missing & inc== 1] <- 1
  vote[dvote == 0] <- 0
  vote[rvote == 0]  <- 1
  congress_list[[i]] <- data.frame(data_year, vote, inc)
}

i86 <- (1986-1896)/2 + 1
cong84 <- congress_list[[i86-1]]
cong86 <- congress_list[[i86]]
cong88 <- congress_list[[i86+1]]
cong90 <- congress_list[[i86+2]]

# Data have some errors so we'll do a crude search-and-destroy of apparent inconsistencies
inc_fix <- function(data1, data2){
  inc2_clean <- data2$inc
  inc2_clean[data2$inc == -1 & data1$vote > 0.5] <- 1
  inc2_clean[data2$inc == 1 & data1$vote < 0.5] <- -1
  inc2_clean
}

#' Impute uncontested elections
uncontested_adj <- function(vote){
  vote[vote < 0.1] <- 0.25
  vote[vote > 0.9] <- 0.75
  return(vote)
}
congress <- data.frame(inc86=inc_fix(cong84, cong86), inc88=inc_fix(cong86, cong88), inc90=inc_fix(cong88, cong90),
                       v86=cong86$vote, v88=cong88$vote, v90=cong90$vote,
                       v86_adj=uncontested_adj(cong86$vote), v88_adj=uncontested_adj(cong88$vote), v90_adj=uncontested_adj(cong90$vote))
congress$inc88[263] <- -1   # result of special election in Lousisiana 4th district
write.csv(congress, root("Congress/data","congress.csv"), row.names=FALSE)
