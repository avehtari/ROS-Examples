#' #### Load packages
library("rprojroot")
root<-has_file(".ROS-Examples-root")$make_fix_file()

#' #### Load data
allnames <- read.csv(root("Names/data","SSA-longtail-names.csv"))
girl <- as.vector(allnames$sex)=="F"
names <- as.vector(allnames$name)

#' #### Remove Laura's list of bad names
error.names <- as.matrix(read.csv(root("Names/data","ErrorNames.csv")))
error.names.girl <- error.names[,2]=="F"
bad.girl.names <- error.names[error.names.girl,1]
bad.boy.names <- error.names[!error.names.girl,1]
N <- nrow(allnames)
keep <- rep(TRUE, N)
for (i in (1:N)) {
  if (girl[i] & names[i] %in% bad.girl.names) keep[i] <- FALSE
  if (!girl[i] & names[i] %in% bad.boy.names) keep[i] <- FALSE
}

#' #### Write into a file
allnames_clean <- allnames[keep,]
write.csv(allnames_clean, root("Names/data","allnames_clean.csv"), row.names=FALSE)

# # In next file:
# allnames <- read.csv(root("Names/data","allnames_clean.csv"))
# girl <- as.vector(allnames$sex)=="F"
# names <- as.vector(allnames$name)
