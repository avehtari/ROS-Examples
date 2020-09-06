# Set up cleaned dataset for Bangladesh well-switching

# Load libraries
library("rprojroot")
root<-has_file(".ROS-Examples-root")$make_fix_file()
library("foreign")

# Read in the data
all <- read.dta(root("Arsenic/data","all.dta"), convert.factors=F)

# For simplicity, pull out all wells with missing data in the variables that we
# will be using in our analysis
missing <- is.na(all[,"func"] + all[,"as"] + all[,"distnearest"] + all[,"assn"] + all[,"ed"])
table(missing)

# Include only the wells that are functioning (func==1), "unsafe"
# (as>50) and with no missing data
keep <- all[,"func"]==1 & all[,"as"]>50 & !missing

# Give convenient names to the variables
switch <- all$switch
arsenic <- all$as/100
dist <- all$distnearest
dist100 <- all$distnearest/100
assoc <- ifelse (all$assn>0,1,0)
educ <- all$ed
educ4 <- all$ed/4

wells.data <- cbind(switch, arsenic, dist, dist100, assoc, educ, educ4)
write.csv(wells.data[keep,], root("Arsenic/data","wells.csv"), row.names=FALSE)
