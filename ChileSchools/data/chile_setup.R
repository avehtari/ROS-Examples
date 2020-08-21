library("rprojroot")
root<-has_dirname("ROS-Examples")$make_fix_file()

#' The outcomes in these analyses are the *gain scores* between 88 and 92.
chile_full <- read.dta(root("ChileSchools/data","chile_full.dta"))
names(chile_full)

#' Only Use Large Urban Schools 
chile_full <- subset(chile_full, big==1)
chile_full$eligible = (chile_full$rule2<0)*1

chile_full$read92 <- chile_full$cas92
chile_full$read88 <- chile_full$cas88

chile_full$math92 <- chile_full$mat92
chile_full$math88 <- chile_full$mat88

#' reduce to complete cases
chile <- chile_full[c("p90", "cmb_regn", "urban88", "rule2", 
               "cutoff", "cutoff_cmb", "eligible","read92","read88","math92","math88")]
chile <- chile[complete.cases(chile),]

write.csv(chile, root("ChileSchools/data","chile.csv"), row.names=FALSE)
