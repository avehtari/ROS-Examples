library("rprojroot")
root<-has_dirname("ROS-Examples")$make_fix_file()

load(root("ChildCare/data","cc2.Rdata"))
keep <- setdiff(names(cc2), c('row.names', 'row.names.1'))
write.csv(cc2[,keep], root("ChildCare/data","cc2.csv"), row.names=FALSE)
