library("rprojroot")
root<-has_dirname("ROS-Examples")$make_fix_file()

sesame <- read.dta(file=root("Sesame/data","sesame_raw.dta"))
sesame$watched <- ifelse(sesame$viewcat==1, 0, 1)
sesame$encouraged <- ifelse(sesame$viewenc==2, 0, 1)
sesame$y <- sesame$postlet
sesame$pretest <- sesame$prelet
write.csv(sesame, root("Sesame/data","sesame.csv"), row.names=FALSE)
