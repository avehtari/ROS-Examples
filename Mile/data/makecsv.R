mile <- read.table(root("Mile/data","mile2.txt"), header=TRUE)
year <- mile$yr + mile$month/12
seconds <- mile$min*60 + mile$sec
mile <- data.frame(mile, year, seconds)
write.csv(mile, root("Mile/data","mile.csv"), row.names=FALSE, quote=FALSE)
