library("rprojroot")
root<-has_dirname("ROS-Examples")$make_fix_file()
data <- read.csv(root("Beauty/data","ProfEvaltnsBeautyPublic.csv"), header=TRUE)

# Rename the two variables for convenience
data$beauty <- data$btystdave
data$eval <- data$courseevaluation

# Create the course index variable
courses <- data[, 18:47]   # (indicators for the 30 courses)
n <- nrow(data)
J <- ncol(courses) + 1
course_id <- rep(0, n)
for (i in 1:n){
  for (j in 1:30){
    if (courses[i,j]==1) course_id[i] <- j
  }
}
data$course_id <- course_id

beauty <- data[,c("eval","beauty","female","age","minority","nonenglish","lower","course_id")]
write.csv(beauty, root("Beauty/data","beauty.csv"), row.names=FALSE)

