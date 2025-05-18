
library("rprojroot")
root<-has_file(".ROS-Examples-root")$make_fix_file()

# read mathematics and Portuguese language results
d1=read.table(root("Student/data","student-mat.csv"),sep=";",header=TRUE)
d2=read.table(root("Student/data","student-por.csv"),sep=";",header=TRUE)

# merge mathematics and Portuguese language results, with all=FALSE
d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 682 students

# pick unique columns
data=d3[,c("G1.x","G2.x","G3.x","G1.y","G2.y","G3.y","school","sex","age","address","famsize","Pstatus","Medu","Fedu","traveltime.x","studytime.x","failures.x","schoolsup.x","famsup.x","paid.x","activities.x", "nursery", "higher.x", "internet", "romantic.x","famrel.x","freetime.x","goout.x","Dalc.x","Walc.x","health.x","absences.x")]

# rename columns
colnames(data)<-c("G1mat","G2mat","G3mat","G1por","G2por","G3por","school","sex","age","address","famsize","Pstatus","Medu","Fedu","traveltime","studytime","failures","schoolsup","famsup","paid","activities", "nursery", "higher", "internet", "romantic","famrel","freetime","goout","Dalc","Walc","health","absences")

# for simplicity transform binary factors to 0/1 coding
predictors_binary <- c("school","sex","address","famsize","Pstatus","schoolsup","famsup","paid","activities","nursery","higher","internet","romantic")
data[,predictors_binary] <- apply(t(1:13), 2, function (x) { as.numeric(as.factor(data[,predictors_binary[x]]))-1 })

# save merged data
write.csv(data, root("Student/data","student-merged.csv"),row.names=FALSE)


# read mathematics and Portuguese language results
d1=read.table(root("Student/data","student-mat.csv"),sep=";",header=TRUE)
d2=read.table(root("Student/data","student-por.csv"),sep=";",header=TRUE)

# merge mathematics and Portuguese language results, with all=TRUE
d3=merge(d1,d2,all=TRUE,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 682 students

# pick unique columns
data=d3[,c("G1.x","G2.x","G3.x","G1.y","G2.y","G3.y","school","sex","age","address","famsize","Pstatus","Medu","Fedu","traveltime.x","studytime.x","failures.x","schoolsup.x","famsup.x","paid.x","activities.x", "nursery", "higher.x", "internet", "romantic.x","famrel.x","freetime.x","goout.x","Dalc.x","Walc.x","health.x","absences.x")]

# rename columns
colnames(data)<-c("G1mat","G2mat","G3mat","G1por","G2por","G3por","school","sex","age","address","famsize","Pstatus","Medu","Fedu","traveltime","studytime","failures","schoolsup","famsup","paid","activities", "nursery", "higher", "internet", "romantic","famrel","freetime","goout","Dalc","Walc","health","absences")

# for simplicity transform binary factors to 0/1 coding
predictors_binary <- c("school","sex","address","famsize","Pstatus","schoolsup","famsup","paid","activities","nursery","higher","internet","romantic")
data[,predictors_binary] <- apply(t(1:13), 2, function (x) { as.numeric(as.factor(data[,predictors_binary[x]]))-1 })

write.csv(data, root("Student/data","student-merged-all.csv"),row.names=FALSE)
