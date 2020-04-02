
library("rprojroot")
root<-has_dirname("ROS-Examples")$make_fix_file()

# read mathematics and Portuguese language results
d1=read.table(root("Student/data","student-mat.csv"),sep=";",header=TRUE)
d2=read.table(root("Student/data","student-por.csv"),sep=";",header=TRUE)

# merge mathematics and Portuguese language results
d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 382 students

# pick unique columns
data=d3[,c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","traveltime.x","studytime.x","failures.x","schoolsup.x","famsup.x","paid.x","activities.x", "nursery", "higher.x", "internet", "romantic.x","famrel.x","freetime.x","goout.x","Dalc.x","Walc.x","health.x","absences.x","G1.x","G2.x","G3.x","G1.y","G2.y","G3.y")]

# rename columns
colnames(data)<-c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","traveltime","studytime","failures","schoolsup","famsup","paid","activities", "nursery", "higher", "internet", "romantic","famrel","freetime","goout","Dalc","Walc","health","absences","G1mat","G2mat","G3mat","G1por","G2por","G3por")

# save merged data
write.table(data, root("Student/data","student-merged.csv"),sep=";",col.names=TRUE)
