library("rprojroot")
root<-has_dirname("ROS-Examples")$make_fix_file()

read.columns <- function(filename, columns){
  start <- min(columns)
  length <- max(columns) - start + 1
  if (start==1){
    return(read.fwf(filename, widths=length))
  }
  else {
    return(read.fwf(filename, widths=c(start-1, length))[,2])
  }
}

# Read in the raw data
height_feet <- read.columns(root("Earnings/data","wfw90.dat"), 144)
height_inches <- read.columns(root("Earnings/data","wfw90.dat"), 145:146)
weight <- read.columns(root("Earnings/data","wfw90.dat"), 147:149)
earn_exact <- read.columns(root("Earnings/data","wfw90.dat"), 203:208)
earn2 <- read.columns(root("Earnings/data","wfw90.dat"), 209:210)
sex <- read.columns(root("Earnings/data","wfw90.dat"), 219)
race <- read.columns(root("Earnings/data","wfw90.dat"), 198)
hisp <- read.columns(root("Earnings/data","wfw90.dat"), 200)
ed <- read.columns(root("Earnings/data","wfw90.dat"), 190:191)
momed <- read.columns(root("Earnings/data","wfw90.dat"), 192:193)
fathed <- read.columns(root("Earnings/data","wfw90.dat"), 194:195)
walk <- read.columns(root("Earnings/data","wfw90.dat"), 139:140)
exer <- read.columns(root("Earnings/data","wfw90.dat"), 142)
smokenow <- read.columns(root("Earnings/data","wfw90.dat"), 150)
tense <- read.columns(root("Earnings/data","wfw90.dat"), 133)
angry <- read.columns(root("Earnings/data","wfw90.dat"), 133)
yearbn <- read.columns(root("Earnings/data","wfw90.dat"), 196:197)

# Take a look and fix up

table(height_feet, height_inches)
table(weight)
table(sex)
height_inches[height_inches>11] <- NA
height_feet[height_feet>=7] <- NA
height <- 12*height_feet + height_inches
male <- 2 - sex
table(male, height)

weight[weight>500] <- NA

earn_approx <- earn2
earn_approx[earn2==0] <- 100
earn_approx[earn2==1] <- median(earn_exact[earn_exact>100000], na.rm=TRUE)/1000
earn_approx[earn2==35] <- 45
earn_approx[earn2==45] <- 35
earn_approx[earn2==90] <- NA
earn_approx[earn2==91] <- 20
earn_approx[earn2==92] <- 10
earn_approx[earn2==93] <- 50
earn_approx[earn2==94] <- 40
earn_approx[earn2==95] <- 75
earn_approx[earn2==96] <- 100
earn_approx[earn2>=97] <- NA

earn <- ifelse(!is.na(earn_exact), earn_exact, 1000*earn_approx)

ethnicity <- ifelse(hisp==1, "Hispanic",
               ifelse(race==1, "White",
                 ifelse(race==2, "Black",
                   "Other")))

education <- ifelse(ed>18, NA, ed)
mother_education <- ifelse(momed>18, NA, ed)
father_education <- ifelse(fathed>18, NA, ed)

walk <- walk
exercise <- exer

smokenow <- ifelse(smokenow>2, NA, smokenow)
tense <- ifelse(tense>7, NA, tense)
angry <- ifelse(angry>7, NA, angry)

year_born <- ifelse(yearbn<90, 1900 + yearbn, 1800 + yearbn)
age <- 1990 - year_born

# Earnings in thousands dollars
earnk <- earn/1000

# Put the cleaned data together and write into a file

earnings_data <- data.frame(height, weight, male, earn, earnk, ethnicity, education,
  mother_education, father_education, walk, exercise, smokenow, tense, angry, age)
# remove rows with missing height and earn
ok <- !is.na(height + earn)
earnings_clean <- earnings_data[ok,]

write.csv(earnings_clean, root("Earnings/data","earnings.csv"), row.names=FALSE)
