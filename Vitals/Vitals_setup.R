setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/Vitals")

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
height_feet <- read.columns("wfw90.dat", 144)
height_inches <- read.columns("wfw90.dat", 145:146)
weight <- read.columns("wfw90.dat", 147:149)
income_exact <- read.columns("wfw90.dat", 203:208)
income_approx <- read.columns("wfw90.dat", 209:210)
sex <- read.columns("wfw90.dat", 219)
race <- read.columns("wfw90.dat", 198)
hisp <- read.columns("wfw90.dat", 200)
ed <- read.columns("wfw90.dat", 190:191)
momed <- read.columns("wfw90.dat", 192:193)
fathed <- read.columns("wfw90.dat", 194:195)
walk <- read.columns("wfw90.dat", 139:140)
exer <- read.columns("wfw90.dat", 142)
smokenow <- read.columns("wfw90.dat", 150)
tense <- read.columns("wfw90.dat", 133)
angry <- read.columns("wfw90.dat", 133)

# Take a look and fix up

table(height_feet, height_inches)
table(weight)
table(sex)
height_inches[height_inches>11] <- NA
height_feet[height_feet>=7] <- NA
height <- 12*height_feet + height_inches
female <- sex - 1
print(mean(income_exact[income_exact>100000],na.rm=TRUE))

income_approx[income_approx>=90] <- NA
income_approx[income_approx==1] <- 150
income <- ifelse(is.na(income_exact), 1000*income_approx, income_exact)

ethnicity <- ifelse(hisp==1, "hispanic",
               ifelse(race==1, "white",
                 ifelse(race==2, "black",
                   "other")))

education <- ifelse(ed>18, NA, ed)
mother_education <- ifelse(momed>18, NA, ed)
father_education <- ifelse(fathed>18, NA, ed)

walk <- walk
exercise <- exer

smokenow <- ifelse(smokenow>2, NA, smokenow)
tense <- ifelse(tense>7, NA, tense)
angry <- ifelse(angry>7, NA, angry)

# Put the cleaned data together and write into a file

height_weight_data <- cbind(height, weight, female, income, ethnicity, education,
  mother_education, father_education, walk, exercise, smokenow, tense, angry)
write.csv(height_weight_data, "vitals.csv")
