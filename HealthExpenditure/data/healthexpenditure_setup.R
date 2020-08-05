library("arm")
library("rprojroot")
root<-has_dirname("ROS-Examples")$make_fix_file()

read.page <- function (datapage){
  variables.keep <- paste ("X", 1960:2007, sep="")
  data <- read.csv (datapage, skip=3)
  data <- data[1:30,]           # file has only 30 rows of data
  countries <- as.character (data[,"X"])
  numbers <- data[,variables.keep]
  n <- length (countries)
  recent.data <- rep (NA, n)
  for (i in 1:n) {
    y <- as.numeric (numbers[i,])
    ok <- !is.na(y)
    if (sum(ok)>0) {
      years.keep <- (1:length(y))[ok]
      recent.data[i] <- y[max(years.keep)]
    }
    else {
      recent.data[i] <- NA
    }
  }
  return (list (countries=countries, recent.data=recent.data))
}

expend <- read.page(root("HealthExpenditure/data","healthexpenditure.csv"))
life <- read.page(root("HealthExpenditure/data","lifeexpectancy.csv"))
doctor <- read.page(root("HealthExpenditure/data","doctorvisits.csv"))

countries <- expend$countries
countries[countries=="Czech Republic"] <- "Czech"
countries[countries=="New Zealand"] <- "N.Zealand"
countries[countries=="Slovak Republic"] <- "Slovakia"
countries[countries=="United Kingdom"] <- "UK"
countries[countries=="United States"] <- "USA"

expend <- expend$recent.data
life <- life$recent.data
doctor <- doctor$recent.data

healthdata <- data.frame(country=countries, spending=expend, lifespan=life)
write.table(healthdata, root("HealthExpenditure/data","healthdata.txt"), row.name=FALSE)
