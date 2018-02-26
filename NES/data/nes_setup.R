
library("here")
library("foreign")

#' **Read in and clean the data**
brdata <- read.dta(here("NES/data","nes5200_processed_voters_realideo.dta"),convert.factors=FALSE)
brdata <- brdata[is.na(brdata$black)==FALSE&is.na(brdata$female)==FALSE&is.na(brdata$educ1)==FALSE
&is.na(brdata$age)==FALSE&is.na(brdata$income)==FALSE&is.na(brdata$state)==FALSE,]
kept_cases <- 1952:2000
matched_cases <- match(brdata$year, kept_cases)
keep <- !is.na(matched_cases)
data <- brdata[keep,]
plotyear <- unique(sort(data$year))
year_new <- match(data$year,unique(data$year))
n_year <- length(unique(data$year))
income_new <-data$income - 3
age_new <- (data$age - mean(data$age))/10
vote <- data$rep_pres_intent
data <- cbind(data, year_new, income_new, age_new, vote)
age_discrete <- as.numeric(cut (data[,"age"], c(0, 29.5, 44.5, 64.5, 200)))
race_adj <- ifelse(data[,"race"]>=3, 1.5, data[,"race"])
dvote <- ifelse(data$presvote==1, 1, 0)
rvote <- ifelse(data$presvote==2, 1, 0)
data <- cbind(data, age_discrete, race_adj, dvote, rvote)

female <- data[,"gender"] - 1
black <- ifelse (data[,"race"]==2, 1, 0)
rvote <- ifelse (data[,"presvote"]==1, 0, ifelse(data[,"presvote"]==2, 1, NA))

region_codes <- c(3,4,4,3,4,4,1,1,5,3,3,4,4,2,2,2,2,3,3,1,1,1,2,2,3,2,4,2,4,1,1,4,1,3,2,2,3,4,1,1,3,2,3,3,4,1,3,4,1,2,4)

nes <- data.frame(data)
write.table(nes, "nes.dat")
