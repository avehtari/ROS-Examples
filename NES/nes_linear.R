# national election study data

setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/NES")

# read in and clean the data

library(foreign)
brdata <- read.dta("nes5200_processed_voters_realideo.dta",convert.factors=FALSE)
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
nes_year <- data[,"year"]
age_discrete <- as.numeric(cut (data[,"age"], c(0, 29.5, 44.5, 64.5, 200)))
race_adj <- ifelse(data[,"race"]>=3, 1.5, data[,"race"])
dvote <- ifelse(data$presvote==1, 1, 0)
rvote <- ifelse(data$presvote==2, 1, 0)
data <- cbind(data, age_discrete, race_adj, dvote, rvote)

nes <- data.frame(data)
write.table(nes, "nes.dat")

female <- data[,"gender"] - 1
black <- ifelse (data[,"race"]==2, 1, 0)
rvote <- ifelse (data[,"presvote"]==1, 0, ifelse(data[,"presvote"]==2, 1, NA))

region_codes <- c(3,4,4,3,4,4,1,1,5,3,3,4,4,2,2,2,2,3,3,1,1,1,2,2,3,2,4,2,4,1,1,4,1,3,2,2,3,4,1,1,3,2,3,3,4,1,3,4,1,2,4)

# partyid model to illustrate secret weapon in linear1 chapter

regress_year <- function (yr) {
  this_year <- data[nes_year==yr,]
  lm_0 <- lm(partyid7 ~ real_ideo + race_adj + factor(age_discrete) + educ1 + female + income, data=this_year)
  coefs <- summary(lm_0)$coef[,1:2]
}

summary <- array (NA, c(9,2,8))
for (yr in seq(1972,2000,4)){
  i <- (yr-1968)/4
  summary[,,i] <- regress_year(yr)
}
yrs <- seq(1972,2000,4)

coef_names <- c("Intercept", "Ideology", "Black", "Age_30_44", "Age_45_64", "Age_65_up", "Education", "Female", "Income")

pdf("partyid_1.pdf", height=2.5, width=7.5)
par(mfrow=c(2,5), mar=c(2,3,2,2), tck=-.02, mgp=c(2,.7,0))
for (k in 1:9){
  plot(range(yrs), range(0,summary[k,1,]+.67*summary[k,2,],summary[k,1,]-.67*summary[k,2,]), type="n", xlab="", ylab="Coefficient", main=coef_names[k], mgp=c(1.2,.2,0), cex.main=1, cex.axis=1, cex.lab=1, tcl=-.1, bty="l", xaxt="n")
  axis(1, c(1972,1986,2000), mgp=c(.5,.3,0))
  abline(0,0, lty=2)
  points(yrs, summary[k,1,], pch=20)
  segments(yrs, summary[k,1,]-.67*summary[k,2,], yrs, summary[k,1,]+.67*summary[k,2,])
}
dev.off()

