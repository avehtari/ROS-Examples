#' ---
#' title: "Regression and Other Stories: Imputation"
#' author: "Andrew Gelman, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Prepare data set for imputation example
#' 
#' -------------
#' 

#' **Load libraries**
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library("rprojroot")
root<-has_dirname("ROS-Examples")$make_fix_file()

#' Read in data from wave 3 of the Social Indicators Survey
wave3 <- read.table(root("Imputation/data","siswave3v4impute3.csv"), header=TRUE, sep=",")
n <- nrow(wave3)

#' Helpful little functions
random_imp <- function(a) {
  missing <- is.na(a)
  n_missing <- sum(missing)
  a_obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample(a_obs, n_missing)
  return(imputed)
}
topcode <- function(a, top) {
  return(ifelse(a > top, top, a))
}

# Missing codes:  -9: refused/dk to say if you have this source
#                 -5: you said you had it but refused/dk the amount

# Earnings variables:

# rearn:  respondent's earnings
# tearn:  spouse's earnings
# pearn:  earnings of primary wage-earner in family
# searn:  earnings of secondary wage-earner in family
# unemp:  unemployment, veteran's benefits, worker comp for entire family
# socsec:  social security of entire family
# pension:  pension, retirement income of entire family
# interest:  interest of entire family
# income:  rental income of entire family
# alimony:  alimony child support for entire family
# giftmon:  gift income for entire family
# ssi:  ssi for entire family
# welfare:  public assistance for entire family
# charity:  income received from charity for entire family

# Demographics:

# earners:  #earners in family (0,1,2)
# sex:  male=1, female=2
# race of respondent:  1=white, 2=black, 3=hispanic(nonblack), 4=other
# famchild:  #kids (0-8)
# anych6:  any children under 6?
# immig:  0 if respondent is U.S. citizen, 1 if not
# rdisab:  is respondent disabled?
# cdisab:  is focal child disabled?
# educ_r:  respondent's education (1=no hs, 2=hs, 3=some coll, 4=college grad)
# r_age:  respondent's age (18-97)
# DON'T USE primary:  -9=missing, 0=spouse, 1=respondent is primary earner
# marrcoh:  0=single, 1=married/cohabitating
# workmos:  primary earner's months worked last year
# workhrs:  primary earner's hours/week worked last year
white <- ifelse(wave3$race == 1, 1, 0)
white[is.na(wave3$race)] <- 0
male <- ifelse(wave3$sex == 1, 1, 0)
over65 <- ifelse(wave3$r_age > 65, 1, 0)
immig <- ifelse(is.na(wave3$immig), 0, wave3$immig)
educ_r <- ifelse(is.na(wave3$educ_r), 2.5, wave3$educ_r)
earners <- ifelse(is.na(wave3$earners), 1, wave3$earners)
no_earners <- ifelse(earners == 0, 1, 0)
workhrs <- wave3$workhrs
workhrs_top <- topcode(workhrs, 40)

#' Set up some simplified variables to work with
na_fix <- function(a) {
  ifelse(a < 0 | a == 999999, NA, a)
}

is_any <- function(a) {
  any_a <- ifelse(a > 0, 1, 0)
  any_a[is.na(a)] <- 0
  return(any_a)
}

workmos <- wave3$workmos
earnings <- na_fix(wave3$rearn) + na_fix(wave3$tearn)
earnings_orig <- earnings
earnings[workmos == 0] <- 0
retirement <- na_fix(wave3$socsec) + na_fix(wave3$pension)
interest <- na_fix(wave3$interest)
assistance <-
  na_fix(wave3$unemp) + na_fix(wave3$ssi) + na_fix(wave3$welfare) + na_fix(wave3$charity)
other <- na_fix(wave3$alimony) + na_fix(wave3$giftmon)

#' Summary variables for various income supports
any_unemp <- is_any(wave3$unemp)
any_ssi <- is_any(wave3$ssi)
any_welfare <- is_any(wave3$welfare)
any_charity <- is_any(wave3$charity)

#' Transforming and topcoding the different sources of income
earnings <- earnings/1000
retirement <- retirement/1000
interest <- interest/1000
assistance <- assistance/1000
other <- other/1000
earnings_top <- topcode(earnings, 100)
retirement_top <- topcode(retirement, 100)
interest_top <- topcode(interest, 100)
assistance_top <- topcode(assistance, 10)
other_top <- topcode(other, 10)

SIS <- data.frame(earnings, retirement, interest, assistance, other, male, over65, white, immig, educ_r, workmos, workhrs_top, any_ssi, any_welfare, any_charity)
write.csv(SIS, root("Imputation/data","SIS.csv"), row.names=FALSE)
