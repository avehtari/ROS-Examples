setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/SIS")

## Read in data from wave 3 of the Social Indicators Survey

attach.overwrite <- function(a, overwrite=TRUE, name="attach.all"){
#
# Version of the "attach" function that overwrites variables if necessary.
# (The usual "attach" function in R only attaches the variables that are not
# yet defined.)
#
  if (overwrite){
    for (j in 1:length(a)){
      if (names(a)[j] %in% ls(.GlobalEnv))
        remove(list=names(a)[j], envir=.GlobalEnv)
    }
  }
  attach(a, name=name)
}

wave3 <- read.table("siswave3v4impute3.csv", header=TRUE, sep=",")
attach.overwrite(wave3)
n <- nrow(wave3)

# Helpful little functions

random_imp <- function (a){
  missing <- is.na(a)
  n_missing <- sum(missing)
  a_obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample(a_obs, n_missing)
  return(imputed)
}

topcode <- function (a, top){
  return(ifelse(a>top, top, a))
}

# missing codes:  -9: refused/dk to say if you have this source
#                 -5: you said you had it but refused/dk the amount

# earnings variables:

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

# demographics:

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

white <- ifelse(race==1, 1, 0)
white[is.na(race)] <- 0
male <- ifelse(sex==1, 1, 0)
over65 <- ifelse(r_age>65, 1, 0)
immig[is.na(immig)] <- 0
educ_r[is.na(educ_r)] <- 2.5
earners[is.na(earners)] <- 1
no_earners <- ifelse(earners==0, 1, 0)
workhrs_top <- topcode(workhrs, 40)

# set up some simplified variables to work with

na_fix <- function (a) {
  ifelse(a<0 | a==999999, NA, a)
}

is_any <- function (a) {
  any_a <- ifelse(a>0, 1, 0)
  any_a[is.na(a)] <- 0
  return(any_a)
}

workmos <- workmos
earnings <- na_fix(rearn) + na_fix(tearn)
earnings_orig <- earnings
earnings[workmos==0] <- 0
retirement <- na_fix(socsec) + na_fix(pension)
interest <- na_fix(interest)
assistance <- na_fix(unemp) + na_fix(ssi) + na_fix(welfare) + na_fix(charity)
other <- na_fix(alimony) + na_fix(giftmon)

# summary variables for various income supports

any_unemp <- is_any(unemp)
any_ssi <- is_any(ssi)
any_welfare <- is_any(welfare)
any_charity <- is_any(charity)

# transforming and topcoding the different sources of income

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
