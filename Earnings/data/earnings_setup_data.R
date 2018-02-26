
library("here")
library("foreign")

#' Load original data
earnings <- read.dta(here("Earnings/data","earnings.dta"))

#' Create variables for age and ethnicity categories
earnings$age <- 90 -  earnings$yearbn # survey was conducted in 1990
earnings$age[earnings$age < 18] <- NA
earnings$age_category <- with(earnings, ifelse(age < 35, 1, ifelse(age < 50, 2, 3)))
earnings$eth <- with(earnings, ifelse(race==2, 1, ifelse(hisp==1, 2, ifelse(race==1, 3, 4))))
earnings$male <- 2 - earnings$sex

#' (For simplicity) remove cases with missing data and restrict
#' to people born after 1925
ok <- with(earnings, !is.na(earn + height + sex + age) & yearbn > 2)
earnings_clean <- earnings[ok,]
write.csv(earnings_clean, here("Earnings/data","earnings.csv"))
