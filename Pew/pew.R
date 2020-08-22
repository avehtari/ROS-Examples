#' ---
#' title: "Regression and Other Stories: Pew"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     theme: readable
#'     toc: true
#'     toc_depth: 2
#'     toc_float: true
#'     code_download: true
#' ---

#' Miscellaneous analyses using raw Pew data. See Chapter 2 in
#' Regression and Other Stories.
#' 
#' -------------
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(message=FALSE, error=FALSE, warning=FALSE, comment=NA)
# switch this to TRUE to save figures in separate files
savefigs <- FALSE

#' #### Load packages
library("rprojroot")
root<-has_file(".ROS-Examples-root")$make_fix_file()
library("foreign")

#' #### Load data
pew_pre <- read.dta(root("Pew/data","pew_research_center_june_elect_wknd_data.dta"))
n <- nrow(pew_pre)

#' #### Glance data
table(pew_pre[,"date"])
which_question <- ifelse(!is.na(pew_pre$heat2), 2, ifelse (!is.na(pew_pre$heat4), 4, 0))
table(pew_pre$date, which_question)

#' #### Create vote intention variable "rvote" using variables heat2 and heat4 from Pew
numeric_heat2 <- as.numeric(pew_pre$heat2)
numeric_heat4 <- as.numeric(pew_pre$heat4)
rvote <- rep (NA, n)
for (i in 1:n){
  if (which_question[i]==2){
    rvote[i] <- ifelse(numeric_heat2[i]==1, 1,
                 ifelse(numeric_heat2[i]==2, 0, NA))
  }
  else if (which_question[i]==4){
    rvote[i] <- ifelse(numeric_heat4[i]==1, 1,
                 ifelse(numeric_heat4[i]==2, 0, NA))
  }
}

#' #### Certain to have registered to vote?
registered <- ifelse(pew_pre$regicert=="absolutely certain", 1, 0)
registered[is.na(registered)] <- 0

#' Date
early <- pew_pre$date < 90008
late <- pew_pre$date > 90008
month <- floor(pew_pre$date/10000)
day <- floor(pew_pre$date/100) - 100*month
day.numeric <- month*31 + day
poll.id <- ifelse (month==6, 1,
             ifelse (month==7 & day<28, 2,
               ifelse ((month==7 & day>=28) | month==8, 3,
                 ifelse (month==9 & day<16, 4,
                   ifelse (month==9 & day>=16, 5,
                     ifelse (month==10 & day<14, 6,
                       ifelse (month==10 & day>=14 & day<20, 7,
                         ifelse (month==10 & day>=20 & day<27, 8, 9))))))))
n.poll.id <- max(poll.id)

#' State (1-51, in alfa order, including DC)
stnum <- as.numeric(pew_pre$state)

#' Identify out DC
state.abb.long <- c(state.abb[1:8], "DC", state.abb[9:50])
dc <- state.abb.long[stnum]=="DC"

#' Votes
votes08 <- c(60,39, 62,36, 54,45, 58,39, 40,59, 46,53, 39,60, 37,62, 7,93, 49,51, 53,46, 25,74, 61,36, 38,61, 49,50, 45,54, 57,42, 58,41, 59,40, 40,58, 39,60, 36,62, 42,56, 44,54, 57,43, 49,49, 50,47, 57,41, 41,57, 44,55, 42,57, 42,57, 37,62, 49,50, 53,45, 47,51, 66,34, 42,56, 44,55, 35,63, 54,45, 54,44, 57,42, 55,44, 62,35, 32,67, 48,52, 43,56, 56,43, 43,56, 65,33)
obama08 <- votes08[seq(2,102,2)]
mccain08 <- votes08[seq(1,101,2)]
ovote.actual <- obama08/(obama08+mccain08)
stnum <- as.numeric(pew_pre$state)

#' Weight
pop.weight0 <- pew_pre$weight
voter.weight0 <- ifelse(is.na(rvote) | registered==0, NA, pop.weight0)
pop.weight <- rep(NA, length(pop.weight0))
voter.weight <- rep(NA, length(voter.weight0))
for (i in 1:n.poll.id){
  ok <- poll.id==i
  pop.weight[ok] <- pop.weight0[ok]/mean(pop.weight0[ok])
  voter.weight[ok] <- voter.weight0[ok]/mean(voter.weight0[ok],na.rm=TRUE)
}
voter.weight1 <- rep(1, length(voter.weight0))
for (i in 1:51){
  ok <- stnum==i
  if (sum(ok)>10){
    sum.d <- sum(voter.weight[ok & rvote==0], na.rm=TRUE)
    sum.r <- sum(voter.weight[ok & rvote==1], na.rm=TRUE)
    ovote.sample <- sum.d/(sum.d+sum.r)
    voter.weight1[ok & rvote==0] <- ovote.actual[i]/ovote.sample
    voter.weight1[ok & rvote==1] <- (1-ovote.actual[i])/(1-ovote.sample)
  }
}
voter.weight2 <- voter.weight*voter.weight1

#' Income (1-9 scale)
inc <- as.numeric(pew_pre$income)
inc[inc==10] <- NA
value.inc <- c(5,15,25,35,45,62.5,87.5,125,200)
n.inc <- max(inc, na.rm=TRUE)

#' Party id
pid0 <- as.numeric(pew_pre[,"party"])
lean0 <- as.numeric(pew_pre[,"partyln"])
pid <- ifelse(pid0==2, 5,  # Republican
         ifelse(pid0==3, 1,  # Democrat
         ifelse(lean0==2, 4, # Lean Republican
         ifelse(lean0==4, 2, # Lean Democrat
         3)))) # Independent
#1=Dem, 2=Lean Dem, 2=Ind, 4=Lean Rep, 5=Repub
pid.label <- c("Democrat", "Lean Dem.", "Independent", "Lean Rep.", "Republican")
n.pid <- max(pid, na.rm=TRUE)

#' Ideology
ideology0 <- as.numeric(pew_pre[,"ideo"])
ideology <- ifelse(ideology0==2, 5, # Very conservative
              ifelse(ideology0==3, 4, # Conservative
              ifelse(ideology0==6, 1, # Very liberal
              ifelse(ideology0==5, 2, # Liberal
              3)))) # Moderate
ideology.label <- c("Very liberal", "Liberal", "Moderate", "Conservative", "Very conservative")
n.ideology <- max(ideology, na.rm=TRUE)

#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Pew/figs","pid.pdf"), height=4.5, width=5.5, colormodel="gray")
#+
par(mar=c(3,2,2.5,1), mgp=c(1.5,.7,0), tck=-.01)
plot(c(1,n.inc), c(0,1), xaxs="i", yaxs="i", type="n", xlab="", ylab="", xaxt="n", yaxt="n")
axis(1, 1:n.inc, rep("",n.inc))
axis(1, seq(1.5,n.inc-.5,length=3), c("Low income", "Middle income", "High income"), tck=0)
axis(2, c(0,.5,1), c(0,"50%","100%"))
center <- floor((1+n.inc)/2)
incprop <- array(NA, c(n.pid+1,n.inc))
incprop[n.pid+1,] <- 1
for (i in 1:n.pid){
  for (j in 1:n.inc){
    incprop[i,j] <- weighted.mean((pid<i)[inc==j], pop.weight0[inc==j], na.rm=TRUE)
  }
}
for (i in 1:n.pid){
  polygon(c(1:n.inc, n.inc:1), c(incprop[i,], rev(incprop[i+1,])), col=paste("gray", 40+10*i, sep=""))
  lines(1:n.inc, incprop[i,])
  text(center, mean(incprop[c(i,i+1),center]), pid.label[i])
}
mtext("Self-declared party identification, by income", side=3, line=1, cex=1.2)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Pew/figs","ideology.pdf"), height=4.5, width=5.5, colormodel="gray")
#+
par(mar=c(3,2,2.5,1), mgp=c(1.5,.7,0), tck=-.01)
plot(c(1,n.inc), c(0,1), xaxs="i", yaxs="i", type="n", xlab="", ylab="", xaxt="n", yaxt="n")
axis(1, 1:n.inc, rep("",n.inc))
axis(1, seq(1.5,n.inc-.5,length=3), c("Low income", "Middle income", "High income"), tck=0)
axis(2, c(0,.5,1), c(0,"50%","100%"))
center <- floor((1+n.inc)/2)
incprop <- array(NA, c(n.ideology+1,n.inc))
incprop[n.ideology+1,] <- 1
for (i in 1:n.ideology){
  for (j in 1:n.inc){
    incprop[i,j] <- weighted.mean((ideology<i)[inc==j], pop.weight0[inc==j], na.rm=TRUE)
  }
}
for (i in 1:n.ideology){
  polygon(c(1:n.inc, n.inc:1), c(incprop[i,], rev(incprop[i+1,])), col=paste("gray", 40+10*i, sep=""))
  lines(1:n.inc, incprop[i,])
  text(center, mean(incprop[c(i,i+1),center]), ideology.label[i])
}
mtext("Self-declared political ideology, by income", side=3, line=1, cex=1.2)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' Recoded religious attendance (relatt) to 1-5 scale
relatt <- 7 - as.numeric(pew_pre$attend)
relatt[relatt==0] <- NA
relatt <- ifelse(relatt==1|relatt==2, 1,
            ifelse(relatt==3, 2,
              ifelse(relatt==4, 3,
                ifelse(relatt==5, 4,
                  ifelse(relatt==6, 5, NA)))))
n.relatt <- max(relatt, na.rm=TRUE)
relatt.label <- c("Nonattenders","Rare attenders","Occasional\nattenders","Frequent attenders","Very frequent\nchurch attenders")

#' Categories
pid2 <- ifelse(pid==1, 1, ifelse(pid==5, 3, 2))
ideology2 <- ifelse(ideology==1|ideology==2, 1, ifelse(ideology==3, 2, 3))
pid2.label <- c("Democrats", "Independents", "Republicans")
ideology2.label <- c("Liberal", "Moderate", "Conservative")
prop <- array(NA, c(3,3,n.inc))
for (i in 1:3){
  for (j in 1:3){
     for (k in 1:n.inc){
       prop[i,j,k] <- weighted.mean(pid2==i & ideology2==j & inc==k, pop.weight, na.rm=TRUE)
     }
   }
}

#+ eval=FALSE, include=FALSE
png("pidideology.png", height=500, width=450)
#+
par(mfrow=c(3,3), oma=c(0,0,2.5,0))
par(mar=c(2,2,4,0), mgp=c(1.5,.7,0), tck=-.01)
for (i in 1:3){
  for (j in 1:3){
      plot(1:n.inc, prop[i,j,], type="l", bty="l", xaxs="i", yaxs="i", ylim=c(0,max(prop)),
           xaxt="n", yaxt="n", xlab="", ylab="")
    axis(1, 1:n.inc, rep("",n.inc))
    axis(1, seq(2.7,n.inc-1.7,length=2), c("Low income", "High income"), tck=0, cex.axis=1.2)
    axis(2, c(0,.01,.02), c("0","1%","2%"), cex.axis=1.2)
    mtext(paste(ideology2.label[j], pid2.label[i]), side=3, line=1, cex=.9)
  }
}
mtext("Income distributions within self-reported political categories in 2008",
      outer=TRUE, side=3, line=.5, cex=1.2)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
