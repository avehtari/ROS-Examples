###############################################################################
# Read in the subset data and get set up for modeling
###############################################################################

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

apt.subset.data <- read.table("apt.subset.dat", header=TRUE)
attach.overwrite(apt.subset.data)
y <- y.subset
defects <- defects.subset
poor <- poor.subset
race <- race.subset
floor <- floor.subset
dist <- dist.subset
bldg <- bldg.subset

asian <- race==5 | race==6 | race==7
black <- race==2
hisp <- race==3 | race==4

n <- length(y)
n.bldg <- max(bldg)
n.dist <- max(dist)

bldg.subset.data <- read.table("bldg.subset.dat", header=TRUE)
attach.all(bldg.subset.data)

dist.data <- read.table("dist.dat", header=TRUE)
attach.all(dist.data)
