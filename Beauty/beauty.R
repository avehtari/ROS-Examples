#' ---
#' title: "Regression and Other Stories: Beauty and Teaching Quality"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Hamermesh and Parker (2005) data on student evaluations of
#' instructors’ beauty and teaching quality for several courses at the
#' University of Texas.
#' 
#' -------------
#' 

#' **Load libraries**
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library("rprojroot")
root<-has_dirname("RAOS-Examples")$make_fix_file()
library("rstanarm")
options(mc.cores = parallel::detectCores())
library("ggplot2")
library("bayesplot")
theme_set(bayesplot::theme_default(base_family = "sans"))

#' The data came originally as an Excel file (ProfEvaltnsBeautyPublic.xls)
#' which was saved to a .csv file (comma-separated values).<br/>
#' Read the data into R, including the variable names (headers)
data <- read.csv(root("Beauty/data","ProfEvaltnsBeautyPublic.csv"), header=TRUE)
# Rename the two variables for convenience
data$beauty <- data$btystdave
data$eval <- data$courseevaluation

#' ### Do more beautiful profs get higher evaluations?
#'

#' **Make a scatterplot of data**
par(mar=c(3,3,1,1), mgp=c(1.7, .5, 0), tck=-.01)
plot(data$beauty, data$eval)

#' **Fit a linear regression**
#+ results='hide'
fit_1 <- stan_glm(eval ~ beauty, data=data)
#+
print(fit_1, digits=2)

#' **Make a scatterplot with regression lines**
# Labeling the axes
plot(data$beauty, data$eval, xlab="Beauty", ylab="Average teaching evaluation")
# Display the regression line, added onto the scatterplot (add=TRUE)
coefs <- coef(fit_1)
curve(coefs[1] + coefs[2]*x, add=TRUE)
# Add dotted lines to show +/- 1 standard deviation
sigma <- sigma(fit_1)
curve(coefs[1] + coefs[2]*x + sigma, lty=2, add=TRUE)
curve(coefs[1] + coefs[2]*x - sigma, lty=2, add=TRUE)

#' ggplot version
ggplot(data, aes(beauty, eval)) +
  geom_point(size = 2, alpha = 0.75) +
  geom_abline(
    slope = rep(coefs[2], 3),
    intercept = c(coefs[1], coefs[1] - sigma, coefs[1] + sigma),
    linetype = c(1, 2, 2),
    color = "darkgray",
    size = 1
  ) +
  labs(
    x = "Beauty",
    y = "Average teaching evaluation"
  )

#' ### Do things differ for male and female profs?  

#' Parallel regression lines
#+ results='hide'
fit_2 <- stan_glm(eval ~ beauty + female, data=data)
#+
print(fit_2, digits=2)
coefs2 <- coef(fit_2)

#' **Make several subplots**
# Set up a 2x2 grid of plots
par(mfrow=c(2,2))
# Make separate plot for men, ...
plot(data$beauty[data$female==0], data$eval[data$female==0], xlim=range(data$beauty), ylim=range(data$eval),
     xlab="Beauty", ylab="Average teaching evaluation", main="Men")
curve(coefs2[1] + coefs2[2]*x + coefs2[3]*0, add=TRUE)
# ... women, ...
plot(data$beauty[data$female==1], data$eval[data$female==1], xlim=range(data$beauty), ylim=range(data$eval),
      xlab="Beauty", ylab="Average teaching evaluation", main="Women")
curve(coefs2[1] + coefs2[2]*x + coefs2[3]*1, add=TRUE)
# ... and both sexes on the same plot
# First make the plot with type="n" (which displays axes but does not plot
#   the points), then plot the points and lines separately for each sex
plot(data$beauty, data$eval, xlab="Beauty", ylab="Average teaching evaluation",
      main="Both sexes", type="n")
points(data$beauty[data$female==0], data$eval[data$female==0], col="blue")
curve(coefs2[1] + coefs2[2]*x + coefs2[3]*0, add=TRUE, col="blue")
points(data$beauty[data$female==1], data$eval[data$female==1], col="red")
curve(coefs2[1] + coefs2[2]*x + coefs2[3]*1, add=TRUE, col="red")

#' ggplot versions
# Men 
gg_male <-
  ggplot(subset(data, female == 0), aes(beauty, eval)) +
  geom_point() +
  geom_abline(slope = coefs2[2], intercept = coefs2[1], color = "darkgray")
# Women
gg_female <-
  ggplot(subset(data, female == 1), aes(beauty, eval)) +
  geom_point() +
  geom_abline(slope = coefs2[2], intercept = coefs2[1] + coefs2[3], color = "darkgray")
# Both
gg_both <-
  ggplot(data, aes(beauty, eval)) +
  geom_point(aes(color = factor(female)), show.legend = FALSE) +
  scale_color_manual(values = c("red", "blue")) +
  geom_abline(
    slope = coefs2[2],
    intercept = c(coefs2[1], coefs2[1] + coefs2[3]),
    color = c("blue3", "red3"),
    size = 1
  )
# Put them in a grid
bayesplot_grid(
  gg_male, gg_female, gg_both,
  grid_args = list(ncol = 2),
  xlim = range(data$beauty),
  ylim = range(data$eval),
  titles = c("Men", "Women", "Both sexes")
)

#' ### Do things differ for male and female profs?  

#' **Non-parallel regression lines**
#+ results='hide'
fit_3 <- stan_glm(eval ~ beauty + female + beauty*female, data=data)
#+
print(fit_3, digits=2)
coefs3 <- coef(fit_3)

#' **Make two subplots**
# Set up a new 1x2 grid of plots
par(mfrow=c(1,2))
# Display the parallel regression lines in gray and the non-parallel lines
# in heavy black
# Make separate plot for men ...
plot(data$beauty[data$female==0], data$eval[data$female==0], xlim=range(data$beauty), ylim=range(data$eval),
      xlab="Beauty", ylab="Average teaching evaluation", main="Men")
curve(coefs2[1] + coefs2[2]*x + coefs2[3]*0,
       lwd=.5, col="gray", add=TRUE)
curve(coefs3[1] + coefs3[2]*x + coefs3[3]*0 + coefs3[4]*x*0,
       lwd=2, col="black", add=TRUE)
# ... and women
plot (data$beauty[data$female==1], data$eval[data$female==1], xlim=range(data$beauty), ylim=range(data$eval),
      xlab="Beauty", ylab="Average teaching evaluation", main="Women")
curve(coefs2[1] +coefs2[2]*x +coefs2[3]*1,
       lwd=.5, col="gray", add=TRUE)
curve(coefs3[1] + coefs3[2]*x + coefs3[3]*1 +coefs3[4]*x*1,
       lwd=2, col="black", add=TRUE)

#' ggplot version
# we can add to the gg_male and gg_female plots we already made above
gg_male2 <- gg_male + geom_abline(intercept = coefs3[1], slope = coefs3[2], size = 1)
gg_female2 <- gg_female + geom_abline(intercept = coefs3[1] + coefs3[3], slope = coefs3[2] + coefs3[4], size = 1)
# Put them in a grid
bayesplot_grid(
  gg_male2, gg_female2,
  grid_args = list(ncol = 2),
  xlim = range(data$beauty),
  ylim = range(data$eval),
  titles = c("Men", "Women")
)

#' ### More models

#' **Add age**
#+ results='hide'
fit_4 <- stan_glm(eval ~ beauty + female + age, data=data)
#+
print(fit_4, digits=2)

#' **Add minority**
#+ results='hide'
fit_5 <- stan_glm(eval ~ beauty + female + minority, data=data)
#+
print(fit_5, digits=2)

#' **Add nonenglish**
#+ results='hide'
fit_6 <- stan_glm(eval ~ beauty + female + nonenglish, data=data)
#+
print(fit_6, digits=2)

#' **Add nonenglish and lower**
#+ results='hide'
fit_7 <- stan_glm(eval ~ beauty + female + nonenglish + lower, data=data)
#+
print(fit_7, digits=2)

#' ### Go back to simple model, add course indicators

#' Create the course index variable
courses <- data[, 18:47]   # (indicators for the 30 courses)
n <- nrow(data)
J <- ncol(courses) + 1
course_id <- rep(0, n)
for (i in 1:n){
  for (j in 1:30){
    if (courses[i,j]==1) course_id[i] <- j
  }
}

#' **Include course indicators in a regression**
#+ results='hide'
fit_8 <- stan_glm(eval ~ beauty + factor(course_id), data=data)
#+
print(fit_8, digits=2)
