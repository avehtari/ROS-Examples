# Regression and Other Stories - Data and code

Regression and Other Stories by [Andrew Gelman](http://www.stat.columbia.edu/~gelman/), [Jennifer Hill](https://steinhardt.nyu.edu/people/jennifer-hill), and [Aki Vehtari](https://users.aalto.fi/~ave/) (2020)

[Regression and Other Stories book home page](http://www.stat.columbia.edu/~gelman/regression/)

This git repository has data and code for the examples and exercises in the book. If you find any errors in code please make an issue or email [Aki.Vehtari@aalto.fi](mailto:Aki.Vehtari@aalto.fi).

## Data as `rosdata` R package

For an easier use of data, there is an R package called `rosdata`. You can install it with a command
```
remotes::install_github("avehtari/ROS-Examples",subdir = "rpackage")
```
Then you can access data, for example, as
```
library(rosdata)
data(wells)
head(wells)
```
You can get the list of data sets with
```
?rosdata
```

## Index and notebooks in html

For easier viewing the index and the notebooks in html are available at [avehtari.github.io/ROS-Examples](https://avehtari.github.io/ROS-Examples/).

## Running the notebooks and `rprojroot`

As there are many examples, to avoid need to switch the working directory the notebooks use `rprojroot` package to set the project root directory and then accessing the data files in relation to that. This means that the downloaded git repository directory can be placed anywhere you like and you can rename the ROS-Examples directory as you like. When running the code, it is sufficient that the working directory is any directory in the ROS-Examples (or renamed). Running
```
library("rprojroot")
root<-has_file(".ROS-Examples-root")$make_fix_file()
```
will find the file `.ROS-Examples-root` which is in the `ROS-Examples` directory, and will set the full path according to that. Then, for example,
```
wells <- read.csv(root("Arsenic/data","wells.csv"))
```
finds the `wells.csv` file, no matter where you have placed or renamed the `ROS-Examples` directory. When you switch to another example, there is no need to switch the working directory.

