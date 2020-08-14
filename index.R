#' ---
#' title: "Regression and Other Stories"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "Page updated: `r format(Sys.Date())`"
#' output:
#'   html_document:
#'     theme: readable
#' ---
#' 

#' # {.tabset}
#' 
#' Home page for the book Regression and Other Stories by [Andrew Gelman](http://www.stat.columbia.edu/~gelman/), [Jennifer Hill](https://steinhardt.nyu.edu/people/jennifer-hill), and [Aki Vehtari](https://users.aalto.fi/~ave/), including the code and data.
#'
#' ## Book information{.tabset}
#'
#'<div style= "float:right;position: relative;">
#' ![](ROS_frontcover.png)
#'</div>
#' 
#' Back cover text: *Many textbooks on regression focus on theory and
#' the simplest of examples. Real statistical problems, however, are
#' complex and subtle. This is not a book about the theory of
#' regression. It is a book about how to use regression to solve real
#' problems of comparison, estimation, prediction, and causal
#' inference. It focuses on practical issues such as sample size and
#' missing data and a wide range of goals and techniques. It jumps
#' right in to methods and computer code you can use fresh out of the
#' box.*
#'
#' The book has been avaibale in Europe since 23 July 2020, and the
#' books to other parts of the world are in transit.
#' 
#' - [Cambridge University Press page](https://www.cambridge.org/fi/academic/subjects/statistics-probability/statistical-theory-and-methods/regression-and-other-stories)
#'
#' - [Podcast: a special episode of Learning Bayesian Statistics featuring the ROS authors (and a discount promo code)](https://www.youtube.com/watch?v=OJyoXJzxTGs) (other podcast sources at [Learning Bayesian Statistics podcast](https://learnbayesstats.anvil.app/))
#' 
#' - [Sample exams](https://github.com/avehtari/ROS-Examples/tree/master/Exams/)
#' - See also an article [Teaching Bayes to Graduate Students in Political Science, Sociology, Public Health, Education, Economics, ...](http://www.stat.columbia.edu/~gelman/research/published/teachingbayes.pdf)
#'
#' If you notice an error that is not mentioned in the errata below, submit an issue at https://github.com/avehtari/ROS-Examples/issues or send an email.
#' 
#' -------------
#'
#' ### Contents
#' 
#' 1. Introduction
#' 2. Data and measurement
#' 3. Some basic methods in mathematics and probability
#' 4. Generative models and statistical inference
#' 5. Simulation
#' 6. Background on regression modeling
#' 7. Linear regression with a single predictor
#' 8. Fitting regression models
#' 9. Prediction and Bayesian inference 
#' 10. Linear regression with multiple predictors
#' 11. Assumptions, diagnostics, and model evaluation
#' 12. Transformations
#' 13. Logistic regression
#' 14. Working with logistic regression
#' 15. Other generalized linear models
#' 16. Design and sample size decisions
#' 17. Poststratification and missing-data imputation
#' 18. Causal inference basics and randomized experiments
#' 19. Causal inference using regression on the treatment variable
#' 21. More advanced topics in causal inference
#' 22. Advanced regression and multilevel models
#'
#' -------------
#'
#' ### Reviews
#'
#' *'Gelman, Hill and Vehtari provide an introductory regression book that hits an amazing trifecta: it motivates regression using real data examples, provides the necessary (but not superfluous) theory, and gives readers tools to implement these methods in their own work. The scope is ambitious - including introductions to causal inference and measurement - and the result is a book that I not only look forward to teaching from, but also keeping around as a reference for my own work.'* Elizabeth Tipton, Northwestern University
#' 
#' *'Regression and Other Stories is simply the best introduction to applied statistics out there. Filled with compelling real-world examples, intuitive explanations, and practical advice, the authors offer a delightfully modern perspective on the subject. It's an essential resource for students and practitioners across the statistical and social sciences.'* Sharad Goel, Department of Management Science and Engineering, Stanford University
#' 
#' *'With modern software it is very easy to fit complex regression models, and even easier to get their interpretation completely wrong. This wonderful book, summarising the authors' years of experience, stays away from mathematical proofs, and instead focuses on the insights to be gained by careful plotting and modelling of data. In particular the chapters on causal modelling, and the challenges of working with selected samples, provide some desperately needed lessons.'* David Spiegelhalter, University of Cambridge
#' 
#' *'Gelman and Hill, have done it again, this time with Aki Vehtari. They have written a textbook that should be on every applied quantitative researcher's bookshelf. Most importantly they explain how to do and interpret regression with real world, complicated examples. Practicing academics in addition to students will benefit from giving this book a close read.'* Christopher Winship, Harvard University, Massachusetts
#'
#' *'Overall this is a very modern, stats centred, engaging and careful book on the most common tool of statistical modelling!'* Christian P. Robert, Ceremade - Université Paris-Dauphine. The full review in a blog post [[The Art of] Regression and other stories](https://xianblog.wordpress.com/2020/07/23/the-art-of-regression-and-other-stories/).
#'
#' ### Errata
#' 
#' Errata (excluding minor typos) for the book.
#'
#' If you notice an error, submit an issue at https://github.com/avehtari/ROS-Examples/issues or send an email.
#' 
#' This list was last changed on 8 August 2020.
#' 
#' 1st (2020) printing
#'
#' - p. 11, the summary of the treatment-control comparison says "the treated units were 4.8 points higher than the controls, \bar{y} = 31.7 under the treatment and \bar{y} = 25.5 for the controls." The difference in shown means is 6.2 and not 4.8.  Not that these values change when rerunning the simulation. (thanks Desislava Petkova) 
#' - p. 39, in the second sentence of the first full paragraph, "Figure 3.4 displays data on log metabolic rate vs. body mass indicating..." should be "log metabolic rate vs. log body mass" (thanks Ravi Shroff)
#' - p. 42, the last line, in "Linear transformations" section "Exercise 3.5" should be "Exercise 3.6". (thanks Ed Berry)
#' - p. 54, in "Comparisons, visual and numerical" subsection, "Figure 4.2" in the first sentence should be "Figure 4.3." (thanks Ravi Shroff)
#' - p. 55 `se_weighted_avg <- sqrt(sum(W*se)^2)` should be `se_weighted_avg <- sqrt(sum((W*se)^2))` (thanks to Desislava Petkova)
#' 
#' -------------
#' 
#' ## Code and data {.tabset}
#'
#'   - The code and data are provided to fully reproduce the examples
#'     and figurs in the book.  They can be a good way to see what the
#'     code does.
#'   - Different people have different styles of code.  The code here
#'     is not supposed to be a model.  The statistical analyses and
#'     graphs in the book are intended to be models for good practice,
#'     but the code here is meant to be simple with minimal dependencies.
#'   - For learning R programming basics we recommend
#'     - [Garrett Grolemund, Hands-On Programming with R](https://rstudio-education.github.io/hopr/)
#'   - For learning basic and advanced plotting using R we recommend
#'     - [Kieran Healy, Data Visualization - A practical introduction](https://socviz.co/)
#'     - [Antony Unwin, Graphical Data Analysis with R](http://www.gradaanwr.net/)
#'   - Further ideas for visualization
#'     - [ggdist: Visualizations of distributions and uncertainty](https://mjskay.github.io/ggdist/)
#'     - [tidybayes: Bayesian analysis + tidy data + geoms](http://mjskay.github.io/tidybayes/)
#'     - [Claus O. Wilke, Fundamentals of Data Visualization](https://serialmentor.com/dataviz/)
#' 
#' -------------
#' 
#' ### Code and data by chapters<a id="by_chapters"></a>
#'
#'   - The folders below (ending /) point to the code (.R and .Rmd)
#'     and `data` folders (.csv or .txt) in github, and .html -files
#'     point to knitted notebooks.
#'   - Most examples have cleaned data in .csv file in `data` subfolder
#'     for easy experimenting. For completeness and reproducibility, the
#'     data subfolders have also the raw data and `*_setup.R` file showing
#'     how the data pre-processing has been done (to do the exercises
#'     and follow along with the examples, you don't need to worry about
#'     the setup code).
#' 
#' -------------
#' 
#' #### 1  Introduction
#'   - [ElectionsEconomy/](https://github.com/avehtari/ROS-Examples/tree/master/ElectionsEconomy/)
#'     - [hibbs.html](ElectionsEconomy/hibbs.html) - Predicting presidential vote share from the economy
#'   - [ElectricCompany/](https://github.com/avehtari/ROS-Examples/tree/master/ElectricCompany/)
#'     - [electric.html](ElectricCompany/electric.html) - Analysis of "Electric company" data
#'   - [Peacekeeping/](https://github.com/avehtari/ROS-Examples/tree/master/Peacekeeping/)
#'     - [peace.html](Peacekeeping/peace.html) - Outcomes after civil war in countries with and without United Nations peacekeeping
#'   - [SimpleCausal/](https://github.com/avehtari/ROS-Examples/tree/master/SimpleCausal/)
#'     - [causal.html](SimpleCausal/causal.html) - Simple graphs illustrating regression for causal inference
#'   - [Helicopters/](https://github.com/avehtari/ROS-Examples/tree/master/Helicopters/)
#'     - [helicopters.html](Helicopters/helicopters.html) - Example data file for helicopter flying time exercise
#' 
#' #### 2  Data and measurement
#'   - [HDI/](https://github.com/avehtari/ROS-Examples/tree/master/HDI/)
#'     - [hdi.html](HDI/hdi.html) - Human Development Index - Looking at data in different ways
#'   - [Pew/](https://github.com/avehtari/ROS-Examples/tree/master/Pew/)
#'     - [pew.html](Pew/pew.html) - Miscellaneous analyses using raw Pew data
#'   - [HealthExpenditure/](https://github.com/avehtari/ROS-Examples/tree/master/HealthExpenditure/)
#'     - [healthexpenditure.html](HealthExpenditure/healthexpenditure.html) - Discovery through graphs of data and models
#'   - [Names/](https://github.com/avehtari/ROS-Examples/tree/master/Names/)
#'     - [names.html](Names/names.html) - Names - Distributions of names of American babies
#'     - [lastletters.html](Names/lastletters.html) - Last letters - Distributions of last letters of names of American babies
#'   - [AgePeriodCohort/](https://github.com/avehtari/ROS-Examples/tree/master/AgePeriodCohort/)
#'     - [births.html](AgePeriodCohort/births.html) - Age adjustment
#'   - [Congress/](https://github.com/avehtari/ROS-Examples/tree/master/Congress/)
#'     - [congress_plots.html](Congress/congress_plots.html) - Predictive uncertainty for congressional elections
#' 
#' #### 3  Some basic methods in mathematics and probability
#'   - [Mile/](https://github.com/avehtari/ROS-Examples/tree/master/Mile/)
#'     - [mile.html](Mile/mile.html) - Trend of record times in the mile run
#'   - [Metabolic/](https://github.com/avehtari/ROS-Examples/tree/master/Metabolic/)
#'     - [metabolic.html](Metabolic/metabolic.html) - How to interpret a power law or log-log regression
#'   - [Earnings/](https://github.com/avehtari/ROS-Examples/tree/master/Earnings/)
#'     - [height_and_weight.html](Earnings/height_and_weight.html) - Predict weight
#'   - [CentralLimitTheorem/](https://github.com/avehtari/ROS-Examples/tree/master/CentralLimitTheorem/)
#'     - [heightweight.html](CentralLimitTheorem/heightweight.html) - Illustrate central limit theorem and normal distribution
#'   - [Stents/](https://github.com/avehtari/ROS-Examples/tree/master/Stents/)
#'     - [stents.html](Stents/stents.html) - Stents - comparing distributions
#' 
#' #### 4  Generative models and statistical inference
#'   - [Coverage/](https://github.com/avehtari/ROS-Examples/tree/master/Coverage/)
#'     - [coverage.html](Coverage/coverage.html) - Example of coverage
#'   - [Death/](https://github.com/avehtari/ROS-Examples/tree/master/Death/)
#'     - [polls.html](Death/polls.html) - Proportion of American adults supporting the death penalty
#'   - [Coop/](https://github.com/avehtari/ROS-Examples/tree/master/Coop/)
#'     - [riverbay.html](Coop/riverbay.html) - Example of hypothesis testing
#'   - [Girls/](https://github.com/avehtari/ROS-Examples/tree/master/Girls/)
#' 
#' #### 5  Simulation
#'   - [ProbabilitySimulation/](https://github.com/avehtari/ROS-Examples/tree/master/ProbabilitySimulation/)
#'     - [probsim.html](ProbabilitySimulation/probsim.html) - Simulation of probability models
#'   - [Earnings/](https://github.com/avehtari/ROS-Examples/tree/master/Earnings/)
#'     - [earnings_bootstrap.html](Earnings/earnings_bootstrap.html) - Bootstrapping to simulate the sampling distribution
#' 
#' #### 6  Background on regression modeling
#'   - [Simplest/](https://github.com/avehtari/ROS-Examples/tree/master/Simplest/)
#'     - [simplest.html](Simplest/simplest.html) - Linear regression with a single predictor
#'     - [simplest_lm.html](Simplest/simplest_lm.html) - Linear least squares regression with a single predictor
#'   - [Earnings/](https://github.com/avehtari/ROS-Examples/tree/master/Earnings/)
#'     - [earnings_regression.html](Earnings/earnings_regression.html) - Predict respondents' yearly earnings using survey data from 1990.
#'   - [PearsonLee/](https://github.com/avehtari/ROS-Examples/tree/master/PearsonLee/)
#'     - [heights.html](PearsonLee/heights.html) - The heredity of height. Published in 1903 by Karl Pearson and Alice Lee.
#'   - [FakeMidtermFinal/](https://github.com/avehtari/ROS-Examples/tree/master/FakeMidtermFinal/)
#'     - [simulation.html](FakeMidtermFinal/simulation.html) - Fake dataset of 1000 students' scores on a midterm and final exam
#' 
#' #### 7  Linear regression with a single predictor
#'   - [ElectionsEconomy/](https://github.com/avehtari/ROS-Examples/tree/master/ElectionsEconomy/)
#'     - [hibbs.html](ElectionsEconomy/hibbs.html) - Predicting presidential vote share from the economy
#'     - [hills.html](ElectionsEconomy/hills.html) - Present uncertainty in parameter estimates
#'     - [hibbs_coverage.html](ElectionsEconomy/hibbs_coverage.html) - Checking the coverage of intervals
#'    - [Simplest/](Simplest/)
#'     - [simplest.html](Simplest/simplest.html) - Linear regression with a single predictor
#'     - [simplest_lm.html](Simplest/simplest_lm.html) - Linear least squares regression with a single predictor
#'
#' #### 8 Fitting regression models
#'   - [ElectionsEconomy/](https://github.com/avehtari/ROS-Examples/tree/master/ElectionsEconomy/)
#'     - [hibbs.html](ElectionsEconomy/hibbs.html) - Predicting presidential vote share from the economy
#'   - [Influence/](https://github.com/avehtari/ROS-Examples/tree/master/Influence/)
#'     - [influence.html](Influence/influence.html) - Influence of individual points in a fitted regression
#' 
#' #### 9 Prediction and Bayesian inference 
#'   - [ElectionsEconomy/](https://github.com/avehtari/ROS-Examples/tree/master/ElectionsEconomy/)
#'     - [hibbs.html](ElectionsEconomy/hibbs.html) - Predicting presidential vote share from the economy
#'     - [bayes.html](ElectionsEconomy/bayes.html) - Demonstration of Bayesian information aggregation
#'   - [SexRatio/](https://github.com/avehtari/ROS-Examples/tree/master/SexRatio/)
#'     - [sexratio.html](SexRatio/sexratio.html) - Example where an informative prior makes a difference
#'   - [Earnings/](https://github.com/avehtari/ROS-Examples/tree/master/Earnings/)
#'     - [height_and_weight.html](Earnings/height_and_weight.html) - Predict weight
#'     - [earnings_regression.html](Earnings/earnings_regression.html) - Predict respondents' yearly earnings using survey data from 1990.
#' 
#' #### 10  Linear regression with multiple predictors
#'   - [KidIQ/](https://github.com/avehtari/ROS-Examples/tree/master/KidIQ/)
#'     - [kidiq.html](KidIQ/kidiq.html) - Linear regression with multiple predictors
#'   - [Earnings/](https://github.com/avehtari/ROS-Examples/tree/master/Earnings/)
#'     - [height_and_weight.html](Earnings/height_and_weight.html) - Predict weight
#'   - [Congress/](https://github.com/avehtari/ROS-Examples/tree/master/Congress/)
#'     - [congress.html](Congress/congress.html) - Predictive uncertainty for congressional elections
#'   - [NES/](https://github.com/avehtari/ROS-Examples/tree/master/NES/)
#'     - [nes_linear.html](NES/nes_linear.html) - Fitting the same regression to many datasets
#'   - [Beauty/](https://github.com/avehtari/ROS-Examples/tree/master/Beauty/)
#'     - [beauty.html](Beauty/beauty.html) - Student evaluations of instructors’ beauty and teaching quality
#'
#' #### 11  Assumptions, diagnostics, and model evaluation
#'   - [KidIQ/](https://github.com/avehtari/ROS-Examples/tree/master/KidIQ/)
#'     - [kidiq.html](KidIQ/kidiq.html) - Linear regression with multiple predictors
#'     - [kidiq_loo.html](KidIQ/kidiq_loo.html) - Linear regression and leave-one-out cross-validation
#'     - [kidiq_R2.html](KidIQ/kidiq_R2.html) - Linear regression and Bayes-R2 and LOO-R2
#'     - [kidiq_kcv.html](KidIQ/kidiq_kcv.html) - Linear regression and K-fold cross-validation
#'   - [Residuals/](https://github.com/avehtari/ROS-Examples/tree/master/Residuals/)
#'     - [residuals.html](Residuals/residuals.html) - Plotting the data and fitted model
#'   - [Introclass/](https://github.com/avehtari/ROS-Examples/tree/master/Introclass/)
#'     - [residual_plots.html](Introclass/residual_plots.html) - Plot residuals vs.\ predicted values, or residuals vs.\ observed values?
#'   - [Newcomb/](https://github.com/avehtari/ROS-Examples/tree/master/Newcomb/)
#'     - [newcomb.html](Newcomb/newcomb.html) - Posterior predictive checking of Normal model for Newcomb's speed of light data
#'   - [Unemployment/](https://github.com/avehtari/ROS-Examples/tree/master/Unemployment/)
#'     - [unemployment.html](Unemployment/unemployment.html) - Time series fit and posterior predictive model checking for unemployment series
#'   - [Rsquared/](https://github.com/avehtari/ROS-Examples/tree/master/Rsquared/)
#'     - [rsquared.html](Rsquared/rsquared.html) - Bayesian R^2
#'   - [CrossValidation/](https://github.com/avehtari/ROS-Examples/tree/master/CrossValidation/)
#'     - [crossvalidation.html](CrossValidation/crossvalidation.html) - Demonstration of cross validation
#'   - [FakeKCV/](https://github.com/avehtari/ROS-Examples/tree/master/FakeKCV/)
#'     - [fake_kcv.html](FakeKCV/fake_kcv.html) - Demonstration of $K$-fold cross-validation using simulated data
#'   - [Pyth/](https://github.com/avehtari/ROS-Examples/tree/master/Pyth/)
#' 
#' #### 12  Transformations
#'   - [KidIQ/](https://github.com/avehtari/ROS-Examples/tree/master/KidIQ/)
#'     - [kidiq.html](KidIQ/kidiq.html) - Linear regression with multiple predictors
#'   - [Earnings/](https://github.com/avehtari/ROS-Examples/tree/master/Earnings/)
#'     - [earnings_regression.html](Earnings/earnings_regression.html) - Predict respondents' yearly earnings using survey data from 1990.
#'   - [Gay/](https://github.com/avehtari/ROS-Examples/tree/master/Gay/)
#'     - [gay_simple.html](Gay/gay_simple.html) - Simple models (linear and discretized age) and political attitudes as a function of age
#'   - [Mesquite/](https://github.com/avehtari/ROS-Examples/tree/master/Mesquite/)
#'     - [mesquite.html](Mesquite/mesquite.html) - Predicting the yields of mesquite bushes
#'   - [Student/](https://github.com/avehtari/ROS-Examples/tree/master/Student/)
#'     - [student.html](Student/student.html) - Models for regression coefficients
#'   - [Pollution/](https://github.com/avehtari/ROS-Examples/tree/master/Pollution/)
#'     - [pollution.html](Pollution/pollution.html) - Pollution data.
#' 
#' #### 13  Logistic regression
#'   - [NES/](https://github.com/avehtari/ROS-Examples/tree/master/NES/)
#'     - [nes_logistic.html](NES/nes_logistic.html) - Logistic regression, identifiability, and separation
#'   - [LogisticPriors/](https://github.com/avehtari/ROS-Examples/tree/master/LogisticPriors/)
#'     - [logistic_priors.html](LogisticPriors/logistic_priors.html) - Effect of priors in logistic regression
#'   - [Arsenic/](https://github.com/avehtari/ROS-Examples/tree/master/Arsenic/)
#'     - [arsenic_logistic_building.html](Arsenic/arsenic_logistic_building.html) - Building a logistic regression model: wells in Bangladesh
#'
#' #### 14  Working with logistic regression
#'   - [LogitGraphs/](https://github.com/avehtari/ROS-Examples/LogitGraphs/)
#'     - [logitgraphs.html](LogitGraphs/logitgraphs.html) - Different ways of displaying logistic regression
#'   - [NES/](https://github.com/avehtari/ROS-Examples/tree/master/NES/)
#'     - [nes_logistic.html](NES/nes_logistic.html) - Logistic regression, identifiability, and separation
#'   - [Rodents/](https://github.com/avehtari/ROS-Examples/tree/master/Rodents/)
#'   - [Arsenic/](https://github.com/avehtari/ROS-Examples/tree/master/Arsenic/)
#'     - [arsenic_logistic_residuals.html](Arsenic/arsenic_logistic_residuals.html) - Residual plots for a logistic regression model: wells in Bangladesh
#'     - [arsenic_logistic_apc.html](Arsenic/arsenic_logistic_apc.html) - Average predictice comparisons for a logistic regression model: wells in Bangladesh
#'
#' #### 15  Other generalized linear models
#'   - [PoissonExample/](https://github.com/avehtari/ROS-Examples/tree/master/PoissonExample/)
#'     - [PoissonExample.html](PoissonExample/poisson_regression.html) - Demonstrate Poisson regression with simulated data.
#'   - [Roaches/](https://github.com/avehtari/ROS-Examples/tree/master/Roaches/)
#'     - [roaches.html](Roaches/roaches.html) - Analyse the effect of integrated pest management on reducing cockroach levels in urban apartments
#'   - [Storable/](https://github.com/avehtari/ROS-Examples/tree/master/Storable/) 
#'     - [storable.html](Storable/storable.html) - Ordered categorical data analysis with a study from experimental economics, on the topic of ``storable votes.''
#'   - [Earnings/](https://github.com/avehtari/ROS-Examples/tree/master/Earnings/)
#'     - [earnings_compound.html](Earnings/earnings_compound.html) - Compound discrete-continuos model
#'   - [RiskyBehavior/](https://github.com/avehtari/ROS-Examples/tree/master/RiskyBehavior/)
#'     - [risky.html](RiskyBehavior/risky.html) Risky behavior data.
#'   - [NES/](https://github.com/avehtari/ROS-Examples/tree/master/NES/)
#'   - [Lalonde/](https://github.com/avehtari/ROS-Examples/tree/master/Lalonde/)
#'   - [Congress/](https://github.com/avehtari/ROS-Examples/tree/master/Congress/)
#'   - [AcademyAwards/](https://github.com/avehtari/ROS-Examples/tree/master/AcademyAwards/)
#'
#' #### 16  Design and sample size decisions
#'   - [ElectricCompany/](https://github.com/avehtari/ROS-Examples/tree/master/ElectricCompany/)
#'     - [electric.html](ElectricCompany/electric.html) - Analysis of "Electric company" data
#'   - [SampleSize/](https://github.com/avehtari/ROS-Examples/tree/master/SampleSize/)
#'     - [simulation.html](DataCollection/simulation.html) - Sample size simulation
#'   - [FakeMidtermFinal/](https://github.com/avehtari/ROS-Examples/tree/master/FakeMidtermFinal/)
#'     - [simulation_based_design.html](FakeMidtermFinal/simulation_based_design.html) - Fake dataset of a randomized experiment on student grades
#'
#' #### 17  Poststratification and missing-data imputation
#'   - [Poststrat/](https://github.com/avehtari/ROS-Examples/tree/master/Poststrat/)
#'     - [poststrat.html](Poststrat/poststrat.html) - Poststratification after estimation
#'     - [poststrat2.html](Poststrat/poststrat2.html) - Poststratification after estimation
#'   - [Imputation/](https://github.com/avehtari/ROS-Examples/tree/master/Imputation/)
#'     - [imputation.html](Imputation/imputation.html) - Regression-based imputation for the Social Indicators Survey
#'     - [imputation_gg.html](Imputation/imputation_gg.html) - Regression-based imputation for the Social Indicators Survey, dplyr/ggplot version
#'
#' #### 18  Causal inference basics and randomized experiments
#'   - [Sesame/](https://github.com/avehtari/ROS-Examples/tree/master/Sesame/)
#'     - [sesame.html](Sesame/sesame.html) - Causal analysis of Sesame Street experiment
#' 
#' #### 19  Causal inference using regression on the treatment variable
#'   - [ElectricCompany/](https://github.com/avehtari/ROS-Examples/tree/master/ElectricCompany/)
#'     - [electric.html](ElectricCompany/electric.html) - Analysis of "Electric company" data
#'   - [Incentives/]((https://github.com/avehtari/ROS-Examples/tree/master/Incentives/))
#'     - [incentives.html](Incentives/incentives.html) - Simple analysis of incentives data
#'   - [Cows/](https://github.com/avehtari/ROS-Examples/tree/master/Cows/)
#' 
#' #### 20  Observational studies with all confounders assumed to be measured
#'   - [ElectricCompany/](https://github.com/avehtari/ROS-Examples/tree/master/ElectricCompany/)
#'     - [electric.html](ElectricCompany/electric.html) - Analysis of "Electric company" data
#'   - [Childcare/](https://github.com/avehtari/ROS-Examples/tree/master/Childcare/)
#'     - [childcare.html](Childcare/childcare.html) - Infant Health and Development Program (IHDP) example.
#' 
#' #### 21  More advanced topics in causal inference
#'   - [Sesame/](https://github.com/avehtari/ROS-Examples/tree/master/Sesame/)
#'     - [sesame.html](Sesame/sesame.html) - Causal analysis of Sesame Street experiment
#'   - [Bypass/](https://github.com/avehtari/ROS-Examples/tree/master/Pypass/)
#'   - [ChileSchools/](https://github.com/avehtari/ROS-Examples/tree/master/ChileSchools/)
#'     - [chile_schools.html](ChileSchools/chile_schools.html) - ChileSchools example.
#' 
#' #### 22  Advanced regression and multilevel models
#'   - [Golf/](https://github.com/avehtari/ROS-Examples/tree/master/Golf/)
#'     - [golf.html](Golf/golf.html) - Gold putting accuracy: Fitting a nonlinear model using Stan
#'   - [Gay/](https://github.com/avehtari/ROS-Examples/tree/master/Gay/)
#'     - [gay.html](Gay/gay.html) - Nonlinear models (Loess, B-spline, GP-spline, and BART) and political attitudes as a function of age
#'   - [ElectionsEconomy/](https://github.com/avehtari/ROS-Examples/tree/master/ElectionsEconomy/)
#'     - [hibbs.html](ElectionsEconomy/hibbs.html) - Predicting presidential vote share from the economy
#'   - [Scalability/](https://github.com/avehtari/ROS-Examples/tree/master/Scalability/)
#'     - [scalability.html](Scalability/scalability.html) - Demonstrate computation speed with 100 000 observations.
#'
#' #### Appendix A
#'   - [Coins/](https://github.com/avehtari/ROS-Examples/tree/master/Coins/)
#'   - [Mile/](https://github.com/avehtari/ROS-Examples/tree/master/Mile/)
#'     - [mile.html](Mile/mile.html) - Trend of record times in the mile run
#'   - [Parabola/](https://github.com/avehtari/ROS-Examples/tree/master/Parabola/)
#'     - [parabola.html](Parabola/parabola.html) - Demonstration of using Stan for optimization
#'   - [Restaurant/](https://github.com/avehtari/ROS-Examples/tree/master/Restaurant/)
#'     - [restaurant.html](Restaurant/restaurant.html) - Demonstration of using Stan for optimization
#'   - [DifferentSoftware/](https://github.com/avehtari/ROS-Examples/tree/master/DifferentSoftware/)
#'     - [linear.html](DifferentSoftware/linear.html) - Linear regression using different software options
#'
#' -------------
#' 
#' ### Code and data alphabetically<a id="alphabetically"></a>
#' 
#' The folders below (ending /) point to the code (.R and .Rmd) and
#' data folders in github, and .html -files point to pretty
#' notebooks. Most examples have cleaned data in csv file in data
#' subfolder for easy experimenting. The data subfolders have also the
#' raw data and *_setup.R file showing how the data cleaning has been
#' done.
#' 
#' -------------
#' 
#' - [AcademyAwards/](https://github.com/avehtari/ROS-Examples/tree/master/AcademyAwards/)
#' - [AgePeriodCohort/](https://github.com/avehtari/ROS-Examples/tree/master/AgePeriodCohort/)
#'   - [births.html](AgePeriodCohort/births.html) - Age adjustment
#' - [Arsenic/](https://github.com/avehtari/ROS-Examples/tree/master/Arsenic/)
#'   - [arsenic_logistic_building.html](Arsenic/arsenic_logistic_building.html) - Building a logistic regression model: wells in Bangladesh
#'   - [arsenic_logistic_residuals.html](Arsenic/arsenic_logistic_residuals.html) - Residual plots for a logistic regression model: wells in Bangladesh
#'   - [arsenic_logistic_apc.html](Arsenic/arsenic_logistic_apc.html) - Average predictice comparisons for a logistic regression model: wells in Bangladesh
#'   - [arsenic_logistic_building_optimizing.html](Arsenic/arsenic_logistic_building_optimizing.html) - Building a logistic regression model: wells in Bangladesh. A version with normal approximation at the mode.
#' - [Balance/](https://github.com/avehtari/ROS-Examples/tree/master/Balance/)
#'   - [treatcontrol.html](Balance/treatcontrol.html)
#' - [Beauty/](https://github.com/avehtari/ROS-Examples/tree/master/Beauty/)
#'   - [beauty.html](Beauty/beauty.html) - Student evaluations of instructors’ beauty and teaching quality
#' - [Bypass/](https://github.com/avehtari/ROS-Examples/tree/master/Bypass/)
#' - [CausalDiagram/](https://github.com/avehtari/ROS-Examples/tree/master/CausalDiagram/)
#'   - [diagrams.html](CausalDiagram/diagrams.html) - Plot causal diagram
#' - [CentralLimitTheorem/](https://github.com/avehtari/ROS-Examples/tree/master/CentralLimitTheorem/)
#'   - [heightweight.html](CentralLimitTheorem/heightweight.html) - Illustrate central limit theorem and normal distribution
#' - [Childcare/](https://github.com/avehtari/ROS-Examples/tree/master/Childcare/)
#'   - [childcare.html](Childcare/childcare.html) - Infant Health and Development Program (IHDP) example.
#' - [ChileSchools/](https://github.com/avehtari/ROS-Examples/tree/master/ChileSchools/)
#'   - [chile_schools.html](ChileSchools/chile_schools.html) - ChileSchools example.
#' - [Coins/](https://github.com/avehtari/ROS-Examples/tree/master/Coins/)
#' - [Congress/](https://github.com/avehtari/ROS-Examples/tree/master/Congress/)
#'   - [congress.html](Congress/congress.html) - Predictive uncertainty for congressional elections
#'   - [congress_plots.html](Congress/congress_plots.html) - Predictive uncertainty for congressional elections
#' - [Coop/](https://github.com/avehtari/ROS-Examples/tree/master/Coop/)
#'   - [riverbay.html](Coop/riverbay.html) - Example of hypothesis testing
#' - [Coverage/](https://github.com/avehtari/ROS-Examples/tree/master/Coverage/)
#'   - [coverage.html](Coverage/coverage.html) - Example of coverage
#' - [Cows/](https://github.com/avehtari/ROS-Examples/tree/master/Cows/)
#' - [CrossValidation/](https://github.com/avehtari/ROS-Examples/tree/master/CrossValidation/)
#'   - [crossvalidation.html](CrossValidation/crossvalidation.html) - Demonstration of cross validation
#' - [SampleSize/](https://github.com/avehtari/ROS-Examples/tree/master/SampleSize/)
#'   - [simulation.html](DataCollection/simulation.html) - Sample size simulation
#' - [Death/](https://github.com/avehtari/ROS-Examples/tree/master/Death/)
#'   - [polls.html](Death/polls.html) - Proportion of American adults supporting the death penalty
#' - [DifferentSoftware/](https://github.com/avehtari/ROS-Examples/tree/master/DifferentSoftware/)
#'   - [linear.html](DifferentSoftware/linear.html) - Linear regression using different software options
#' - [Earnings/](https://github.com/avehtari/ROS-Examples/tree/master/Earnings/)
#'   - [earnings_regression.html](Earnings/earnings_regression.html) - Predict respondents' yearly earnings using survey data from 1990.
#'   - [earnings_bootstrap.html](Earnings/earnings_bootstrap.html) - Bootstrapping to simulate the sampling distribution
#'   - [earnings_compound.html](Earnings/earnings_compound.html) - Compound discrete-continuos model
#'   - [height_and_weight.html](Earnings/height_and_weight.html) - Predict weight
#' - [ElectionsEconomy/](https://github.com/avehtari/ROS-Examples/tree/master/ElectionsEconomy/)
#'   - [bayes.html](ElectionsEconomy/bayes.html) - Demonstration of Bayesian information aggregation
#'   - [hibbs.html](ElectionsEconomy/hibbs.html) - Predicting presidential vote share from the economy
#'   - [hills.html](ElectionsEconomy/hills.html) - Present uncertainty in parameter estimates
#'   - [hibbs_coverage.html](ElectionsEconomy/hibbs_coverage.html) - Checking the model-fitting procedure using fake-data simulation.
#' - [ElectricCompany/](https://github.com/avehtari/ROS-Examples/tree/master/ElectricCompany/)
#'   - [electric.html](ElectricCompany/electric.html) - Analysis of "Electric company" data
#' - [FakeKCV/](https://github.com/avehtari/ROS-Examples/tree/master/FakeKCV/)
#'   - [fake_kcv.html](FakeKCV/fake_kcv.html) - Demonstration of $K$-fold cross-validation using simulated data
#' - [FakeMidtermFinal/](https://github.com/avehtari/ROS-Examples/tree/master/FakeMidtermFinal/)
#'   - [simulation.html](FakeMidtermFinal/simulation.html) - Fake dataset of 1000 students' scores on a midterm and final exam
#'   - [simulation_based_design.html](FakeMidtermFinal/simulation_based_design.html) - Fake dataset of a randomized experiment on student grades
#' - [FrenchElection/](https://github.com/avehtari/ROS-Examples/tree/master/FrenchElection/)
#'   - [ps_primaire.html](FrenchElection/ps_primaire.html) - French Election data
#' - [Gay/](https://github.com/avehtari/ROS-Examples/tree/master/Gay/)
#'   - [gay_simple.html](Gay/gay_simple.html) - Simple models (linear and discretized age) and political attitudes as a function of age
#'   - [gay.html](Gay/gay.html) - Nonlinear models (Loess, B-spline, GP-spline, and BART) and political attitudes as a function of age
#' - [Girls/](https://github.com/avehtari/ROS-Examples/tree/master/Girls/)
#' - [Golf/](https://github.com/avehtari/ROS-Examples/tree/master/Golf/)
#'   - [golf.html](Golf/golf.html) - Gold putting accuracy: Fitting a nonlinear model using Stan
#' - [HDI/](https://github.com/avehtari/ROS-Examples/HDI/)
#'   - [hdi.html](HDI/hdi.html) - Human Development Index - Looking at data in different ways
#' - [HealthExpenditure/](https://github.com/avehtari/ROS-Examples/tree/master/HealthExpenditure/)
#'   - [healthexpenditure.html](HealthExpenditure/healthexpenditure.html) - Discovery through graphs of data and models
#' - [Helicopters/](https://github.com/avehtari/ROS-Examples/tree/master/Helicopters/)
#'   - [helicopters.html](Helicopters/helicopters.html) - Example data file for helicopter flying time exercise
#' - [Imputation/](https://github.com/avehtari/ROS-Examples/tree/master/Imputation/)
#'   - [imputation.html](Imputation/imputation.html) - Regression-based imputation for the Social Indicators Survey
#'   - [imputation_gg.html](Imputation/imputation_gg.html) - Regression-based imputation for the Social Indicators Survey, dplyr/ggplot version
#' - [Incentives/]((https://github.com/avehtari/ROS-Examples/tree/master/Incentives/))
#'   - [incentives.html](Incentives/incentives.html) - Simple analysis of incentives data
#' - [Influence/](https://github.com/avehtari/ROS-Examples/tree/master/Influence/)
#'   - [influence.html](Influence/influence.html) - Influence of individual points in a fitted regression
#' - [Interactions/](https://github.com/avehtari/ROS-Examples/tree/master/Interactions/)
#'   - [interactions.html](Interactions/interactions.html) - Plot interaction example figure
#' - [Introclass/](https://github.com/avehtari/ROS-Examples/tree/master/Introclass/)
#'   - [residual_plots.html](Introclass/residual_plots.html) - Plot residuals vs.\ predicted values, or residuals vs.\ observed values?
#' - [KidIQ/](https://github.com/avehtari/ROS-Examples/tree/master/KidIQ/)
#'   - [kidiq.html](KidIQ/kidiq.html) - Linear regression with multiple predictors
#'   - [kidiq_loo.html](KidIQ/kidiq_loo.html) - Linear regression and leave-one-out cross-validation
#'   - [kidiq_R2.html](KidIQ/kidiq_R2.html) - Linear regression and Bayes-R2 and LOO-R2
#'   - [kidiq_kcv.html](KidIQ/kidiq_kcv.html) - Linear regression and K-fold cross-validation
#' - [Lalonde/](https://github.com/avehtari/ROS-Examples/tree/master/Lalonde/)
#' - [LogisticPriors/](https://github.com/avehtari/ROS-Examples/tree/master/LogisticPriors/)
#'   - [logistic_priors.html](LogisticPriors/logistic_priors.html) - Effect of priors in logistic regression
#' - [Mesquite/](https://github.com/avehtari/ROS-Examples/tree/master/Mesquite/)
#'   - [mesquite.html](Mesquite/mesquite.html) - Predicting the yields of mesquite bushes
#' - [Metabolic/](https://github.com/avehtari/ROS-Examples/tree/master/Metabolic/)
#'   - [metabolic.html](Metabolic/metabolic.html) - How to interpret a power law or log-log regression
#' - [Mile/](https://github.com/avehtari/ROS-Examples/tree/master/Mile/)
#'   - [mile.html](Mile/mile.html) - Trend of record times in the mile run
#' - [Names/](https://github.com/avehtari/ROS-Examples/tree/master/Names/)
#'   - [names.html](Names/names.html) - Names - Distributions of names of American babies
#'   - [lastletters.html](Names/lastletters.html) - Last letters - Distributions of last letters of names of American babies
#' - [NES/](https://github.com/avehtari/ROS-Examples/tree/master/NES/)
#'   - [nes_linear.html](NES/nes_linear.html) - Fitting the same regression to many datasets
#'   - [nes_logistic.html](NES/nes_logistic.html) - Logistic regression, identifiability, and separation
#' - [Newcomb/](https://github.com/avehtari/ROS-Examples/tree/master/Newcomb/)
#'   - [newcomb.html](Newcomb/newcomb.html) - Posterior predictive checking of Normal model for Newcomb's speed of light data
#' - [Parabola/](https://github.com/avehtari/ROS-Examples/tree/master/Parabola/)
#'   - [parabola.html](Parabola/parabola.html) - Demonstration of using Stan for optimization
#' - [Peacekeeping/](https://github.com/avehtari/ROS-Examples/tree/master/Peacekeeping/)
#'   - [peace.html](Peacekeeping/peace.html) - Outcomes after civil war in countries with and without United Nations peacekeeping
#' - [PearsonLee/](https://github.com/avehtari/ROS-Examples/tree/master/PearsonLee/)
#'   - [heights.html](PearsonLee/heights.html) - The heredity of height. Published in 1903 by Karl Pearson and Alice Lee.
#' - [Pew/](https://github.com/avehtari/ROS-Examples/tree/master/Pew/)
#'   - [pew.html](Pew/pew.html) - Miscellaneous analyses using raw Pew data
#' - [PoissonExample/](https://github.com/avehtari/ROS-Examples/tree/master/PoissonExample/)
#'   - [poissonexample.html](PoissonExample/poissonexample.html) - Demonstrate Poisson regression with simulated data.
#' - [Pollution/](https://github.com/avehtari/ROS-Examples/tree/master/Pollution/)
#'   - [pollution.html](Pollution/pollution.html) - Pollution data.
#' - [Poststrat/](https://github.com/avehtari/ROS-Examples/tree/master/Poststrat/)
#'   - [poststrat.html](Poststrat/poststrat.html) - Poststratification after estimation
#'   - [poststrat2.html](Poststrat/poststrat2.html) - Poststratification after estimation
#' - [ProbabilitySimulation/](https://github.com/avehtari/ROS-Examples/tree/master/ProbabilitySimulation/)
#'   - [probsim.html](ProbabilitySimulation/probsim.html) - Simulation of probability models
#' - [Pyth/](https://github.com/avehtari/ROS-Examples/tree/master/Pyth/)
#' - [Redistricting/](https://github.com/avehtari/ROS-Examples/tree/master/Redistricting/)
#' - [Residuals/](https://github.com/avehtari/ROS-Examples/tree/master/Residuals/)
#'   - [residuals.html](Residuals/residuals.html) - Plotting the data and fitted model
#' - [Restaurant/](https://github.com/avehtari/ROS-Examples/tree/master/Restaurant/)
#'   - [restaurant.html](Restaurant/restaurant.html) - Demonstration of using Stan for optimization
#' - [RiskyBehavior/](https://github.com/avehtari/ROS-Examples/tree/master/RiskyBehavior/)
#'   - [risky.html](RiskyBehavior/risky.html) Risky behavior data.
#' - [Roaches/](https://github.com/avehtari/ROS-Examples/tree/master/Roaches/)
#'   - [roaches.html](Roaches/roaches.html) - Analyse the effect of integrated pest management on reducing cockroach levels in urban apartments
#' - [Rodents/](https://github.com/avehtari/ROS-Examples/tree/master/Rodents/)
#' - [Rsquared/](https://github.com/avehtari/ROS-Examples/tree/master/Rsquared/)
#'   - [rsquared.html](Rsquared/rsquared.html) - Bayesian R^2
#' - [Sesame/](https://github.com/avehtari/ROS-Examples/tree/master/Sesame/)
#'   - [sesame.html](Sesame/sesame.html) - Causal analysis of Sesame Street experiment
#' - [SexRatio/](https://github.com/avehtari/ROS-Examples/tree/master/SexRatio/)
#'   - [sexratio.html](SexRatio/sexratio.html) - Example where an informative prior makes a difference
#' - [SimpleCausal/](https://github.com/avehtari/ROS-Examples/tree/master/SimpleCausal/)
#'   - [causal.html](SimpleCausal/causal.html) - Simple graphs illustrating regression for causal inference
#' - [Simplest/](https://github.com/avehtari/ROS-Examples/tree/master/Simplest/)
#'   - [simplest.html](Simplest/simplest.html) - Linear regression with a single predictor
#'   - [simplest_lm.html](Simplest/simplest_lm.html) - Linear least squares regression with a single predictor
#' - [Stents/](https://github.com/avehtari/ROS-Examples/tree/master/Stents/)
#'   - [stents.html](Stents/stents.html) - Stents - comparing distributions
#' - [Storable/](https://github.com/avehtari/ROS-Examples/tree/master/Storable/)
#'   - [storable.html](Storable/storable.html) - Ordered categorical data analysis with a study from experimental economics, on the topic of ``storable votes.''
#' - [Student/](https://github.com/avehtari/ROS-Examples/tree/master/Student/)
#'   - [student.html](Student/student.html) - Models for regression coefficients
#' - [Unemployment/](https://github.com/avehtari/ROS-Examples/tree/master/Unemployment/)
#'   - [unemployment.html](Unemployment/unemployment.html) - Time series fit and posterior predictive model checking for unemployment series
#'
#' ### Download code and data<a id="Download"></a>
#' 
#' - [Git repo for the code and data](https://github.com/avehtari/ROS-Examples)
#' - [Download all code and data as a ZIP file](https://github.com/avehtari/ROS-Examples/archive/master.zip)
#'
#' ### Python code<a id="Python"></a>
#'
#' Ravin Kumar and Osvaldo Martin are porting ROS examples to Python using bambi (BAyesian Model-Building Interface) which has similar formula syntax as rstanarm. 
#' - [Bambi resources](https://github.com/bambinos/Bambi_resources)
#'
