Data in file incentives_full.txt

The data are from the metaanalysis in Singer et al. (1999), and shared with permission. 
We received the data in ascii, converted to txt, and fixed some apparent typos. Each line in the data represents a different experimental condition in a survey experiment. There are 39 experiments with a total of 101 conditions.

sid = survey id. There are 39 ids ranging from 20 to 400. 
I = Is an incentive given? (0=no, 1=yes)    
m = mode of survey (-.5 = telephone or .5 = face to face)    
r = response rate (from 0 (nobody contacted responded to the survey) to 1 (everyone responded who was contacted))
basen = number of people who were attempted to be contacted in this arm of the experiment
t = timing (-.5 = after  or .5 = before the survey, 0 = no incentive)
f = form (-.5 = cash, .5 = gift, 0 = no incentive)
r.dif = difference in response rate (within survey experiment from no incentive to incentive). Set to NA for the baseline condition in each survey
v = value of incentive (from 0 to 119.587, 0 = no incentive)
v.dif = difference in value of incentive (within survey experiment from no incentive to incentive) . Set to NA for the baseline condition in each survey
b = burden (-.5 = low, .5 = high)

Singer et al. (1999) compiled this dataset and analyzed it using a linear regression which we reproduce (with some small differences) and criticize in chapter 19 in Regression and Other Stories.

Gelman, Stevens, and Chan (2003) reanalyze these data using a hierachical regression.

Kennedy and Gelman (2020) reanalyze using a hierarchical regression with informative priors and a nonlinear function for incentive effects.
