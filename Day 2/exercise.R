 
# School-level data from New York to investigate whether an 
# intervention (i.e., being designated a Schools to Watch (stw) school) was 
# a predictor of success in reading and mathematics achievement, when controlling 
# for :
#    school size (tot), 
#    percentage of minority students (min), and 
#    percentage of students receiving free lunch (dis). 

# In the New York data set, there were 25 stw schools and 560 non-stw schools

# installing a package
install.packages("MatchIt")

# "loading" a package for use
library(MatchIt)

# read data
mydata <- read.csv ("newyork.csv")

head(mydata)

# descriptive statistics --------------------------------------------------
# school size (tot)
summary(mydata$tot)
boxplot(mydata$tot)
boxplot(mydata$tot~mydata$stw)

# achivement status by stw (the intervention)
table(STW = mydata$stw, Achievement = mydata$Achievement)
# percentage of achievement for stw == 0
60 / (499 + 60) * 100
# percentage of achievement for stw == 1
18 / (18 + 7) * 100

## To estimate the effect of stw on Achievement while controlling for other variables,
# we need to perform Logistic regression (future lecture)

# PSM ---------------------------------------------------------------------
m.out <- matchit(stw ~ tot + min + dis,
                 data = mydata, method = "nearest",
                 ratio = 1) # the default is 1 for 1:1 matching

plot(summary(m.out))
plot(summary(m.out), xlim = c(0, 1.6))

plot(m.out, type = "jitter", interactive = FALSE)
plot(m.out, type = "hist")

#Exact Matching - This treated unit with a control unit that has exactly the same values on each covariate. When there are many covariates and/or covariates that can take a large range of values, exact matching may not be possible (method = "exact").
#Subclassification - This technique breaks the data set into subclasses such that the distributions of the covariates are similar in each subclass (method = "subclass").
#Nearest Neighbor - This technique matches a treated unit to a control u terms of a distance measure such as a logit (method = "nearest").
#Optimal Matching - This technique focuses on minimizing the average absolute distance across all matched pairs (method = "optimal"). This method of matching requir package.
#Genetic Matching - computationally intensive genetic search algorithm to match treatment and control units (method = "genetic"). It requires the Matching package.
#Coarsened Exact Matching technique matches on a covariate while maintaining the balance of other covariates. It is claimed to work "well for multicategory treatments, determining blocks in experimental designs, and evaluating extreme counterfactuals" (Ho, Kosuke, King, & Stuart,2011, p.12) (method = "cem")

# postPSM -----------------------------------------------------------------
m.data <- match.data(m.out)

table(STW = m.data$stw, Achievement = m.data$Achievement)
4 / 25 * 100
18 / 25 * 100
