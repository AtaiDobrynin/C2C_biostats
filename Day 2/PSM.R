##################################################
## Project: R Training
## Script purpose: Propensity Score Matching
## Author: Ege Ulgen
##################################################
# https://cran.r-project.org/web/packages/MatchIt/vignettes/MatchIt.html

# install.packages("MatchIt")
# install.packages(c("optmatch", "lmtest", "sandwich"))

library(MatchIt)
library(lmtest) 
library(sandwich)

?lalonde
data("lalonde")

head(lalonde)

# what is the effect of treatment on income in 1978 (re78)?
table(lalonde$treat)
summary(lalonde$re78[lalonde$treat == 0])
summary(lalonde$re78[lalonde$treat == 1])

boxplot(lalonde$re78~lalonde$treat)

# what is the effect of treatment on income in 1978 (re78), controlling for other variables?
fit0 <- lm(re78 ~ treat + age + educ + race + married + nodegree + 
               re74 + re75, data = lalonde)
summary(fit0)

?matchit

# No matching; constructing a pre-match matchit object
m.out0 <- matchit(treat ~ age + educ + race + married + 
                      nodegree + re74 + re75, data = lalonde,
                  method = NULL, distance = "glm")
summary(m.out0)

plot(summary(m.out0))

plot(m.out0, type = "jitter", interactive = FALSE)

# 1:1 Nearest Neighbor PS matching w/o replacement
m.out1 <- matchit(treat ~ age + educ + race + married + 
                      nodegree + re74 + re75, data = lalonde,
                  method = "nearest", distance = "glm")
m.out1

# Checking balance after NN matching
# summary(m.out1, un = FALSE)

plot(summary(m.out1))
plot(m.out1, type = "jitter", interactive = FALSE)


# Full matching on a probit PS
# probit: the cumulative distribution function of the standard normal distribution
# logit: the cumulative distribution function of the logistic distribution
m.out2 <- matchit(treat ~ age + educ + race + married + 
                      nodegree + re74 + re75, data = lalonde,
                  method = "full", distance = "glm", link = "probit")

m.out2

# Checking balance after full matching
# summary(m.out2, un = FALSE)

plot(summary(m.out2))
plot(m.out2, type = "jitter", interactive = FALSE)
plot(m.out2, type = "hist")


# Refit model using matched data ------------------------------------------
m.data1 <- match.data(m.out1)
head(m.data1)

fit1 <- lm(re78 ~ treat + age + educ + race + married + nodegree + 
               re74 + re75, data = m.data1, weights = weights)

# We recommend using cluster-robust standard errors for most analyses, 
# with pair membership (subclass) as the clustering variable
?coeftest
coeftest(fit1, vcov. = vcovCL, cluster = ~subclass)


m.data2 <- match.data(m.out2)
head(m.data2)

fit2 <- lm(re78 ~ treat + age + educ + race + married + nodegree + 
               re74 + re75, data = m.data2, weights = weights)

coeftest(fit2, vcov. = vcovCL, cluster = ~subclass)

# We used propensity score matching to estimate the average marginal effect of 
# the treatment on 1978 earnings on those who received it accounting for 
# confounding by the included covariates. We first attempted 1:1 nearest 
# neighbor propensity score matching without replacement with a propensity score 
# estimated using logistic regression of the treatment on the covariates. 
# This matching yielded poor balance, so we instead tried full matching on the 
# propensity score, which yielded adequate balance. After matching, all 
# standardized mean differences for the covariates were below 0.1. Full matching 
# uses all treated and all control units, so no units were discarded by the matching.

# To estimate the treatment effect and its standard error, we fit a linear 
# regression model with 1978 earnings as the outcome and the treatment and the 
# covariates as additive predictors and included the full matching weights in 
# the estimation. The coefficient on the treatment was taken to be the estimate 
# of the treatment effect. The lm() function was used to estimate the effect, 
# and a cluster-robust variance as implemented in the vcovCL() function in the 
# sandwich package was used to estimate its standard error with matching stratum 
# membership as the clustering variable.

# The estimated effect was $1980 (SE = 756.1, p = .009), indicating that the
# average effect of the treatment for those who received it is to increase earnings.