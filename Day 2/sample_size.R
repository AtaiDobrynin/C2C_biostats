##################################################
## Project: R Training
## Script purpose: Experimental design
## Author: Ege Ulgen
##################################################

# install.packages("pwr")
library(pwr)

# Sample size calculation -------------------------------------------------

### Effect size: magnitude of the effect under the alternative hypothesis
# The larger the effect size, the easier it is to detect an effect and require fewer samples
### Power: probability of correctly rejecting the null hypothesis if it is false
# The higher the power, the more likely it is to detect an effect if it is present and the more samples needed
# Standard setting for power is 0.80
### Significance level (α): probability of falsely rejecting the null hypothesis even though it is true
# The lower the significance level, the more likely it is to avoid a false positive and
# the more samples needed
# Standard setting for α is 0.05



### Correlation
?pwr.r.test
# r=correlation
# sig.level=significant level
# power=power of test

# effect size >>> 0.1=small, 0.3=medium, and 0.5 large

# Is there a correlation between hours studied and test score?
# assuming large correlation
pwr.r.test(r=0.5, sig.level=0.05, power=0.80)

# calculating power
pwr.r.test(n = 50, r=0.5, sig.level=0.05)
pwr.r.test(n = 10, r=0.5, sig.level=0.05)


### Two-sample t-test
?pwr.t.test
# d=effect size
# sig.level=significant level
# power=power of test
# type=type of test

# effect size >>> 0.2=small, 0.5=medium, and 0.8 large
# effect size calculation >>> Cohen's D

# Are the average body temperatures of women and men different?
# assuming medium effect size
pwr.t.test(d=0.5, sig.level=0.05, power=0.80, type="two.sample", alternative="two.sided")

# small effect size
pwr.t.test(d=0.2, sig.level=0.05, power=0.80, type="two.sample", alternative="two.sided")

# large effect size
pwr.t.test(d=0.8, sig.level=0.05, power=0.80, type="two.sample", alternative="two.sided")


# Is the average body temperature higher in women than in men?
pwr.t.test(d=0.5, sig.level=0.05, power=0.80, type="two.sample", alternative="greater")

### One-way ANOVA
?pwr.anova.test
# k=number of groups
# f=effect size
# sig.level=significant level
# power=power of test


# effect size >>> 0.1=small, 0.25=medium, and 0.4 large

# Is there a difference in disease incidence across 6 different cities?
# assuming small effect size
pwr.anova.test(k = 6 , f = 0.1 , sig.level = 0.05 , power = 0.80)


### Chi-squared test
?pwr.chisq.test
# w=effect size
# df=degrees of freedom
# sig.level=significant level
# power=power of test


# effect size >>> 0.1=small, 0.3=medium, and 0.5 large


# Does the observed proportions of phenotypes from a genetics experiment different from the expected 9:3:3:1?

# aassuming medium effect
# df = 4 (phenotypes) – 1 = 3
pwr.chisq.test(w=0.3, df=3, sig.level=0.05, power=0.80)


### Linear Regression
?pwr.f2.test
# u=numerator degrees of freedom
# v=denominator degrees of freedom
# f2=effect size
# sig.level=significant level
# power=power of test

# effect size >>> 0.02=small, 0.15=medium, and 0.35 large

# Can height, age, and time spent at the gym, predict weight in adult males?
# assuming medium effect size
(res <- pwr.f2.test(u = 3, f2 = 0.15, sig.level = 0.05, power = 0.8))
res$v + 4 #(tot. num. of variables)
