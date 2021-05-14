# install.packages("ggpubr")

library(ggpubr)

# one-way ANOVA -----------------------------------------------------------
data(PlantGrowth)
?PlantGrowth

summary(PlantGrowth$weight[PlantGrowth$group == "ctrl"])
summary(PlantGrowth$weight[PlantGrowth$group == "trt1"])
summary(PlantGrowth$weight[PlantGrowth$group == "trt2"])

ggboxplot(PlantGrowth, x = "group", y = "weight", 
          color = "group", 
          order = c("ctrl", "trt1", "trt2"),
          ylab = "Weight", xlab = "Treatment")

# add jitter
g <- ggboxplot(PlantGrowth, x = "group", y = "weight", 
               color = "group", 
               order = c("ctrl", "trt1", "trt2"),
               ylab = "Weight", xlab = "Treatment", add = "jitter")
g

# check normality of outcome
ggqqplot(PlantGrowth$weight[PlantGrowth$group == "ctrl"])
ggqqplot(PlantGrowth$weight[PlantGrowth$group == "trt1"])
ggqqplot(PlantGrowth$weight[PlantGrowth$group == "trt2"])

res.aov <- aov(weight ~ group, data = PlantGrowth)
summary(res.aov)

g + stat_compare_means(method = "aov")

# The Df column displays the degrees of freedom for
# the independent variable (the number of levels in the variable minus 1),
# and the degrees of freedom for the residuals (the total number of observations
# minus one and minus the number of levels in the independent variables).
# The Sum Sq column displays the sum of squares (a.k.a. the total variation between
# the group means and the overall mean).
# The Mean Sq column is the mean of the sum of squares, calculated by dividing the
# sum of squares by the degrees of freedom for each parameter.
# The F-value column is the test statistic from the F test. This is the mean square 
# of each independent variable divided by the mean square of the residuals. The larger
# the F value, the more likely it is that the variation caused by the 
# independent variable is real and not due to chance.
# The Pr(>F) column is the p-value of the F-statistic. This shows how likely it is
# that the F-value calculated from the test would have occurred if the null 
# hypothesis of no difference among group means were true.


# Tukey Honest Significant Differences
TukeyHSD(res.aov)

### Alternative
fit_lm <- lm(weight ~ group, data = PlantGrowth)
summary(fit_lm)

anova(fit_lm)

PlantGrowth$group
PlantGrowth$group <- relevel(PlantGrowth$group, ref = 2)
PlantGrowth$group
fit_lm2 <- lm(weight ~ group, data = PlantGrowth)
summary(fit_lm2)

PlantGrowth$group <- relevel(PlantGrowth$group, ref = 2)

# Kruskal-Wallis rank-sum test --------------------------------------------
kruskal.test(weight ~ group, data = PlantGrowth)
# As the p-value is less than the significance level 0.05, 
# we can conclude that there are significant differences between groups.

# Ex2 one way ANOVA -------------------------------------------------------

# Our sample dataset contains observations from a study of the 
# effects of fertilizer type and planting density on crop yield.

crop.data <- read.csv("crop_data.csv", header = TRUE)
head(crop.data)

crop.data$density <- as.factor(crop.data$density)
crop.data$fertilizer <- as.factor(crop.data$fertilizer)

summary(crop.data)

# Is there a difference of crop yield between the 3 types of fertilizer?
ggboxplot(data = crop.data, x = "fertilizer", y = "yield", 
          color = "fertilizer", 
          order = c("1", "2", "3"),
          ylab = "Yield", xlab = "Fertilizer", add = "jitter")

one.way <- aov(yield ~ fertilizer, data = crop.data)

summary(one.way)

# Tukey Honest Significant Differences test
TukeyHSD(one.way)


# two way ANOVA -----------------------------------------------------------
# In the two-way ANOVA, we add an additional independent variable: planting density. 
# We test the effects of 3 types of fertilizer and 2 different planting densities on crop yield.
ggboxplot(crop.data, x = "fertilizer", y = "yield", 
          color = "density", 
          order = c("1", "2", "3"),
          ylab = "Yield", xlab = "Fertilizer", add = "jitter")

two.way <- aov(yield ~ fertilizer + density, data = crop.data)

summary(two.way)
# both planting density and fertilizer have statistically significant effect (p-values < 0.001)

# Tukey Honest Significant Differences test
tukey.two.way <- TukeyHSD(two.way)

tukey.two.way
# there are statistically significant differences (p < 0.05) between 
# fertilizer groups 3 and 1 and between fertilizer types 3 and 2, but the
# difference between fertilizer groups 2 and 1 is not statistically significant.
# There is also a significant difference between the two different levels of planting density.

# test whether two variables have an interaction effect in ANOVA
interaction <- aov(yield ~ fertilizer*density, data = crop.data)

summary(interaction)
# there is no significant variation that can be explained by the interaction between fertilizer and planting density

tukey.interaction <- TukeyHSD(interaction)

tukey.interaction$fertilizer
tukey.interaction$density
tukey.interaction$`fertilizer:density`


### Alternatively
fit_lm3 <- lm(yield ~ fertilizer*density, data = crop.data)
summary(fit_lm3)
