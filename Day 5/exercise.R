# install.packages("ggpubr")

library(ggpubr)

# one-way ANOVA -----------------------------------------------------------

# creating data
# Consider the size for fish from 3 populations (n=12). 
# We want to use a model that will help us examine the question of whether 
# the mean fish size differs among populations.
fish_df <- data.frame(size = c(3,4,5,6,4,5,6,7,7,8,9,10),
                      pop = c("A","A","A","A","B","B","B","B","C","C","C","C"))
fish_df


g <- ggboxplot(fish_df, x = "pop", y = "size", 
          color = "pop", 
          order = c("A", "B", "C"),
          ylab = "Size", xlab = "Population", add = "jitter")

g


aov.model <- aov(size ~ pop, data = fish_df)
summary(aov.model)
# we can say that there is a significant difference between groups

g + stat_compare_means(method = "aov")


TukeyHSD(aov.model)
# There is a siginificant difference between group A-C and B-C

# exercise 2 --------------------------------------------------------------

# Time: Survival time of the animal
# poison: Type of poison used: factor level: 1,2 and 3
# treat: Type of treatment used: factor level: 1,2 and 3

df <- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/poisons.csv", row.names = 1)

head(df)
df$poison <- as.factor(df$poison)

ggboxplot(df, x = "poison", y = "time", 
          color = "poison", 
          order = c("1", "2", "3"),
          ylab = "Time", xlab = "Poison", add = "jitter")

anova_one_way <- aov(time~poison, data = df)
summary(anova_one_way)
#The p-value is lower than the usual threshold of 0.05. 
# we can say there is a statistical difference between the groups.

TukeyHSD(anova_one_way)
# There is a siginificant difference between poison 1-3 and 2-3

# two way ANOVA -----------------------------------------------------------
?ToothGrowth
my_data <- ToothGrowth

head(my_data)

# Convert dose as a factor and recode the levels
# as "D0.5", "D1", "D2"
my_data$dose <- factor(my_data$dose, 
                       levels = c(0.5, 1, 2),
                       labels = c("D0.5", "D1", "D2"))

my_data$supp <- factor(my_data$supp, levels = c("VC", "OJ"))

# contingency table
table(my_data$supp, my_data$dose)

ggboxplot(my_data, x = "dose", y = "len", color = "supp",
          palette = c("#00AFBB", "#E7B800"))

res.aov <- aov(len ~ supp + dose, data = my_data)
summary(res.aov)
# From the ANOVA table we can conclude that both supp and dose are statistically significant.

# Two-way ANOVA with interaction effect
res.aov2 <- aov(len ~ supp * dose, data = my_data)
summary(res.aov2)
# It can be seen that the two main effects (supp and dose) are statistically significant, as well as their interaction.


TukeyHSD(res.aov2, which = "dose")
# It can be seen from the output, that all pairwise comparisons are significant with an adjusted p-value < 0.05.


TukeyHSD(res.aov2, which = "supp:dose")
