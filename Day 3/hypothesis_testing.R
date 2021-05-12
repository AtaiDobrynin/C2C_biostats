##################################################
## Project: R Training
## Script purpose: Hypothesis Testing
## Author: Ege Ulgen
##################################################
# install.packages("ggpubr")
library(ggpubr)

# data to be used ---------------------------------------------------------
# Data in two numeric vectors
women_weight <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
men_weight <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4) 

# Create a data frame
my_data <- data.frame(gender = rep(c("female", "male"), each = 9),
                      weight = c(women_weight,  men_weight))
my_data


# one sample t-test -------------------------------------------------------
# test the null hypothesis that, on average, the weight is 60
weight <- my_data$weight

summary(weight)
sd(weight)

boxplot(weight)

# check normality assumption
qqnorm(weight)
qqline(weight)

ggqqplot(weight)

shapiro.test(weight)

# test the null hypothesis that, on average, the weight is 60
# perform t-test
res <- t.test(weight, mu = 60)
res 

res$p.value
res$conf.int

# test the null hypothesis that, on average, the weight is larger than 50
# One-tailed t-test
res <- t.test(weight, mu = 50, alternative = "greater")
res

# two sample t-test -------------------------------------------------------
# is the weight diffrent between males and females?
summary(my_data$weight[my_data$gender == "female"])
summary(my_data$weight[my_data$gender == "male"])

g <- ggboxplot(data = my_data,
               x = "gender", y = "weight",
               xlab = "Gender", ylab = "Weight",
               color = "gender")
g

# check normality assumption
shapiro.test(my_data$weight[my_data$gender == "female"])
shapiro.test(my_data$weight[my_data$gender == "male"])

# check variance equality
res_ftest <- var.test(weight ~ gender, data = my_data)
res_ftest
# there is no significant difference between the variances of the two sets of data

res <- t.test(weight ~ gender, data = my_data, var.equal = TRUE)
res

g + stat_compare_means(method = "t.test")

# paired t-test -----------------------------------------------------------
# Is there a difference between the weights of mice before and after treatment?
# Weight of the mice before treatment
before <- c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
# Weight of the mice after treatment
after <- c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
# Create a data frame
my_data <- data.frame(group = rep(c("before", "after"), each = 10),
                      weight = c(before,  after))

head(my_data)

g <- ggpaired(my_data, x = "group", y = "weight", 
              color = "group", palette = c("#00AFBB", "#E7B800"),
              order = c("before", "after"),
              ylab = "Weight", xlab = "Groups")
g

# Check normality of the differences
D <- my_data$weight[my_data$group == "before"] - my_data$weight[my_data$group == "after"]
D
shapiro.test(D)

res <- t.test(weight ~ group, data = my_data, paired = TRUE)
res

g + stat_compare_means(method = "t.test", paired = TRUE)

# is the weight of a mouse higher after treatment?
t.test(weight ~ group, data = my_data, paired = TRUE,
       alternative = "greater")

# Wilcox test -------------------------------------------------------------
?ToothGrowth
head(ToothGrowth)

# hypothesis: the tooth lengths are different between VC & OJ
summary(ToothGrowth$len[ToothGrowth$supp == "VC"])
summary(ToothGrowth$len[ToothGrowth$supp == "OJ"])

g <- ggboxplot(data = ToothGrowth,
               x = "supp", y = "len",
               xlab = "Supplement Type", ylab = "Tooth Length",
               color = "supp")
g

# check normality
shapiro.test(ToothGrowth$len[ToothGrowth$supp == "VC"])
shapiro.test(ToothGrowth$len[ToothGrowth$supp == "OJ"])

wilcox.test(len~supp, data = ToothGrowth)

g + stat_compare_means(method = "wilcox")

# chi-squared -------------------------------------------------------------
df <- read.csv("https://goo.gl/j6lRXD")

head(df)

table(df$treatment, df$improvement)

chisq.test(df$treatment, df$improvement)
# we reject the null hypothesis and conclude that the two variables are dependent

# a detailed tutorial on: http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r