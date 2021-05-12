install.packages("ggpubr")
library(ggpubr)

# Two-Sample t-Tests -------------------------------------------------------
?sleep
head(sleep)

table(sleep$group)

#The 2 sample t-test is used when you want to compare two independent groups.
#Suppose we want to test if the average increased sleep with two different drugs are equal, using the sleep dataset and t.test function.

mean(sleep$extra[sleep$group == "1"])
sd(sleep$extra[sleep$group == "1"])

mean(sleep$extra[sleep$group == "2"])
sd(sleep$extra[sleep$group == "2"])

# check normality assumption
shapiro.test(sleep$extra[sleep$group == "1"])
shapiro.test(sleep$extra[sleep$group == "2"])

g <- ggboxplot(data = sleep, x="group", y = "extra", color = "group")
g

# check variance equality
sleep_ftest <- var.test(extra ~ group, data = sleep)
sleep_ftest

# there is no significant difference between the variances of the two sets of data


# perform t test - is the mean "extra" different between groups?
sleep1 <- t.test(extra ~ group, data = sleep, var.equal = TRUE)
sleep1

g + stat_compare_means(method = "t.test")

# Paired t-Test -------------------------------------------------------
# Check normality of the differences
D <- sleep$extra[sleep$group == "1"] - sleep$extra[sleep$group == "2"]
D
shapiro.test(D)

g <- ggpaired(data = sleep, x="group", y = "extra", color = "group")
g + stat_compare_means(method = "wilcox", paired = TRUE)

wilcox.test(extra~group, data = sleep, paired = TRUE)


# chi-squared -------------------------------------------------------------
aids_df <- read.table("aids_dataset.txt", header = TRUE)

head(aids_df, 2)

# is there an association between gender and treatment group?
# i.e. does the gender distribution differ between treatment groups?
table(aids_df$treatment, aids_df$gender)

tbl <- table(aids_df$treatment, aids_df$gender)
tbl
tbl / rowSums(tbl) * 100

chisq.test(tbl)

# the gender distribution does not differ between treatment groups
