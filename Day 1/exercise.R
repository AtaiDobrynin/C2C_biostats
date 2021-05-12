##################################################
## Project: R Training
## Script purpose: Introduction to R - Exercise
## Author: Ege Ulgen
##################################################

aids_df <- read.table("aids_dataset.txt")
head(aids_df)

# the first row is the header, containing the column names
aids_df <- read.table("aids_dataset.txt", header = TRUE)

dim(aids_df)

head(aids_df)

# overall summary
summary(aids_df)

# number of samples by treatment group
table(aids_df$treatment)

# number of samples by treatment group
table(aids_df$gender)


# histogram of baseline CD4 counts
hist(aids_df$cd4_1)

# histogram of week 2 CD4 counts
hist(aids_df$cd4_2)
hist(log(aids_df$cd4_2)) # ~normally distributed

# relation of baseline CD4 with age
plot(aids_df$age, aids_df$cd4_1)

# correlation?
cor(aids_df$age, aids_df$cd4_1)

# relation of second CD4 with age
plot(aids_df$age, aids_df$cd4_2)
# add axis labels
plot(aids_df$age, aids_df$cd4_2, xlab = "Age", ylab = "CD4_2")
# add color
plot(aids_df$age, aids_df$cd4_2, xlab = "Age", ylab = "CD4_2", col = "red")
# add plot title
plot(aids_df$age, aids_df$cd4_2, xlab = "Age", ylab = "CD4_2", col = "red", main = "Scatter Plot")


pdf("age_CD4_2.pdf", width = 10, height = 10)
plot(aids_df$age, aids_df$cd4_2, xlab = "Age", ylab = "CD4_2", col = "red", main = "Scatter Plot")
dev.off()

# compare CD4_1 and CD4_2
boxplot(aids_df$cd4_1, aids_df$cd4_2)

# CD4 counts by treatment group
boxplot(aids_df$cd4_1~aids_df$treatment)
boxplot(aids_df$cd4_2~aids_df$treatment)

# CD4 counts by gender
boxplot(aids_df$cd4_1~aids_df$gender)

# descriptive stat.s for baseline CD4 count of males
cd4_1_male <- aids_df$cd4_1[aids_df$gender == "male"]

mean(cd4_1_male)
sd(cd4_1_male)
median(cd4_1_male)

summary(cd4_1_male)

# histograms of baseline CD4 count of males and females
cd4_1_male <- aids_df$cd4_1[aids_df$gender == "male"]
cd4_1_female <- aids_df$cd4_1[aids_df$gender == "female"]

par(mfrow = c(1, 2)) # 1X2 layout
hist(cd4_1_male, main = "Male", border = "blue")
hist(cd4_1_female, main = "Female", border = "red")
par(mfrow = c(1, 1)) # reset layout



### Boxplot of 1 and 2 CD4 counts by treatment group
par(mfrow = c(2, 2)) # 2X2 layout

# compare CD4_1 and CD4_2 for trt1
sub_df <- aids_df[aids_df$treatment == "trt1", ]
boxplot(sub_df$cd4_1, sub_df$cd4_2, main = "trt1")

# compare CD4_1 and CD4_2 for trt2
sub_df <- aids_df[aids_df$treatment == "trt2", ]
boxplot(sub_df$cd4_1, sub_df$cd4_2, main = "trt2")

# compare CD4_1 and CD4_2 for trt3
sub_df <- aids_df[aids_df$treatment == "trt3", ]
boxplot(sub_df$cd4_1, sub_df$cd4_2, main = "trt3")

# compare CD4_1 and CD4_2 for trt4
sub_df <- aids_df[aids_df$treatment == "trt4", ]
boxplot(sub_df$cd4_1, sub_df$cd4_2, main = "trt4")

par(mfrow = c(1, 1)) # reset layout



# compare logs of CD4_1 and CD4_2 for each treatment group
par(mfrow = c(2, 2)) # 2X2 layout
for (trt in c("trt1", "trt2", "trt3", "trt4")) {
    sub_df <- aids_df[aids_df$treatment == trt, ]
    boxplot(log(sub_df$cd4_1), log(sub_df$cd4_2), main = trt, col = c("red", "blue"))
}
par(mfrow = c(1, 1)) # reset layout
