##################################################
## Project: R Training
## Script purpose: Introduction to R
## Author: Ege Ulgen
##################################################

# Sample Session ----------------------------------------------------------

# This is a comment
# print a number
2 

2 + 3 # perform a simple calculation

# natural log
log(2)

# working directory -------------------------------------------------------
# Return current working directory
getwd()

# Set working directory
setwd("~/Documents/Sezerman_Lab/Teaching/stat_course/R_training/1.Introduction/")

# special characters in R -------------------------------------------------

# NA: Not Available (i.e. missing values)
# NaN: Not a Number (e.g. 0/0)
0/0
# Inf: Infinity
# -Inf: Minus Infinity. For instance 0 divided by 0 gives a NaN, but 1 divided by 0 gives Inf.
1/0

# numeric and string objects ----------------------------------------------
### Numeric objects
# store an object
x <- 2
x # print this object

# store and print an object
(y <- 3)

# perform simple calculations
x + y
x * 3 + y * 2

# update an object to be doubled
x <- 2 * x

### String objects
x <- "Hello"
x

(y <- "World")

# concatenate the two words
paste(x, y)

# vectors -----------------------------------------------------------------
# store a vector
height <- c(168, 177, 177, 177, 178, 172, 165, 171, 178, 170)

# print the second element
height[2]

# print from the 2nd to the 5th element
height[2:5]

# Define a vector as a sequence (1 to 10)
(obs <- 1:10)
# define a regular sequence
seq(1, 10)
seq(1, 10, by = 2)

# Perform a simple calculation using vectors (element-wise)
weight <- c(88, 72, 85, 52, 71, 69, 61, 61, 51, 75)

BMI <- weight / ((height / 100) ^ 2)
BMI

### simple descriptions of the vector
# length
length(height)

# sample mean
mean(height)
?mean

# sample variance/standard deviation
(s2 <- var(height))
sqrt(s2)

sd(height)

# median, range
median(height)

range(height)

# quantiles
#Q1
quantile(height, 0.25)

#Q3
quantile(height, 0.75)

# 33rd and 66th percentiles
quantile(height, c(0.33, 0.66))

# summary
summary(height)

# correlation
cor(height, weight)
cor(height, weight, method = "spearman")

cor.test(height, weight)

### string vectors
eye_color <- c("blue", "green", "brown", "blue", "brown", 
               "blue", "blue", "green", "brown", "brown")

table(eye_color)

### appending to and deleting from a vector
# empty vector
my_vec <- c()
# append to vector
my_vec <- c(my_vec, 3)
my_vec
my_vec <- c(1, 2, my_vec)
my_vec

# delete an element
my_vec <- my_vec[-2]
my_vec

# simple plotting ---------------------------------------------------------
### scatter plot 
plot(height, weight, xlab = "Height", ylab = "Weight")

# equivalently:
plot(weight~height, xlab = "Height", ylab = "Weight")

# save bar plot as png
png("scatter.png", width = 480, height = 480)
plot(weight~height, xlab = "Height", ylab = "Weight")
dev.off()

# save bar plot as pdf
pdf("scatter.pdf", width = 6, height = 6)
plot(weight~height, xlab = "Height", ylab = "Weight")
dev.off()

plot(height, weight, xlab = "Height", ylab = "Weight", col = "red")

plot(1:20, pch = 1:20)
plot(height, weight, xlab = "Height", ylab = "Weight", pch = 19)

###  histogram of weight
hist(weight)

###  boxplot
boxplot(height)
boxplot(height, weight)


### bar plot
freq_tbl <- table(eye_color)
barplot(freq_tbl, 
        xlab = "Eye-colour",
        ylab = "Frequency",
        main = "Bar-plot of the eye-colour data")

barplot(freq_tbl, 
        xlab = "Eye-colour",
        ylab = "Frequency",
        main = "Bar-plot of the eye-colour data",
        col = c("blue", "brown", "green"))

# matrices ----------------------------------------------------------------
# create an all-zero matrix of 3 x 5
M0 <- matrix(0, nrow = 3, ncol = 5)
M0

# create aa matrix combining the previous variables rowwise
M <- rbind(obs, height, weight, BMI)
M

# create aa matrix combining the previous variables columnwise
M <- cbind(obs, height, weight, BMI)
M

class(M)
is.matrix(M)
typeof(M)
dim(M)

# data frames -------------------------------------------------------------
df <- as.data.frame(M)

class(df)
colnames(df)

# descriptive statistics
summary(df)

boxplot(df$BMI)

# reading/saving data -----------------------------------------------------
aids_df <- read.table("aids_dataset.txt", header = TRUE)
# some_df <- read.delim("/path/to/file.tsv")
# some_df <- read.csv("/path/to/file.csv")

write.table(aids_df, "aids_dataset.txt", sep = " ")

# loops and conditionals --------------------------------------------------
# print numbers from 1 to 100
for (i in 1:100) {
  print(i)
}

# store numbers from 1 to 100
my_vec <- c()
for (i in 1:100) {
  my_vec <- c(my_vec, i)
}
my_vec

# store numbers from 100 to 1
my_vec <- c()
for (i in 1:100) {
  my_vec <- c(i, my_vec)
}

# store numbers from 1 to 100, only for numbers between 30 & 50 (both included)
5 == 5
5 != 5
5 != 6
6 < 10

(5 == 5) & (4 < 3)
(5 == 5) | (4 < 3)

my_vec <- c()
for (i in 1:100) {
  if (i >= 30 & i <= 50) {
    my_vec <- c(my_vec, i)
  }
}
my_vec

# For numbers from 1 to 100, store multiples of 3 as -the number, other as as normal
6 %% 3
4 %% 3

my_vec <- c()
for (i in 1:100) {
  if (i %% 3 == 0) {
    my_vec <- c(my_vec, -i)
  } else {
    my_vec <- c(my_vec, i)
  }
}
my_vec

### Fizz-buzz
# Print numbers from 1 to 100, if the number is a multiple of 3, print Fizz,
# if the number is a multiple of 5, print Buzz, if the number is a multiple of 
# both 3 & 5 print FizzBuzz

for (i in 1:100){
  
  if (i %% 3 == 0 & i %% 5 == 0) {
    print("FizzBuzz")
  }
  else if (i %% 3 == 0) {
    print("Fizz")
  }
  else if (i %% 5 == 0){
    print("Buzz")
  } else {
    print(i)
  }
  
}


# working with data frames ------------------------------------------------
# Results obtained from an experiment to compare yields (as measured by dried 
# weight of plants) obtained under a control and two different treatment conditions
data("PlantGrowth")
PlantGrowth

nrow(PlantGrowth)
ncol(PlantGrowth)

table(PlantGrowth$group)

summary(PlantGrowth$weight)

### first and last parts
# (default) first 6 rows
head(PlantGrowth)
# first 2 rows
head(PlantGrowth, 2)

# last 6 rows
tail(PlantGrowth)

### subsetting
# specific cell
PlantGrowth[4, 1]
PlantGrowth$weight[4]

# 4-12th rows
PlantGrowth[4:12, ]
PlantGrowth[4:12, 2]

# subset the "trt2" group
subset(PlantGrowth, group == "trt2")

# indices of plants in the "ctrl" group
PlantGrowth$group == "trt2"
which(PlantGrowth$group == "trt2")

# weights of plants in the "trt2" group
PlantGrowth$weight[PlantGrowth$group == "trt2"]
PlantGrowth[PlantGrowth$group == "trt2", 1]
PlantGrowth[PlantGrowth$group == "trt2", "weight"]

# weights of plants whose weight is between 5 - 5.5
PlantGrowth$weight[PlantGrowth$weight >= 5 & PlantGrowth$weight <= 5.5]

# ROWS whose weight is between 5 - 5.5
PlantGrowth[PlantGrowth$weight >= 5 & PlantGrowth$weight <= 5.5, ]


### mean weight by group
mean_by_gr <- c(ctrl = mean(PlantGrowth$weight[PlantGrowth$group == "ctrl"]),
                trt1 = mean(PlantGrowth$weight[PlantGrowth$group == "trt1"]),
                trt2 = mean(PlantGrowth$weight[PlantGrowth$group == "trt2"]))
barplot(mean_by_gr)
barplot(mean_by_gr, ylim = c(0, 6))

# distribution of weight by group
boxplot(PlantGrowth$weight~PlantGrowth$group)
boxplot(PlantGrowth$weight~PlantGrowth$group, col = c("red", "blue", "orange"))
