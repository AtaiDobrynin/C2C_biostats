##################################################
## Project: R Training
## Script purpose: Linear and Logistic Regression
## Author: Ege Ulgen
##################################################

# Simple Linear Regression ------------------------------------------------
# The predictor vector.
height <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)

# The response vector.
weight <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

# Fit linear regression  model
fit <- lm(weight~height)
summary(fit)

# height_new <- height - min(height)
# fit <- lm(weight~height_new)

confint(fit)


# Find weight of a persons with heights 170, 160, 190.
new_data <- data.frame(height = c(170, 160, 190))
predict(fit, new_data)

# Plot the fitted line
plot(height, weight, col = "blue", pch = 20, 
     main = "Height & Weight Regression",
     ylab = "Weight in Kg", xlab = "Height in cm")
abline(fit)


# Multiple Linear Regression ----------------------------------------------
my_df <- data.frame(Weight = weight,
                    Height = height,
                    Age = c(25, 18, 30, 24, 21, 35, 25, 32, 19, 19))

fit2 <- lm(Weight ~ Age + Height, data = my_df)
summary(fit2)

fit2 <- lm(Weight ~ ., data = my_df)
summary(fit2)

# The regression now fitted a plane

# predict the weight 2 new individuals
new_data <- data.frame(Height = c(160, 180),
                       Age = c(25, 35))
predict(fit2, new_data)

###### better example
# from https://www.scribbr.com/statistics/linear-regression-in-r/
# in an imaginary sample of 500 towns
# outcome: the percentage of people with heart disease 
# predictor 1: the percentage of people biking to work each day 
# predictor 2: the percentage of people smoking
heart_df <- read.csv("heart_data.csv")

head(heart_df)
summary(heart_df)

### Check assumptions
# Independence of predictors
cor.test(heart_df$biking, heart_df$smoking)

# Normality of outcome
qqnorm(heart_df$heart.disease)
qqline(heart_df$heart.disease)

# Linearity
plot(heart.disease ~ biking, data = heart_df)
plot(heart.disease ~ smoking, data = heart_df)

### Fit linear regression model
fit3 <- lm(heart.disease ~ ., data = heart_df)
summary(fit3)
# for a town where the biking and smoking rate is 0%, the rate of heart disease is ~15%
# for every 1% increase in biking to work, there is a 0.2% decrease in the rate of heart disease
# for every 1% increase in smoking, there is a 0.178% increase in the rate of heart disease

# HD Rate = 14.98 - 0.2 * (biking rate) + 0.178 * (smoking rate) 

par(mfrow=c(2,2))
plot(fit3)
par(mfrow=c(1,1))

# logistic regression -----------------------------------------------------
# install.packages("mlbench")

library(mlbench)

# Load the data and remove NAs
data("PimaIndiansDiabetes2")
?PimaIndiansDiabetes2

head(PimaIndiansDiabetes2)

PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)

head(PimaIndiansDiabetes2)


table(PimaIndiansDiabetes2$diabetes)

fit4 <- glm(diabetes~., data = PimaIndiansDiabetes2, family = binomial)
summary(fit4)

# Accuracy
predicted_probs <- predict(fit4, newdata = PimaIndiansDiabetes2, type = "response")
predicted_probs
predicted_classes <- ifelse(predicted_probs > 0.5, "pos", "neg")

table(TRUTH = PimaIndiansDiabetes2$diabetes, PREDICTED = predicted_classes)
mean(predicted_classes == PimaIndiansDiabetes2$diabetes)

# coefficients
coef(fit4)
# odds ratios
exp(coef(fit4))

# the regression coefficient for glucose is 0.0383. 
# This indicates that one unit increase in the glucose concentration will 
# increase the odds of being diabetes-positive by exp(0.0383) 1.039 times.
