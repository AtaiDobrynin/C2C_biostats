##################################################
## Project: R Training
## Script purpose: Regression - Exercises
## Author: Ege Ulgen
##################################################

library(ggpubr)

# Linear regression -------------------------------------------------------
# The main aim of collecting this data set was to inspect the association 
# between prostate-specific anti- gen (PSA) and prognostic clinical measurements 
# in men with advanced prostate cancer. Data were collected on 97 men who were 
# about to undergo radical prostectomies.
prca_df <- read.csv("prostate_cancer.csv")

head(prca_df)

hist(prca_df$PSA)
hist(prca_df$PSA, breaks = 15)

## check normality of dependent variable
ggqqplot(prca_df$PSA)
# the data quantiles deviates from the normal distribution quantiles
shapiro.test(prca_df$PSA)

# For this reason, take the natural log values of the PSA levels, and again test for normality
prca_df$log_PSA <- log(prca_df$PSA)
ggqqplot(prca_df$log_PSA)
shapiro.test(prca_df$log_PSA)

### Relationship between prostate volume and PSA levels
plot(PSA~vol, prca_df)
cor(prca_df$PSA, prca_df$vol)

### Is there any difference between patients who had seminal vesicle invasion 
# and who had not with regards the PSA levels?
var.test(prca_df$log_PSA~prca_df$invasion)
# the variances are not significantly different

t.test(prca_df$log_PSA~prca_df$invasion, var.equal = TRUE)

### What is the effect of Gleason score on PSA levels?
table(prca_df$Gleason)
prca_df$Gleason <- as.factor(prca_df$Gleason)

boxplot(log_PSA~Gleason, prca_df)

fit_gleason <- lm(log_PSA~Gleason, data = prca_df)
summary(fit_gleason)

# change reference level
prca_df$Gleason
?relevel()
prca_df$Gleason <- relevel(prca_df$Gleason, ref = 2)
prca_df$Gleason

fit_gleason2 <- lm(log_PSA~Gleason, data = prca_df)
summary(fit_gleason2)

# re-level
prca_df$Gleason <- relevel(prca_df$Gleason, ref = 2)
prca_df$Gleason

# interaction of age with Gleason score
fit_gleason3 <- lm(log_PSA~Gleason*age, data = prca_df)
summary(fit_gleason3)

# the effect of age when gleason score = 6 >>>> 0.0351
# the effect of age when gleason score = 7 >>>> 0.0351 + (-0.0177)
# the effect of age when gleason score = 8 >>>> 0.0351 + (-0.0704)


### What are important factors that have an effect on PSA levels?
fit0 <- lm(log_PSA~vol + wt + age + BPH + invasion + penetration + Gleason, data = prca_df)
summary(fit0)

## keeping only significant variables, fit another model
fit1 <- lm(log_PSA~vol + BPH + invasion + Gleason, data = prca_df)
summary(fit1)


# Logistic regression -----------------------------------------------------
# demographic and survival information for the passengers in the Titanic shipwreck
titanic_data <- read.csv("titanic_data.csv")

head(titanic_data)

titanic_data$CLASS <- factor(titanic_data$CLASS, levels = c("crew", "1st", "2nd", "3rd"))
titanic_data$AGE <- factor(titanic_data$AGE, levels = c("adult", "child"))
titanic_data$SEX <- factor(titanic_data$SEX, levels = c("male", "female"))
titanic_data$SURVIVED <- factor(titanic_data$SURVIVED, levels = c("no", "yes"))


logit_fit <- glm(SURVIVED ~ ., data = titanic_data, family = binomial)
summary(logit_fit)

exp(coef(logit_fit))
