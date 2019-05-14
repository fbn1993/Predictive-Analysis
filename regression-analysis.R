# example uses "women" dataset
# that contains height and weight of 15 women aged 30 - 39
str(women)
simple_linear_model <- lm(weight ~ height, data = women)
simple_linear_model
# Shows intercept and beta coefficient for height variable
# ie weight = -87.52 + 3.45 X height

plot(women$height,
     women$weight,
     xlab = "Height (inches)",
     ylab = "Weight (lbs)",
     main = "Scatter Plot showing regression line
              for weight predicted from height")
abline(simple_linear_model)
summary(simple_linear_model)    

confint(simple_linear_model)

cor(women$height, women$weight)
# Measures the level of the association between 2 variables
# and it ranges from -1 (perfect negative correlation) to
# +1 (perfect positive correlation)
# A value close to 0 indicates a weak relationship
# A low correlation (-0.2 < x < 0.2) suggests that much of the
# variation of the outcome variable is not explained by the predictor
# In such case, we should then look at better predictor variables

# Lets examine the goodness of fit
summary(simple_linear_model)


# Cars dataset
library(car)
head(cars)

# Visualize if there are any linear relationships between the 
# independent and dependent variables
scatter.smooth(x = cars$speed,
               y = cars$dist,
               main = "Distance ~ Speed",
               xlab = "Car Speed",
               ylab = "Stopping Distance")

par(mfrow = c(1,2)) # divides graph area into 2 cols
boxplot(cars$speed, 
        main = "Car Speed", 
        sub = paste("Outlier rows",boxplot.stats(cars$speed)$out))
boxplot(cars$dist, 
        main = "Stopping Distance", 
        sub = paste("Outlier rows",boxplot.stats(cars$dist)$out))

install.packages("e1071")
library(e1071)

par(mfrow = c(1,2)) # divides graph area into 2 cols

plot(density(cars$speed), 
     main = "Density plot : speed",
     ylab = "Frequency",
     sub = paste("Skewness : ", round(e1071::skewness(cars$speed), 2)))

# lets fill in the area under the density plot
polygon(density(cars$speed),col  = "red")

plot(density(cars$dist), 
     main = "Density plot : stopping distance",
     ylab = "Frequency",
     sub = paste("Skewness : ", round(e1071::skewness(cars$dist), 2)))

# lets fill in the area under the density plot
polygon(density(cars$dist),col  = "red")

cor(cars$speed, cars$dist)


# Build the linear regression model on full data
linearmod <- lm(dist ~ speed, data = cars)
summary(linearmod)
print(linearmod)

model_summary <- summary(linearmod)
# model coefficients
model_coeffs <- model_summary$coefficients
model_coeffs

AIC(linearmod)
BIC(linearmod)

# --------------------------------
set.seed(200)

# select random sample 
#  from 1:all records in cars, with 80 % of rows

total_records <- sample(1:nrow(cars), 0.8*nrow(cars))

training_data <- cars[total_records,]
training_data

testing_data <- cars[-total_records,]
testing_data

# Build the model using the training data
lr_model <- lm(dist ~ speed, data = training_data)

# Model Summary
summary(lr_model)

# predict distance from testing data
dist_predicted <- predict(lr_model, testing_data)

# make an actuals vs predicted dataframe
actuals_pred <- data.frame(actuals = testing_data$dist,
                           predicted= dist_predicted)
actuals_pred

correlation_accuracy <- cor(actuals_pred)
correlation_accuracy

# min-max accuracy

min_max_accuracy <- mean(apply(actuals_pred, 1, min) / 
                           apply(actuals_pred, 1, max))
min_max_accuracy

# Multiple linear regression -------
# We use the ":" to indicate an interaction between
# predictor variables
multiple_linear_model <- lm(mpg ~ hp + wt + hp:wt, data = mtcars)
summary(multiple_linear_model)

# We can visulaize interaction using the effects() function
# This means we can change the values for wt and view the changes
# graphically
install.packages("effects")
library(effects)
plot(effect("hp:wt", 
            multiple_linear_model,, 
            list(wt = c(2.2, 3.2, 4.2))),
     multiline = TRUE)

# Evaluate the statistical assumption using the plot()
# function.
par(mfrow = c(2,2))
plot(multiple_linear_model)

install.packages("gvlma")
library(gvlma)
gv_model <- gvlma(multiple_linear_model)
summary(gv_model)
