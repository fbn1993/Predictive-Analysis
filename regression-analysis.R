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
     main = "Scatter plot showing regrssion line for
              weight predicted from height")
abline(simple_linear_model)