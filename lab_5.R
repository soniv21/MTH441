library(readxl)
library(mvtnorm)
ele_dat <- read_xlsx("Electricity_Data.xlsx")
head(ele_dat)

 ### a part ###
### performing regression analysis
model <- lm(ele_dat$Y ~ ele_dat$X)
summary(model)
residuals <- model$residuals
plot(model, pch = 16, which = 1)
qqnorm(residuals)

### b part ###
library(MASS)
boxcox <- boxcox(ele_dat$Y ~ 1)
head(boxcox)

### c part ###
# Extract the optimal lambda
lambda_optimal <- boxcox$x[which.max(boxcox$y)]

# Apply the Box-Cox transformation with the optimal lambda
y_transformed <- ifelse(lambda_optimal == 0, log(ele_dat), (ele_dat^lambda_optimal - 1) / lambda_optimal)
new_data <- data.frame( x = ele_dat$X,y_transformed  )
head(new_data)

### performing regression analysis on new data
new_model <- lm(new_data$c.10.7102842348935..8.71003067372486..11.7426165343071..9.9232828312622.. ~ new_data$x)
summary(new_model)

### calculating residuals for updated data
new_residuals <- new_model$residuals
plot(new_model, which =1)
qqnorm(new_residuals)
