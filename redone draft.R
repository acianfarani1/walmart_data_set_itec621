# Cleaning the Data: 
#Structuring Dates in YYYY-MM-DD format 
library(lubridate)
walmart$Date <- ifelse(walmart$Date =="", NA, walmart$Date)
walmart$Date <- parse_date_time(walmart$Date, orders=c("mdy", "dmy","ymd"))
sum(is.na(walmart$Date))
walmart <- walmart[!is.na(walmart$Date),]
walmart$IsHoliday <- as.numeric(walmart$IsHoliday)
walmart$Type <- as.factor(walmart$Type)
walmart$Dept <- as.factor(walmart$Dept)
colSums(is.na(walmart))
walmart <- walmart[rowSums(is.na(walmart)) < 2, ]
walmart <- walmart[,-1]
print(paste("Number of Rows Remaining After Cleaning:", nrow(walmart)))

# 2. Dataset Overview: Structure
str(walmart)

# 3. Descriptive Analytics: 
summary(walmart)
# Model 1: Full Model 
walmart_cor <- cor(walmart[, sapply(walmart, is.numeric)])
walmart_cor
library(corrplot)
corrplot(walmart_cor, method="number", order="hclust", tl.cex = 0.5)

full.anova <- aov(Weekly_Sales ~ ., data = walmart)
summary(full.anova)

# 5. Model Analysis 
# Model 1: OLS 
#Basic model specification 1:
ols.business <- lm(Weekly_Sales ~ Fuel_Price + CPI + Unemployment + IsHoliday, data = walmart)
summary(ols.business)

ols.mse.1 <- mean(residuals(ols.business)^2)
ols.mse.2 <- mean(residuals(ols.business.2)^2)
cbind("OLS Simple Model MSE" = ols.mse.1, "OLS Log-Transformed MSE" = ols.mse.2)

#OLS Assumptions tests 

# Load required libraries
library(tidyverse)
library(car)       # For VIF and linearity checks
library(lmtest)    # For heteroscedasticity tests
library(nortest)   # For normality tests

#OLS 1 - YC - PASSES
#OLS 2 - EN - FAILED - extreme deviation at the right tail; histogram is skewed right. 
qqnorm(residuals(ols.business))
qqline(residuals(ols.business), col="red")
hist(residuals(ols.business), main="Histogram", xlab="Residuals")
#OLS 3 - XI - PASSES
vif.ols <- vif(ols.business)
print(vif.ols)
#OLS 4 - LI - FAILED - residuals are not randomly scattered around the intercept; clustered pattern 
plot(fitted(ols.business), residuals(ols.business), xlab="Fitted Values", ylab="Residuals", main="Residuals vs. Fitted Plot")
abline(h=0, col="red")
#OLS 5 - OI - PASSES
dw_test <- dwtest(ols.business)
print(dw_test)
#OLS 6 - EI - PASSES - similar to DW Test
#OLS 7 - EA - PASSES 
mean_residuals <- mean(residuals(ols.business))
print(mean_residuals)
#OLS 8 - EV - FAILED - p value less than 0.05 
bptest1 <- bptest(ols.business)
print(bptest1)

# Enhanced OLS Model with Interaction and Transformation specification 2:
ols.business.2 <- lm(log(Weekly_Sales + abs(min(walmart$Weekly_Sales, na.rm = TRUE)) + 1) ~ Fuel_Price + IsHoliday + CPI + Unemployment, data = walmart)
summary(ols.business.2)

#OLS 1 - YC - PASSES
#OLS 2 - EN - PASSES 
qqnorm(residuals(ols.business.2))
qqline(residuals(ols.business.2), col="red")
hist(residuals(ols.business.2), main="Histogram", xlab="Residuals")
#OLS 3 - XI - PASSES
vif.ols.2 <- vif(ols.business.2)
print(vif.ols.2)
#OLS 4 - LI - PASSES 
plot(fitted(ols.business.2), residuals(ols.business.2), xlab="Fitted Values", ylab="Residuals", main="Residuals vs. Fitted Plot")
abline(h=0, col="red")
#OLS 5 - OI - PASSES
dw_test.2 <- dwtest(ols.business.2)
print(dw_test.2)
#OLS 6 - EI - PASSES - similar to DW Test
#OLS 7 - EA - PASSES 
mean_residuals.2 <- mean(residuals(ols.business.2))
print(mean_residuals.2)
#OLS 8 - EV - PASSES 
bptest2 <- bptest(ols.business.2)
print(bptest2)

# Model 2: Ridge Regression
# Basic model specification 1: 
x.r.1 <- model.matrix(Weekly_Sales ~ Fuel_Price + CPI + Unemployment + IsHoliday, data = walmart)[, -1]
y.r.1 <- walmart$Weekly_Sales

set.seed(1)
library(glmnet)
ridge.business <- cv.glmnet(x.r.1, y.r.1, alpha = 0) 
plot(ridge.business)

best.lambda.1 <- ridge.business$lambda.min
min.cv.mse <- min(ridge.business$cvm)
cbind("Best Lambda" = best.lambda.1)

ridge.coef.best <- coef(ridge.business, s=best.lambda.1)
colnames(ridge.coef.best) <- c("Best Ridge")
print(ridge.coef.best)

predict(ridge.business, s=best.lambda.1, type="coefficients")

ridge.pred.1 <- predict(ridge.business, s = best.lambda.1, newx = x.r.1)
ridge.mse.1 <- mean((y.r.1 - ridge.pred.1)^2)
ridge.pred.2 <- predict(ridge.business.2, s = best.lambda.2, newx = x.r.2)
ridge.mse.2 <- mean((y.r.2 - ridge.pred.2)^2)
cbind("Ridge Simple Model MSE" = ridge.mse.1, 
      "Ridge Log-Transformed MSE" = ridge.mse.2)


# Enhanced Model with Interaction and Transformation specification 2:
x.r.2 <- model.matrix(log(Weekly_Sales + abs(min(walmart$Weekly_Sales, na.rm = TRUE)) + 1) ~ Fuel_Price + CPI + Unemployment + IsHoliday, data = walmart)[, -1]
y.r.2 <- log(walmart$Weekly_Sales + abs(min(walmart$Weekly_Sales, na.rm = TRUE)) + 1)

set.seed(1)
library(glmnet)
ridge.business.2 <- cv.glmnet(x.r.2, y.r.2, alpha = 0) 
plot(ridge.business.2)

best.lambda.2 <- ridge.business.2$lambda.min
min.cv.mse.2 <- min(ridge.business.2$cvm)
cbind("Best Lambda" = best.lambda.2, "Best Log Lambda" = log(best.lambda.2),"Best Log Lambda" = log(best.lambda.1), "Best 10-Fold CV MSE" = min.cv.mse.2)

ridge.coef.best.2 <- coef(ridge.business.2, s = best.lambda.2)
colnames(ridge.coef.best.2) <- c("Best Ridge (Log Transformed)")
print(ridge.coef.best.2)

predict(ridge.business.2, s=best.lambda.2, type="coefficients")


#Model 3: LASSO 
# Basic model specification 1: 
x <- model.matrix(Weekly_Sales ~ Fuel_Price + CPI + Unemployment + IsHoliday, data = walmart)[, -1]
y <- walmart$Weekly_Sales

set.seed(1)
library(glmnet)
lasso.business <- cv.glmnet(x, y, alpha = 1) 
plot(lasso.business)

best.lasso.lambda.1 <- lasso.business$lambda.min
min.cv.mse.1.lasso <- min(lasso.business$cvm)
cbind("Best Lambda" = best.lasso.lambda.1, "Best Log Lambda" = log(best.lasso.lambda.1), "Best 10-Fold CV MSE" = min.cv.mse.1.lasso)

lasso.coef.best.1 <- coef(lasso.business, s = best.lasso.lambda.1)
colnames(lasso.coef.best.1) <- c("Best LASSO")
print(lasso.coef.best.1)

# Enhanced Model with Interaction and Transformation specification 2:
x <- model.matrix(log(Weekly_Sales + abs(min(walmart$Weekly_Sales, na.rm = TRUE)) + 1) ~ Fuel_Price + CPI + Unemployment + IsHoliday, data = walmart)[, -1]
y <- log(walmart$Weekly_Sales + abs(min(walmart$Weekly_Sales, na.rm = TRUE)) + 1)

lasso.business.2 <- cv.glmnet(x, y, alpha = 1) 
plot(lasso.business.2)

best.lasso.lambda.2 <- lasso.business.2$lambda.min
min.cv.mse.2.lasso <- min(lasso.business.2$cvm)
cbind("Best Lambda" = best.lasso.lambda.2,"Best Log Lambda" = log(best.lasso.lambda.2), "Best 10-Fold CV MSE" = min.cv.mse.2.lasso)

lasso.coef.best.2 <- coef(lasso.business.2, s = best.lasso.lambda.2)
colnames(lasso.coef.best.2) <- c("Best LASSO (Log Transformed)")
print(lasso.coef.best.2)


min.cv.mse.1.ridge <- min(ridge.business$cvm)
min.cv.mse.2.ridge <- min(ridge.business.2$cvm)
cbind("Ridge Simple Model CV MSE" = min.cv.mse.1.ridge, 
      "Ridge Log-Transformed CV MSE" = min.cv.mse.2.ridge)

min.cv.mse.1.lasso <- min(lasso.business$cvm)
min.cv.mse.2.lasso <- min(lasso.business.2$cvm)
cbind("LASSO Simple Model CV MSE" = min.cv.mse.1.lasso, 
      "LASSO Log-Transformed CV MSE" = min.cv.mse.2.lasso)

