#####################################
## BA_MIS649_Ch3 Linear Regression
#####################################

#simulation for simple linear regression
set.seed(23456)
epsilon = rnorm(100, 0, 20)
X = runif(100,-2,2)
Y = 2 + 3*X + epsilon
plot(X,Y)

#adding the population/true regression line 
x = c(-2:2, 0.05)
y = 2 + 3*x
lines(x, y, col = "red", lwd = 2, lty = 1)

#adding the sample regression line
lm(formula = Y ~ X)
abline(lm(Y ~ X), col = "blue")


#Advertising: Sales vs. TV
advertising = read.csv("https://www.statlearning.com/s/Advertising.csv", header=T, na.string="?")
summary(advertising)
attach(advertising)
model1 = lm(sales ~ TV)
summary(model1)

#Multiple Linear Regression
cor(advertising[,-1]) #correlation matrix
model2 = lm(sales ~ TV + radio + newspaper)
summary(model2)

#Predictor with Only Two Levels (Credit data)
credit = read.csv("https://www.statlearning.com/s/Credit.csv", header=T, na.string="?")
summary(credit)
plot(credit[c(11, 5, 4, 6, 1, 2, 3)])
attach(credit)
model3 = lm(Balance ~ Student)
summary(model3)

#Predictor with More Than Two Levels (Credit data)
model4 = lm(Balance ~ Region)
summary(model4)

#Mixed Predictors (Credit data)
model5 = lm(Balance ~ Income + Limit + Rating + Cards + Age + Education + Student + Married + Region)
summary(model5)

#Extension of Linear Model (Advertising data) 
##- transformations and interaction terms
attach(advertising)
model6 = lm(sales ~ TV + radio + radio:TV + I(log(newspaper)))
summary(model6)

#Plot diagnosis
par(mfrow=c(2,2))
plot(model6)


