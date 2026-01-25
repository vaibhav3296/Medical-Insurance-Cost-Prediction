auto_raw = read.csv("https://www.statlearning.com/s/Auto.csv", header=T, na.string="?") 
Auto = na.omit(auto_raw)
Auto <- auto_raw
Auto$name <- NULL
model <- lm(mpg ~ ., data = Auto)
summary(model)
plot(model)

model_log <- lm(log(mpg) ~ log(cylinders) + log(displacement) + log(horsepower) + log(weight) + sqrt(acceleration) + year + origin, data = Auto)
summary(model_log)
model_sqrt <- lm(sqrt(mpg) ~ sqrt(cylinders) + sqrt(displacement) + sqrt(horsepower) + sqrt(weight) + sqrt(acceleration) + year + origin, data = Auto)
summary(model_sqrt)

model_with_interactions <- lm(mpg ~ cylinders * displacement + horsepower * weight + acceleration * year + origin, data = Auto)
summary(model_with_interactions)

new_vehicle <- data.frame(cylinders = 6, displacement = 300, horsepower = 130, weight = 3204, acceleration = 12, year = 80, origin = 3)
log_predicted_mpg <- predict(model_log, newdata = new_vehicle)
predicted_mpg <- exp(log_predicted_mpg)
predicted_mpg

