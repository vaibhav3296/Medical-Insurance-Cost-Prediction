###############################
# MIS/BA649 Week 3 
# EDA
################################

# PartI: R Pages 33-82

# Chapter 8 Probability Distribution
##'d'-density, f(x), 'p'-CDF, F(x)=P(X<=x), 'q'-quartile, 'r'-random for simulations
## 2-tailed p-value for t distribution
#pt(-2, df = 100) #Prob(t(df=30)<= - 2)
2*pt(-2.43, df=13)

## upper 1% point for an F(2,7) distribution
qf(.01, 2, 7, lower.tail = FALSE)

## explore a set of univariate data
attach(faithful)
summary(faithful)
summary(eruptions)
fivenum(eruptions)  #why the 2nd # is slightly different?
stem(eruptions)
hist(eruptions)
hist(eruptions, seq(1.6, 5.2, 0.2), prob=T)
lines(density(eruptions, bw=.1))
rug(eruptions) # show the actual data points

plot(ecdf(eruptions), do.points=F, verticals=T)
long = eruptions[eruptions > 3]
plot(ecdf(long), do.points=F, verticals=T)
x = seq(3, 5.4, 0.01)
lines(x, pnorm(x,mean=mean(long), sd=sqrt(var(long))), lty=3)

#Q-Q plot
par(pty="s") #arrange for a squre figure region
qqnorm(long); qqline(long)

x = rt(250, df = 5)
qqnorm(x); qqline(x)
qqplot(qt(ppoints(250), df=5), x, xlab="Q-Q plot for t_df=5")
qqline(x)
shapiro.test(long)
ks.test(long, "pnorm", mean=mean(long), sd=sqrt(var(long)))

#2-sample t test: 
A = rnorm(100, 0, 1)
B = rnorm(120, 0.2, .9)
boxplot(A, B)
t.test(A, B)
var.test(A, B)
t.test(A, B, var.equal=T)
t.test(A, B, var.equal=F)
wilcox.test(A, B)
plot(ecdf(A), do.points=F, verticals=T, xlim=range(A,B))
plot(ecdf(B), do.points=F, verticals=T, add=T)
ks.test(A, B)


########################### EDA ############################
# Step 1: data set and objectives
##- what are the variables (name, description, type) involved? 
##- # of observations (n)?
##- what are the objectives for business analytics in this case?
# Step 2: univariate EDA (quantitative; qualitative)
##- any missing values? 
##- numerical summary?
##- graphical summary?
##- any outliers? any findings?
# Step 3: bivariate EDA (quant+quant; quant+qual; qual+qual)
##- numerical summary?
##- graphical summary?
##- any outliers? any findings?
# Others
##############################################################

auto_raw = read.csv("https://www.statlearning.com/s/Auto.csv", header=T, na.string="?")
read.csv("Auto.csv",header=T)
getwd()
read.table

#getwd()
#auto_raw = read.csv("Auto.csv", header=T, na.string="?")

summary(auto_raw)
dim(auto_raw)

Auto = na.omit(auto_raw)
length(auto_raw$cylinders)
summary(auto)
str(auto)
dim(auto)
read.table
range(auto$mpg)
range(auto$displacement, na.rm = TRUE)
range(auto$horsepower, na.rm = TRUE)
range(auto$weight)
range(auto$acceleration)
range(auto$displacement)
range(auto$year)
sd(auto$mpg)
sd(auto$mpg, na.rm = TRUE)
sd(auto$displacement, na.rm = TRUE)
sd(auto$horsepower, na.rm = TRUE)
sd(auto$weight, na.rm = TRUE)
sd(auto$acceleration, na.rm = TRUE)
sd(auto$year, na.rm = TRUE)

auto <- auto[-c(10:85),]

# Select only numeric predictors
numeric_predictors <- auto[, c("mpg", "year", "horsepower", "displacement", "weight", "acceleration")]

# Calculate range
range_stats <- sapply(numeric_predictors, function(x) range(x, na.rm = TRUE))

# Calculate mean
mean_stats <- sapply(numeric_predictors, function(x) mean(x, na.rm = TRUE))

# Calculate standard deviation
sd_stats <- sapply(numeric_predictors, function(x) sd(x, na.rm = TRUE))



summary(numeric_predictors)
length(numeric_predictors)
length(numeric_predictors$mpg)


# Print results
print("Range:")
print(range_stats)

print("Mean:")
print(mean_stats)

print("Standard Deviation:")
print(sd_stats)

boxplot(auto_raw$cylinders, auto_raw$mpg)
plot(auto$mpg, auto$horsepower, xlab = "MPG", ylab = "Horsepower")

plot(auto$mpg, auto$displacement, xlab = "MPG", ylab = "Displacement")



mpg_by_year_base <-tapply(auto$mpg,auto$year, mean, na.rm = TRUE)
print(mpg_by_year_base)


plot(names(mpg_by_year_base), mpg_by_year_base, type = "o", col = "blue", xlab = "Year", ylab = "Average MPG", main = "Average MPG Over Time")

View(auto_raw)

plot(cylinders, mpg)
plot(Auto$cylinders, Auto$mpg)
attach(auto)
length(cylinders)
name_list=table(Auto$name)
name_list[1:10]
vehicle_year=table(name,year)
summary(vehicle_year)
detach(Auto)
plot(cylinders, mpg)
cylinders=as.factor(cylinders)
plot(cylinders, mpg)
boxplot(mpg ~ cylinders)
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red", varwidth=T)
plot(cylinders, mpg, col="red", varwidth=T,horizontal=T)
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG")
hist(mpg)
hist(mpg,col=2)
hist(mpg,col=2,breaks=15)
?hist

hist(cylinders)
table(cylinders)
year_freq=table(year)
boxplot(cylinders)

barplot(year)
barplot(year_freq)
year_freq=table(year)
origin_freq=table(origin)
origin_freq
barplot(origin_freq)

cor(cylinders,mpg)
plot(cylinders,mpg)
boxplot(mpg ~ cylinders)
plot(weight,mpg)
plot(mpg,weight)
cor(mpg,weight)
origin = as.factor(origin)
boxplot(mpg ~ origin)
table(origin,cylinder)
#how to graphical show two 




hist(cylinders,breaks=5)
hist(year,breaks = 13) #not correct


hist(mpg,col=2,breaks=10)
pairs(Auto[,1:8])
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto[,1:8])
plot(horsepower,mpg)
#identify(horsepower,mpg,name)
summary(Auto)
summary(mpg)





