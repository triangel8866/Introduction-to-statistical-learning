#8
college = read.csv("College.csv", header = T)
fix(college)
rownames(college) = college[,1]
fix(college)
college = college[,-1]
fix(college)
summary(college)
pairs(college[,1:10])
attach(college)
plot(Private, Outstate)
Elite = rep("No", nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(Elite)
plot(Elite, Accept)

par(mfrow = c(2,2))
hist(Accept)
hist(Expend)
plot(Elite, perc.alumni)  #Elite schools have more % of alumni who donate.
plot(Elite, PhD)  #Elite schools have more % of PhD falcuties.

#9
summary(Auto)
attach(Auto)
range(displacement)
range(horsepower)
range(weight)
range(acceleration)
range(year)
mean(displacement)
mean(horsepower)
mean(weight)
mean(acceleration)
mean(year)
sd(displacement)
sd(horsepower)
sd(weight)
sd(acceleration)
sd(year)

auto = Auto[-c(10:85),]
attach(auto)
range(displacement)
range(horsepower)
range(weight)
range(acceleration)
range(year)
mean(displacement)
mean(horsepower)
mean(weight)
mean(acceleration)
mean(year)
sd(displacement)
sd(horsepower)
sd(weight)
sd(acceleration)
sd(year)

pairs(Auto)
par(mfrow = c(1,1))
plot(Auto$weight, Auto$mpg) #with weight increasing, mpg decreases
plot(Auto$horsepower, Auto$mpg) #with horsepower increasing, mpg decreases
plot(Auto$displacement, Auto$mpg) #with displacement increasing, mpg also has a trend to decrease


#10
library(MASS)
?Boston 

#Boston data has 506 rows and 14 columns
#rows represent suburbs of Boston
#columns represent predictors of housing values

pairs(~crim+zn+indus+tax+nox+age+rm+medv, data=Boston)
plot(Boston$medv, Boston$crim) 

#with median value of owner-occupied home increasing, crime rate decreases

range(Boston$crim)
range(Boston$tax)
range(Boston$ptratio)

sum(Boston$chas) #35 suburbs bound the Charles river
median(Boston$ptratio)
Boston[Boston$medv == min(Boston$medv),]
summary(Boston) 

#The suberbs of Boston with lowest median value of owner-occupied homes have high tax rate, 
#high proportion of blacks, high index of accessiblity to radial highways and high percentage of 
#lower status of the population and all are built prior to 1940.

sum(Boston$rm > 7)
sum(Boston$rm > 8)
Boston[Boston$rm > 8,] #per capita crime rate is low.
