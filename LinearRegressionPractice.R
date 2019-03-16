head(cars)
scatter.smooth(x=cars$speed, y=cars$dist, main="Distance ~ Speed") # scatterplot
par(mfrow=c(1, 2)) # Divide graph area in 2 columns
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out))
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))

library(e1071)
par(mfrow=c(1,2)) # Divide graph area in 2 columns
plot(density(cars$speed), main="Density Plot: Speed", ylab = "Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2))) # Density plot for speed
polygon(density(cars$speed), col="red")
plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2))) # Density plot for 'dist'
polygon(density(cars$dist), col="red")
cor(cars$speed, cars$dist)

linearMod <- lm(Distance ~ Speed, data = cars) # Build Linear regression model on full data
print(linearMod)

# The Coefficients that the statement above prints out have two components: Intercept: -17.579, Speed: 3.932.
# These are called the beta coefficients. In other words:
# dist=Intercept+(B*speed) = dist=-17.579 + 3.932*speed
# Before I can use my linear model I have to make sure my model 
# is statistically signifigant.

summary(linearMod)
