#Q1 a)
#Checking the correlation between the variables
cor(pressure$temperature, pressure$pressure)
#Q1 b)
#Creating a scatter plot of the data that shows both the points and a smoothed line of the points
scatter.smooth(x=pressure$temperature, y=pressure$pressure, main="Pressure ~ Temperature")  # scatterplot
#Q1 c)
#Creating side-by-side box-plots of each variable
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(pressure$temperature, main="Temperature") # box plot for 'temperature'
boxplot.stats(pressure$temperature)$out # display outliers
boxplot(pressure$pressure, main="Pressure")  # box plot for 'pressure'
boxplot.stats(pressure$pressure)$out # display outliers
#Q1 d)
#Creating side-by-side graphs of the densities of the variables
plot(density(pressure$temperature), main="Density Plot: Temperature")  # density plot for 'temperature'
plot(density(pressure$pressure), main="Density Plot: Pressure")  # density plot for 'pressure'
#Q2 a)
#Fit a simple linear model that predicts pressure from temperature
pressure.lm <- lm(temperature ~ pressure, data=pressure)  # build linear regression model on full data
summary(pressure.lm)  #inspect the results
#Q2 b)
#Visualize the resulting regression line on a scatterplot of the data
par(mfrow=c(1, 1))  # back to 1 plot
plot(pressure$temperature, pressure$pressure)
abline(pressure.lm)
#Q2 c)
#Plot the residuals density
pressure.res <- resid(pressure.lm)
print(mean(pressure.res))
plot(density(pressure.res), main="Density Plot: residuals") 
#Q2 d)
#Use the plot function to generate the 4 graphs
par(mfrow=c(2,2)) # 2x2 grid of plots
plot(pressure.lm)