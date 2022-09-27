#TASK1:
#create a vector x0 of length 5000, drawn from a normal distribution with mean 5, sd 4
x0 <- rnorm(5000,5,4)

# record the size, mean and sd of x0
N0 <- length(x0)
mean0 <- mean(x0)
sd0 <- sd(x0)

#Take a random sample of size 40 from x0
x1 <- sample(x0,40)

# record the size, mean and sd of x1
n1 <- length(x1)
mean1 <- mean(x1)
sd1 <- sd(x1)

#calculate the standard error of mean1 using known and estimated sd0
se_kn1 <- sd0/sqrt(n1)
se_unkn1 <- sd1/sqrt(n1)

#Take a random sample of size 100 from x0
x2 <- sample(x0,100)

# record the size, mean and sd of x2
n2 <- length(x2)
mean2 <- mean(x2)
sd2 <- sd(x2)

#calculate the standard error of mean2 using known and estimated sd0
se_kn2 <- sd0/sqrt(n2)
se_unkn2 <- sd2/sqrt(n2)

#create a vector x3 of length 5000, drawn from an Exponential(1.2) distribution (where 1.2=lambda)
x3 <- rexp(5000, 1.2)

# record the size, mean and sd of x3
N3 <- length(x3)
mean3 <- mean(x3)
sd3 <- sd(x3)

#Take a random sample of size 40 from x3
x4 <- sample(x3,40)

# record the size, mean and sd of x4
n4 <- length(x4)
mean4 <- mean(x4)
sd4 <- sd(x4)

#calculate the standard error of mean4 using known and estimated sd3
se_kn4 <- sd3/sqrt(n4)
se_unkn4 <- sd4/sqrt(n4)

#Take a random sample of size 100 from x3
x5 <- sample(x3,100)

# record the size, mean and sd of x5
n5 <- length(x5)
mean5 <- mean(x5)
sd5 <- sd(x5)

#calculate the standard error of mean5 using known and estimated sd3
se_kn5 <- sd3/sqrt(n5)
se_unkn5 <- sd5/sqrt(n5)

#-----------------------------------------------------------
#TASK2:
#Calculate Z-score for 95% CI
Z_score <- qnorm(0.975)

#Calculate critical t-value for 95% CI and x1
t_score <- qt(0.975, n1-1)

#Calculate 95% CIs for x1 using the Z-score and the known population SD
left_Z95_kn1 <- mean1-Z_score*se_kn1
right_z95_kn1 <- mean1+Z_score*se_kn1

#Calculate 95% CIs for x1 using t and the sample SD
left_t95_unkn1 <- mean1-t_score*se_unkn1
right_t95_unkn1 <- mean1+t_score*se_unkn1

#show results
paste("var: x0", "mean:", mean0, "sd:", sd0)
paste("var: x1", "mean:", mean1, "sd:", sd1, "se w/known pop sd:", se_kn1, "se w/unknown pop sd:", se_unkn1)
paste("95% CIs for estimate of population mean")
paste("Z-distribution w/known pop sd:", left_Z95_kn1, right_z95_kn1)
paste("t-distribution w/unknown pop sd:", left_t95_unkn1, right_t95_unkn1)

#Calculate critical t-value for 95% CI and x4
t_score <- qt(0.975, n4-1)

#Calculate 95% CIs for x4 using the Z-score and the known population SD
left_Z95_kn4 <- mean4-Z_score*se_kn4
right_z95_kn4 <- mean4+Z_score*se_kn4

#Calculate 95% CIs for x4 using t and the sample SD
left_t95_unkn4 <- mean4-t_score*se_unkn4
right_t95_unkn4 <- mean4+t_score*se_unkn4

#show results
paste("var: x0", "mean:", mean0, "sd:", sd0)
paste("var: x4", "mean:", mean4, "sd:", sd4, "se w/known pop sd:", se_kn4, "se w/unknown pop sd:", se_unkn4)
paste("95% CIs for estimate of population mean")
paste("Z-distribution w/known pop sd:", left_Z95_kn4, right_z95_kn4)
paste("t-distribution w/unknown pop sd:", left_t95_unkn4, right_t95_unkn4)

#-----------------------------------------------------------
#TASK3(a):
#make a qqplot and a histogram with normal density curve for x0
qqnorm(x0)
qqline(x0)
hist(x0, freq = FALSE)
xfit <- seq(min(x0), max(x0), length = 5000) 
yfit <- dnorm(xfit, mean = mean(x0), sd = sd(x0))
lines(xfit, yfit)

#make a qqplot and a histogram with normal density curve for x1
qqnorm(x1)
qqline(x1)
hist(x1, freq = FALSE)
xfit <- seq(min(x1), max(x1), length = 40) 
yfit <- dnorm(xfit, mean = mean(x1), sd = sd(x1))
lines(xfit, yfit)

#make a qqplot and a histogram with normal density curve for x3
qqnorm(x3)
qqline(x3)
hist(x3, freq = FALSE)
xfit <- seq(min(x3), max(x3), length = 5000) 
yfit <- dnorm(xfit, mean = mean(x3), sd = sd(x3))
lines(xfit, yfit)

#make a qqplot and a histogram with normal density curve for x4
qqnorm(x4)
qqline(x4)
hist(x4, freq = FALSE)
xfit <- seq(min(x4), max(x4), length = 40) 
yfit <- dnorm(xfit, mean = mean(x4), sd = sd(x4))
lines(xfit, yfit)