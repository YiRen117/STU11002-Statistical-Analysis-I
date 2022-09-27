#Part A:
#1)
#simulate a vector of size 100 drawn from a Normal(4,5) distribution
v1 <- rnorm(100,4,5)
#Simulate another vector of size 80 drawn from a Normal(3.5,2) distribution
v2 <- rnorm(64,3.5,2)

#record the size, mean and sd of vector 1
n1 <- length(v1)
xbar1 <- mean(v1)
sd1 <- sd(v1)
# record the size, mean and sd of vector 2
n2 <- length(v2)
xbar2 <- mean(v2)
sd2 <- sd(v2)

#define mu (the mean of the population under the null hypothesis
mu <- 0
#calculate the standard error
s <- sqrt(((n1-1)*(sd1^2)+(n2-1)*(sd2^2))/(n1+n2-2))
se_A <- s*sqrt(1/n1+1/n2)
#calculate the test statistic
test_stat_A <- (xbar1-xbar2-mu)/se_A

#define df
df <- n1+n2-2
#two-tailed
p_val_H0 <- 2*pt(abs(test_stat_A), df, lower.tail=FALSE)
p_val_HA <- 1-p_val_H0

#Part B:
#1)
#calculate the pooled population proportion
Prop_0 <- ((Prop_1*n_1)+(Prop_2*n_2))/(n_1+n_2)
#calculate se
se_B <- sqrt((Prop_0)*(1-Prop_0)*(1/n_1+1/n_2))
#calculate test statistic
test_stat_B <- (abs(Prop_1-Prop_2))/SE

#2)
#recalculate se
se_B <- sqrt((Prop_1*(1-Prop_1))/n_1+(Prop_2*(1-Prop_2))/n_2)
#calculate 95% CI for estimate of the difference between the two population proportions
left_t95_unkn4 <- abs(Prop_1-Prop_2)-1.96*se_B
right_t95_unkn4 <- abs(Prop_1-Prop_2)+1.96*se_B

#Part C:
#1)
#load file
survey <- read.csv(file="survey.csv", header=TRUE)
#form the table
tbl <- table(survey$Smoke, survey$Exer)
print(tbl)
#run the chi square test
chisq.test(tbl)