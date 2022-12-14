Lab1<-read.csv(file="Lab1.csv")

summary(Lab1$EARN)

table(Lab1$Job.class)

table.combine <- ftable(Lab1$EDUC, Lab1$Gender, Lab1$Job.class)
prop.table(table.combine)

hist(Lab1$EARN)

boxplot(Lab1$EARN~Lab1$Job.class)

Lab1$EARN_10000 = Lab1$EARN/10000

plot(Lab1$EARN_10000, Lab1$AGE)