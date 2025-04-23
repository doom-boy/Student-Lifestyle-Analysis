library(ISLR)

lifestyle = read.csv("C:/MYDOWNLOADS/Coding/000 Scripts and Files/Datasets/student_lifestyle_dataset.csv")

head(lifestyle)
par(ps = 14)
plot(lifestyle[,2:7] , cex.lab = 1.5)
cols=c(2,6,7)
plot(lifestyle[,cols] , cex.main = 2, cex.lab = 1)

par(mfrow=c(2,3))
par(mfrow=c(1,1))

# study hrs per day
m1.study = lm(GPA ~ Study_Hours_Per_Day, data = lifestyle)
plot(lifestyle$Study_Hours_Per_Day, lifestyle$GPA, xlab = "Study Hours per Day", ylab = "GPA", main = "Study Hours vs GPA")
abline(m1.study, col='blue',lwd = 3)
summary(m1.study)
coef(m1.study)

# physical activity
m1.physical = lm(GPA ~ Physical_Activity_Hours_Per_Day, data = lifestyle)
plot(lifestyle$Physical_Activity_Hours_Per_Day, lifestyle$GPA, xlab = "Physical Activity Hours per Day", ylab = "GPA", main = "Physical Activity Hours vs GPA")
abline(m1.physical, col='orange', lwd = 3)
summary(m1.physical)

#extracurricular
m1.extra = lm(GPA ~ Extracurricular_Hours_Per_Day, data = lifestyle)
plot(lifestyle$Extracurricular_Hours_Per_Day, lifestyle$GPA, xlab = "Extracurricular Hours per Day", ylab = "GPA", main = "Extracurricular Hours vs GPA")
abline(m1.extra, col='orange', lwd = 3)
summary(m1.extra)

# sleep
m1.sleep = lm(GPA ~ Sleep_Hours_Per_Day, data = lifestyle)
plot(lifestyle$Sleep_Hours_Per_Day, lifestyle$GPA, xlab = "Sleep Hours per Day", ylab = "GPA", main = "Sleep Hours vs GPA")
abline(m1.sleep, col='orange', lwd = 3)
summary(m1.sleep)

#social hr per day
m1.social = lm(GPA ~ Social_Hours_Per_Day, data = lifestyle)
plot(lifestyle$Social_Hours_Per_Day, lifestyle$GPA, xlab = "Social Hours per Day", ylab = "GPA", main = "Social Hours vs GPA")
abline(m1.social, col='orange', lwd = 3)
summary(m1.social)

AIC(m1.study, m1.physical, m1.extra, m1.sleep, m1.social)


par(mfrow=c(5,4))
par(mfrow=c(1,1))
# for residual vs fitted plot
plot(m1.study, lwd = 4)
plot(m1.physical, lwd = 4)
plot(m1.extra, lwd = 4)
plot(m1.sleep, lwd = 4)
plot(m1.social, lwd = 4)

# using Quadratic relationship 
m2.study.sq = lm(GPA ~ Study_Hours_Per_Day + I(Study_Hours_Per_Day^2), data = lifestyle)
summary(m2.study.sq)
plot(m2.study.sq)

#physical
m2.physical.sq = lm(GPA ~ Physical_Activity_Hours_Per_Day + I(Physical_Activity_Hours_Per_Day^2), data = lifestyle)
summary(m2.physical.sq)

#extra 
m2.extra.sq = lm(GPA ~ Extracurricular_Hours_Per_Day + I(Extracurricular_Hours_Per_Day^2), data = lifestyle)
summary(m2.extra.sq)

#sleep
m2.sleep.sq = lm(GPA ~ Sleep_Hours_Per_Day + I(Sleep_Hours_Per_Day^2), data = lifestyle)
summary(m2.sleep.sq)

#social
m2.social.sq = lm(GPA ~ Social_Hours_Per_Day + I(Social_Hours_Per_Day^2), data = lifestyle)
summary(m2.social.sq)

AIC(m2.study.sq, m2.physical.sq, m2.extra.sq, m2.sleep.sq, m2.social.sq) 
AIC(m1.study, m1.physical, m1.extra, m1.sleep, m1.social, m2.study.sq, m2.physical.sq, m2.extra.sq, m2.sleep.sq, m2.social.sq)


# plotting the Quadratic model
x = lifestyle$Study_Hours_Per_Day
xmesh = seq(0.5*min(x), 2*max(x), by = 0.1)
yhat = predict(m2.study.sq, newdata = data.frame(Study_Hours_Per_Day = xmesh))

plot(lifestyle$Study_Hours_Per_Day, lifestyle$GPA, xlab = "Study Hours per Day", ylab = "GPA", main = "Study Hours vs GPA")
abline(m1.study, lwd = 3)
lines(xmesh, yhat, col = 'red', lwd = 2)
legend("topleft", c("Linear", "Quadratic"), lty = c(1,1), lwd = c(2,2), col = c("black", "red"))


# using the inverted model
#study
m2.study.inv = lm(GPA ~ Study_Hours_Per_Day + I(1/Study_Hours_Per_Day), data = lifestyle)
summary(m2.study.inv)

#phy
m2.physical.inv = lm(GPA ~ Physical_Activity_Hours_Per_Day + I(1/Physical_Activity_Hours_Per_Day), data = lifestyle)
summary(m2.physical.inv)

#extra
m2.extra.inv = lm(GPA ~ Extracurricular_Hours_Per_Day + I(1/Extracurricular_Hours_Per_Day), data = lifestyle)
summary(m2.extra.inv)

#sleep
m2.sleep.inv = lm(GPA ~ Sleep_Hours_Per_Day + I(1/Sleep_Hours_Per_Day), data = lifestyle)
summary(m2.sleep.inv)

#social
m2.social.inv = lm(GPA ~ Social_Hours_Per_Day + I(1/Social_Hours_Per_Day), data = lifestyle)
summary(m2.social.inv)

AIC(m2.study.inv, m2.physical.inv, m2.extra.inv, m2.sleep.inv, m2.social.inv)

AIC(m1.study, m1.physical, m1.extra, m1.sleep, m1.social, m2.study.sq, m2.study.inv)



# Model 2
# using multi linear regression
m2.study.physical = lm(GPA ~ Study_Hours_Per_Day + Physical_Activity_Hours_Per_Day , data = lifestyle)
summary(m2.study.physical)
coef(m2.study.physical)

m2.study.physical.sq = lm(GPA ~ Study_Hours_Per_Day + I(Study_Hours_Per_Day^2) + Physical_Activity_Hours_Per_Day + I(Physical_Activity_Hours_Per_Day^2), data = lifestyle)
summary(m2.study.physical.sq)

AIC(m2.study.physical, m2.study.physical.sq)

AIC(m1.study, m2.study.sq, m2.study.physical, m2.study.physical.sq)

# overfitting, look for significance
overfit = lm(GPA ~ ., data = lifestyle)
summary(overfit)

# interaction terms 
m2.interaction.phy = lm(GPA ~ Study_Hours_Per_Day*Physical_Activity_Hours_Per_Day, data = lifestyle)
summary(m2.interaction.phy)

m2.interaction.extra = lm(GPA ~ Study_Hours_Per_Day*Extracurricular_Hours_Per_Day, data = lifestyle)
summary(m2.interaction.extra)

m2.interaction.sleep = lm(GPA ~ Study_Hours_Per_Day*Sleep_Hours_Per_Day, data = lifestyle)
summary(m2.interaction.sleep)

m2.interaction.social = lm(GPA ~ Study_Hours_Per_Day*Social_Hours_Per_Day, data = lifestyle)
summary(m2.interaction.social)

AIC(m2.interaction.phy, m2.interaction.extra, m2.interaction.sleep, m2.interaction.social)

AIC(m1.study, m2.study.sq, m2.study.physical, m2.study.physical.sq, m2.interaction, m2.interaction2)


x = lifestyle$Study_Hours_Per_Day
xmesh = seq(0.5*min(x), 2*max(x), by = 0.1)
#yhat = predict(m2.study.sq, newdata = data.frame(Study_Hours_Per_Day = xmesh))

yhat2 = predict(m2.study.physical, newdata = data.frame(Study_Hours_Per_Day = xmesh))


plot(lifestyle$Study_Hours_Per_Day, lifestyle$GPA, xlab = "Study Hours per Day", ylab = "GPA", main = "Study Hours vs GPA")
abline(m1.study, col ='red', lwd = 4)
lines(xmesh, yhat2, col = 'blue', lwd = 2)
legend("topleft", c("Linear", "Multi-Linear"), lty = c(1,1), lwd = c(2,2), col = c("red", "blue"))

#lines(xmesh, yhat, col = 'red', lwd = 2)
legend("topleft", c("Linear", "Quadratic", "Multi-Linear"), lty = c(1,1,1), lwd = c(2,2,2), col = c("green", "red", "blue"))



# getting the RSS
yhat_m1 = predict(m1.study)
rss_m1.study = sum((lifestyle$GPA - yhat_m1)^2); rss_m1.study

yhat_m2 = predict(m2.study.physical)
rss_m2.study.physical = sum((lifestyle$GPA - yhat_m2)^2); rss_m2.study.physical







# https://www.kaggle.com/datasets/rabieelkharoua/students-performance-dataset
performance = read.csv("C:/MYDOWNLOADS/Coding/000 Scripts and Files/Datasets/student_performance_data.csv")
cols = c(2,6,7,14)     
plot(performance[,cols])

head(performance)

m1 = lm(GPA ~ Absences, data = performance)
m1 = lm(performance$GPA ~ performance$Absences)
summary(m1)
