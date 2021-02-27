rm(list=ls())
library(ggplot2)
library(car)
library(Hmisc)

foxdata <- foxes_w4
View(foxdata)


plot(foxdata)

#relationships between
#food and size
#food and area
#area and size


rcorr(as.matrix(foxdata))
#size and food = positive correlation
#area and food = positive correlation
#size and area = positive correlation


#fitting the maximal model to the data 
model1 <- lm(weight ~ gsize + avfood + area, data = foxdata)
anova(model1)

model2 <- lm(weight ~ area + gsize + avfood, data = foxdata)
anova(model2)


#Getting an anova table with type 3 sums of squares. need car package loaded 

options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))
model3 <- lm(weight ~ gsize + avfood + area, data = foxdata)
Anova(model3, type=3)
options(contrasts=c(unordered="contr.treatment", ordered="contr.poly"))

#creating diagnostic residual plots to check assumptions  
par(mfrow=c(2,2)) 
plot(model3)
par(mfrow=c(1,1)) 

#simplified model 
options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))
model4 <- lm(weight ~ gsize + avfood, data = foxdata)
Anova(model4, type=3)
options(contrasts=c(unordered="contr.treatment", ordered="contr.poly"))

#creating diagnostic residual plots to check assumptions  
par(mfrow=c(2,2)) 
plot(model4)
par(mfrow=c(1,1)) 

#fitting model again 
model4 <- lm(weight ~ gsize + avfood, data = foxdata)
summary(model4)

#weight=3.98 -0.6557*size + 4.5082*food 
#food has positive effect 
#size has negative effect 
#r squared value tells you how much of variation in weight is explained by model
#41.81% of variation in weight is explained by our model 



