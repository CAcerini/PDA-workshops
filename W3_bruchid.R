rm(list=ls())
library(ggplot2)

beetledata <- bruchid_w3
view(beetledata)

#do a scatter plot
ggplot(beetledata, aes(x = size, y = fec)) + geom_point()

ggplot(beetledata, aes(x = size, y = fec, colour = competition)) + geom_point(size = 3)


#fitting model  
mymodel<-lm(fec ~ size+ competition + size*competition, data = beetledata)

#creating diagnostic residual plots to check assumptions  
par(mfrow=c(2,2)) 
plot(mymodel)
par(mfrow=c(1,1)) 

#additional assumption being made is that there is decent overlap of x values between groups
#this is seen in the scatter plot- there is overlap 

#interaction is not significant - so lines are parallel 
anova(mymodel)

summary(mymodel)
#we need to be confident that the relationship between our covariate and 
#our response variable is the same for the different level of the factor. 
#no significant interaction therefore this assumption is met  


#we want to fit a simpler model with no interaction.
#In doing so we are assuming that any relationship between fecundity 
#and size is the same for the two levels of our factor. 
#Put another way, we are assuming that any effect of treatment on fecundity 
#is the same for any size of beetle. 

mymodel2<-lm(fec ~ size+ competition, data = beetledata)

par(mfrow=c(2,2)) 
plot(mymodel2)
par(mfrow=c(1,1)) 
#assumptions are met 

anova(mymodel2)
summary(mymodel2)
#can reject null hypothesis

#adding regression line to the data 
ggplot(beetledata, aes(x = size, y = fec, colour = competition)) + 
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE)

