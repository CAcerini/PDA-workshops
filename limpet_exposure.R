# does the exposure level of a beach have an effect on the
# morphology of animals living there ? - specifically limpet shell morphology
# we will classify all of the beaches to 3 exposure levels
# exposure levels: sheltered, semi exposed or exposed 
# we then pick 10 beaches of each exposure level at random 
# visit each of the beaches and measure width and height of a random sample of limpits
# convert this to shape by dividing width by height
# data = "limpet_exposure.txt" - two columns: 1st = exposure of beach 2nd= average shape at beach

#actual stats time..
#first save the new script

rm(list=ls()) #clears out any previous data or analyses and means we start with clean slate

limpet_data<- read.table(file.choose(),header=TRUE) #will allow you to load file of choice and assign it to a data fram named limpet_analysis

limpet_data #  will display data 
plot(limpet_data) # simplet plot of data, chooses own appropriate plot depending on type of data: in this case as box plot
by(limpet_data$shape, limpet_data$beachtype, mean) #calculates means for exposures $ assigns to specific column. if we wanted standard deviations, change mean to sd, or could do var, min, max, range
limpet_data$beachtype <- as.factor(limpet_data$beachtype) # if not recognising columns, this line converts factor to column 
limpet_data$beachtype

# now well fit our GLM, the model were fitting is: response=predictors+error
# lm function = linear model 
# first part specifies the model, and then we specify the dataframe to use
# we will give the model the name: mymodel
mymodel <- lm(shape~beachtype, data = limpet_data)
#we now need to check our assumptions before interpretting the results
#first we will generate residual plots to check our assumptions
#we do this by making a plot but applying it to the glm (ie. mymodel)
#first and last lines of next 3 lines of code tell R how to format graphical output
#the command here will produce 4 graphs and so we want to see them all at once.

par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(mymodel)
par(mfrow=c(1,1)) # Change back to 1 x 1 - we want to change panel layout back to default after we run our model ! 

#we now want to look at the results to our model so we shall ask for an analysis of variance table, using:
anova(mymodel)

#we need to look at which groups differ from which others...

# tukey test - can be used to make a general type of comparison
# it generates a 95% confidence interval for the difference between each pair of group means 
# can be run with the following code:
AV <- aov(mymodel) # extracts more info from our glm and stores it as an object = AV
TUKEY <- TukeyHSD(AV) # this line carries out the tukey test stores it as object called TUKEY
TUKEY # shows this output
plot(TUKEY) # plots results of tukey test as a graph to interpret

#some other ways to do the tukey test...
# TukeyHSD(aov(mymodel)) - this does the exact same thing as the last three lines of code  
# TukeyHSD(aov(lm(shape~beachtype, data = limpet_exposure))) - this encompases step of fitting glm and savign it as "my model" - can replace all those steps with this code 

