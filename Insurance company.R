
## Insurance prediction with regression modelling


## Description:

# Develop a regression model that the insurance company can use to predict a new customers future medical costs.
# I try different models by selecting different subsets of the variables and 
# choose the one that I find as the best model based on model fit and model simplicity. 
# Based on the selected model, I predict the cost for a person who is 25 years old, female, bmi 32.5, 2 children and non-smoker.
# If the insurance company plans to accept customers with estimated cost less than 5000,
# would they accept this person according to the prediction?

library("tidyverse")
library("ggplot2")
library("rpart")
library("car")


# Importing the data
insurance <- read_csv2("insurance.csv")
view(insurance)
str(insurance)

# checking the Null values
sum(is.na(insurance))


# First we start with some basic visualization 
# in order to find which variables (age, gender, bmi, children, smoker)
# are the most significant variables affecting  cost

ggplot(insurance, aes(x = age, y = cost)) + 
  geom_point()

ggplot(insurance, aes(x = gender, y = cost)) + 
  geom_point()

ggplot(insurance, aes(x = bmi, y = cost)) + 
  geom_point()

ggplot(insurance, aes(x = children, y = cost)) + 
  geom_point()

ggplot(insurance, aes(x = smoker, y = cost)) + 
  geom_point()

# Based on the plots only gender variable seems to be relatively non-significant.

# Now we start creating a regression model.

# We need to recode smoker and gender into a binary variable to use it in a regression model
insurance$smoker <- Recode(insurance$smoker, "'yes'=1; 'no'=0", as.factor = F)
insurance$gender <- Recode(insurance$gender, "'male'=1; 'female'=0", as.factor = F)

# We can first create the correlation table to check
# the correlationship among dependent and independent variables

cor(insurance)

# smoker and cost have the higher correlationship (0.787251430).
# gender and cost have the lowest correlationship (0.05729207).



# regression models
insurance_fit1 <- lm(cost~age+gender+bmi+children+
                       smoker, data = insurance)

summary(insurance_fit1)
# except  gender, other variables are significant (***), R^2=0.7497
# so we can exclude variable gender to improve the model.

insurance_fit2 <- lm(cost~age+bmi+children+
                       smoker, data = insurance)

summary(insurance_fit2)
# the model is improved
# now all variables are significant(***), but R^2 did not change (0.7497)



# Moreover, can also see the R^2 for all the possible predictor combinations
install.packages("leaps")
library(leaps)

leaps<-regsubsets(cost~age+gender+bmi+children+
                    smoker,data=insurance,nbest=10)


# We can plot a table of models showing variables in each model(R-squared measure)

plot(leaps,scale="r2")
# According to plot the maximum R^2 is about 0.75 which can be find for two combinations
# (age + gender +bmi + children + smoker) and with excluding gender (age + bmi +children + smoker)


## Cost prediction for a person who is 25 years old, female, bmi 32.5, 2 children and non-smoker
## we check it with both two model(with and without variable gender)

newdata <- data.frame(age=25,gender=0,bmi=32.5,children=2,smoker=0)
newdata1 <- data.frame(age=25,bmi=32.5,children=2,smoker=0)


predict(insurance_fit1, newdata)
## cost predition = 5816.572
predict(insurance_fit2, newdata1)
## cost predition = 5750.644



## If the insurance company plans to accept customers with estimated cost
## less than 5000, would they accept this person according to your prediction?

# No,according to the prediction, the cost is higher than 5000.
# they would not accept this person.

