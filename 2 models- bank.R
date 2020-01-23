
## Description: Comparing classification trees and neural networks

# Build a churn prediction model, i.e. identifying the customers who are the most likely 
# to leave the bank. A  dataset (churn model.csv) of 10000 customers is given in order to build a model.
# Build both classification tree and neural network models to create churn prediction models.
# Evaluate the models based on sensitivity and specificity measures! 
# Would you use classification trees or neural networks based on your findings?

library('devtools')
library("tidyverse")
library("car")
library("rpart")
library("caTools")
library("Metrics")
library("ggplot2")
library("gridExtra")
library("rpart.plot")
library('nnet')
library('neuralnet')
library('NeuralNetTools')

churn <- read.csv2("churn_model.csv")

view(churn)
str(churn)

# checking Null values
sum(is.na(churn))


# First some basic visualization for descriptive analytics
# to check most influencial independent variables 

ggplot(churn, aes(CreditScore, Exited)) + geom_jitter()
ggplot(churn, aes(Gender, Exited)) + geom_jitter()
ggplot(churn, aes(Age, Exited)) + geom_jitter()
ggplot(churn, aes(Tenure, Exited)) + geom_jitter()
ggplot(churn, aes(NumOfProducts, Exited)) + geom_jitter()
ggplot(churn, aes(CreditScore, Exited)) + geom_jitter()
ggplot(churn, aes(Balance, Exited)) + geom_jitter()
ggplot(churn, aes(HasCrCard, Exited)) + geom_jitter()
ggplot(churn, aes(EstimatedSalary, Exited)) + geom_jitter()

# According to the graphs the most influential variables are age and NumOfProducts.


# 1.Classification tree model


# Setting the seed 
set.seed(1985)

# Then we split the data set in a way that 
# 70% will be in the training set, 30% in test set.

split <- sample.split(churn$Exited, SplitRatio = 0.70)

split

training_churn <- subset(churn, split == TRUE)
test_churn <- subset(churn, split == FALSE)

summary(training_churn)
summary(test_churn)

# Now we can build a tree using the predictors and the Existed variable 
# as the output class

tree_churn <- rpart(Exited ~ . , data = training_churn, method="class")

# Now we can visualize the tree to interpret it.
prp(tree_churn)


# Different visualization 

rpart.plot(x = tree_churn, yesno = 2, type = 0, extra = 0)


# First we can look at the performance on the training set

Predict_churn <- predict(tree_churn, newdata = training_churn, type = "class")

tab <- table(training_churn$Exited, Predict_churn)

tab
#    0    1
# 0 5271  303
# 1  779  647

s <- eval_class(tab[1,1], tab[1,2], tab[2,1], tab[2,2])

s
# Accuracy Sensitivity Specificity   Precision         NPP 
# 0.8454286   0.6810526   0.8712397   0.4537167   0.9456405 

# Then can check the performance on the test set

Predict_churn <- predict(tree_churn, newdata = test_churn, type = "class")

tab <- table(test_churn$Exited, Predict_churn)

tab
#    0    1
# 0 2229  160
# 1  342  269

s <- eval_class(tab[1,1], tab[1,2], tab[2,1], tab[2,2])

s
# Accuracy Sensitivity Specificity   Precision         NPP 
# 0.8326667   0.6270396   0.8669778   0.4402619   0.9330264 

# The results in training and test sets are so close.



## 2.neural network models

# First we need to scale data. before that we recode some variables
# into number

churn <- churn %>% mutate(Gender = Recode(Gender,"'Male'=1; 'Female'=0", as.factor = F))


str(churn)

# scaling data

churn[,1:(ncol(churn) - 1)] <- scale(churn[,1:(ncol(churn) - 1)])


# then we create traning and test sets.
set.seed(1985)

split <- sample.split(churn$Exited, SplitRatio = 0.70)

split

training_churn <- subset(churn, split == TRUE)
test_churn <- subset(churn, split == FALSE)

summary(training_churn)
summary(test_churn)

#creating a single hidden layer neural network
nnet_churn <- nnet(training_churn[,1:(ncol(training_churn) - 1)], 
                   training_churn[,c('Exited')], size = 3, rang = 0.1, maxit = 200) 


# visualizing neural network
plotnet(nnet_churn, pos_col = 'green', neg_col = 'red')

# Identifying individual importance of attributes

install.packages('devtools')
library('devtools')

# Importing it from github

source_url('https://gist.githubusercontent.com/fawda123/6206737/raw/d6f365c283a8cae23fb20892dc223bc5764d50c7/gar_fun.r')

# Specifying the names of the attributes and the model

gar.fun(colnames(training_churn)[-ncol(training_churn)], nnet_churn)






eval_class <- function(tn, fn, fp, tp){
  accuracy <- (tp + tn) / (tp + tn + fn + fp)
  sensitivity <- tp / (tp + fn)
  specificity <- tn / (fp + tn)
  precision <- tp / (tp + fp)
  npp <- tn / (tn + fn)
  res <- c(accuracy, sensitivity, specificity, precision, npp)
  names(res) <- c("Accuracy", "Sensitivity", "Specificity", "Precision", "NPP")
  res
}
# Then we can obtain prediction for the test set

predict_test <- predict(nnet_churn, test_churn[,1:(ncol(test_churn)-1)])

tab <- table(round(predict_test, digits = 0), test_churn[,c('Exited')])

tab
#    0    1
# 0 2389  611

# The second row (fp & tp) in confusion matrix does not exits. 
# So we specify them as zero.

s <- eval_class(tab[1,1], tab[1,2], 0 , 0)

s
# Accuracy Sensitivity Specificity   Precision         NPP 
# 0.7963333   0.0000000   1.0000000         NaN   0.7963333 



# Neural model with two layers

# First we create the data partition again

set.seed(1985)

split <- sample.split(churn$Exited, SplitRatio = 0.70)

training_churn <- subset(churn, split == TRUE)
test_churn <- subset(churn, split == FALSE)

summary(training_churn)
summary(test_churn)

# Then create the model, with two hidden layers, the first has 8, the second 4 nodes

# To create the form of the model, we can use the reformulate function

f <- reformulate(setdiff(colnames(training_churn), "Exited"), response="Exited")

f

# Then finally the model
neural_churn <- neuralnet(f, training_churn, hidden=c(8,4))

plot(neural_churn)


predict_test <- compute(neural_churn, test_churn[,1:(ncol(test_churn)-1)])

tab <- table(test_churn$Exited, predict_test$net.result > 0.2)

s <- eval_class(tab[1,1], tab[1,2], tab[2,1], tab[2,2])

s

# Accuracy Sensitivity Specificity   Precision         NPP 
# 0.7643333   0.4503106   0.9134710   0.7119476   0.7777313 




## Finally,
## Evaluate the models based on sensitivity and specificity measures! Would
## you use classification trees or neural networks based on your findings?


# decision tree (traning set)
#  Sensitivity Specificity   
#  0.6810526   0.8712397    


# decision tree (test set)
# Sensitivity Specificity  
#  0.6270396   0.8669778 



# neural model (one layer)
# Sensitivity Specificity 
# 0.0000000   1.0000000 


# # neural model (2 layers)
# Accuracy Sensitivity Specificity   Precision         NPP 
# 0.7643333   0.4503106   0.9134710   0.7119476   0.7777313 



## The choise between these two methods depends on the main purpose of modeling and thus, whether
## being more interested in sensitivity or specificity.
## If the idea of false positives far better than false negatives,
## we choose sensitivity.
## and if we want to cover all true negatives, we would choose specificity.

## In this case, neural model is a good choise if we are interested in specification (Specificity=1)
## on the other side, we we want to investigate both sensitivity and specificity
## we can choose decision tree model which has relatively good values in sensitivity (0.6270396)
## and specificity(0.8669778).

## Furthermore, although neural model acts as a black box and cannot be explained
## easily like decision trees, however
## neural network has the ability to learn hidden relationships in the data 
## here specification of the neural network is quite high (1 in one layer & 0.91 in two layers )
## (A highly specific test means that there are few false positive results)
## Thus, neural network can be a good choise.