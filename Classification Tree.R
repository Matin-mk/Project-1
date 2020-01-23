
## Description: Classification Tree

# The main task will be to identify a model that can predict whether a client of the bank
# will positively respond to a marketing campaign. The data used as the basis of building 
# a model is from a direct marketing campaign of a banking institution. 
# The marketing campaign was based on phone calls to access if a specific product (bank term deposit)
# would be (’yes’) or not (’no’) subscribed.  
# a classification model will be created based on classification trees 
# The models will be evaluated based on various performance measures 


# Importing data
Bank <- read.csv2("bank-full.csv")
dim(Bank)
str(Bank)
view(Bank)
summary(Bank)

# checking the Null values
sum(is.na(Bank))


# First we visualize the relationship between each variable versus y. 
# In order to find out that which variables are significant among all other variables.


ggplot(Bank, aes(education, y)) + geom_jitter()
ggplot(Bank, aes(age, y)) + geom_jitter()
ggplot(Bank, aes(job, y)) + geom_jitter()
ggplot(Bank, aes(marital, y)) + geom_jitter()
ggplot(Bank, aes(housing, y)) + geom_jitter()
ggplot(Bank, aes(loan, y)) + geom_jitter()
ggplot(Bank, aes(contact, y)) + geom_jitter()
ggplot(Bank, aes(balance, y)) + geom_jitter()
ggplot(Bank, aes(poutcome, y)) + geom_jitter()
ggplot(Bank, aes(contact, y)) + geom_jitter()
ggplot(Bank, aes(duration, y)) + geom_jitter()
ggplot(Bank, aes(default, y)) + geom_jitter()
ggplot(Bank, aes(previous, y)) + geom_jitter()
ggplot(Bank, aes(month, y)) + geom_jitter()
ggplot(Bank, aes(campaign, y)) + geom_jitter()
ggplot(Bank, aes(pdays, y)) + geom_jitter()

#Surely there should be a significant relationship between duration and the y.
# So we can check the count of "yes" and "no" regarding duration, which is significant.

Bank_Yes <-filter(Bank, y == 'yes')
Bank_No <-filter(Bank, y == 'no')


yesTermDepositsByDuration <- ggplot(Bank_Yes, aes(duration)) + geom_histogram(binwidth = 10) +
  labs(title = "Term Deposits Yes by Duration", x="Duration", y="Count of Yes") + xlim(0,3000)

noTermDepositsByDuration <- ggplot(Bank_No, aes(duration)) + geom_histogram(binwidth = 10) + 
  labs(title = "Term Deposits No by Duration", x="duration", y="Count of No")+ xlim(0,3000)

grid.arrange(yesTermDepositsByDuration, noTermDepositsByDuration)


# moreover,according to plots the most significant variables are duration, poutcome and month.



# In the follwoing lines, we convert variable y, month, poutcome
# into a factor variable

Bank <- Bank %>% mutate(y = factor(y), month = factor(month), poutcome = factor(poutcome))

str(Bank)

# creating classification model

# First we set the seed.
set.seed(1985)

# Then we split the dataset into training and testing data.
# in a way that 75% will be in the training set.

split <- sample.split(Bank$y, SplitRatio = 0.75)

split

training_bank <- subset(Bank, split == TRUE)
test_bank <- subset(Bank, split == FALSE)

summary(training_bank)
summary(test_bank)

# Now we can build a tree using the predictors and the y variable as the 
# output class

tree_Bank <- rpart(y ~ . , data = training_bank, method="class")

# Now we can visualize the tree to interpret it.
prp(tree_Bank)

# Different visualization 

rpart.plot(x = tree_Bank, yesno = 2, type = 0, extra = 0)

# based on the tree the most significant variables are duration, poutcome, and month


# The function for evaluation

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

# First we can look at the performance on the training set

Predict_Bank <- predict(tree_Bank, newdata = training_bank, type = "class")

tab <- table(training_bank$y, Predict_Bank)

tab
#       no   yes
# no  28882  1060
# yes  2217  1750

s <- eval_class(tab[1,1], tab[1,2], tab[2,1], tab[2,2])

s
# Accuracy Sensitivity Specificity   Precision         NPP 
# 0.9033590   0.6227758   0.9287115   0.4411394   0.9645982 

# Then we can check the performance on the test set

Predict_Bank <- predict(tree_Bank, newdata = test_bank, type = "class")

tab <- table(test_bank$y, Predict_Bank)

tab
#      no  yes
# no  9617  363
# yes  757  565

s <- eval_class(tab[1,1], tab[1,2], tab[2,1], tab[2,2])

s
#  Accuracy Sensitivity Specificity   Precision         NPP 
#  0.9009025   0.6088362   0.9270291   0.4273828   0.9636273 

## Based on performances on training set and test set,although the accuracy is high,
## the number of FP (757) is relatively high (compared to TN=565).
## this means there are too many risky investments classified as safe.
## Thus, we employ penalty matrix to alleviate this problem and 
## to give priority to identify the risky investments correctly.
## We choose more weight for the element that is more important in confusion matrix.



penalty <- matrix(c(0,1,100,0), nrow = 2, byrow = TRUE)

tree_bank1 <- rpart(y ~ . , data = training_bank, method="class", cp=0.00005, minbucket=60, parms = list(loss=penalty))

Predict_Bank1 <- predict(tree_bank1, newdata = test_bank, type = "class")

tab <- table(test_bank$y, Predict_Bank1)

tab
#      no  yes
# no  6091 3889
# yes   54 1268

# The element FP is improved (but accuracy is decreased)

s <- eval_class(tab[1,1], tab[1,2], tab[2,1], tab[2,2])

s
# Accuracy Sensitivity Specificity   Precision         NPP 
# 0.6511237   0.2458794   0.9912124   0.9591528   0.6103206 




