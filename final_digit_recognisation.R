########SVM Handwritten Digit  Recogniser #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Linear kernel
#  4.2 RBF Kernel
# 5 Hyperparameter tuning and cross validation
############################################################
install.packages("caret")

install.packages("kernlab")

install.packages("dplyr")

install.packages("readr")

install.packages("ggplot2")

install.packages("gridExtra")


# 1. Business Understanding: 

#The objective is to identify each of a digit that has been scanned by scanner
#rectangular pixel displays digit (0-9)  written in a image

#####################################################################################

# 2. Data Understanding: 
# 
# Number of Instances: 60000
# Number of Attributes: 785


#####################################################3
#3. Data Preparation: 


#Loading Neccessary libraries

library(kernlab)
library(readr)
library(caret)
library(ggplot2)

###################################################
#Loading data set

Data <- read.csv("mnist_train.csv",header = FALSE)
### Used header = FALSE as there is no attribute named in the provided dataset 
#View(Data)

##Viewed the data set with generated attribute like V1,V2
View(Data)
############################################

str(Data)
### I have converted the output attribute as Digit to make it more significant
colnames(Data)[which(names(Data) == "V1")] <- "Digit"

### Scale the data set target variable ###

Data$Digit <- scale(Data$Digit)
##################################################################
str(Data)

## Now convert our target  factor that is 1st attriute  to factor as we want to classify all data in 10 levels ie 0-9
View(Data)
Data$Digit <- as.factor(Data$Digit)
str(Data)
## succesfully converted the first attribute  that is digit to factor with levels 10 that means we have 10 level from 0 to 9
################################################


###Understanding Dimensions  of dataset
dim(Data)
##dim(Data)
 #60000   785
##################################################

##############################################

##Printing the first rows of dataset 

head(Data)
##################################333

###Exploring the data using summary 
summary(Data)

##############################33

### Checking the missing value is any attribute have###

sapply(Data, function(x) sum(is.na(x)))
### As a result No missing value in  dataset
################################################
# Split the data into train and test set

set.seed(1)
 ##taken 10 % of dataset that why used 0.1 *nrow 
train.indices = sample(1:nrow(Data), 0.1*nrow(Data))
train = Data[train.indices, ]
test = Data[-train.indices, ]


#########################################################################33
#####Constructing  model####################################3333
#Using Linear Kernel#####


 #Linear model - SVM  at Cost(C) = 1
#####################################################################
Model_1 <- ksvm(Digit~ ., data = train, scale = FALSE, kernel = "vanilladot",C=1)
# Model with C =1

# Predicting the model results 
evaluate_1<- predict(model_1, test)

# Confusion Matrix - Finding accuracy, Sensitivity and specificity
confusionMatrix(evaluate_1, test$Digit)

##                             
##Accuracy : 0.9496          
##Sensitivity           0.98340   0.9832  0.94591  0.91783  0.94895
##Specificity           0.99717   0.9950  0.99362  0.99476  0.99378
#################################################################
# Model with C =2
Model_2 <- ksvm(Digit~ ., data = train, scale = FALSE, kernel = "vanilladot",C=2)

# Predicting the model results 
evaluate_2<- predict(model_2, test)

# Confusion Matrix - Finding accuracy, Sensitivity and specificity
confusionMatrix(evaluate_2, test$Digit)

##Accuracy : 0.9496          
 ##Sensitivity           0.98340   0.9832  0.94591  0.91783  0.94895
 ##Specificity           0.99717   0.9950  0.99362  0.99476  0.99378

### As we increased value of C , the accuracy also inceasing 

########################################################
# Model with C =5
Model_5 <- ksvm(Digit~ ., data = train, scale = FALSE, kernel = "vanilladot",C=5)

# Predicting the model results 
evaluate_5<- predict(model_5, test)

# Confusion Matrix - Finding accuracy, Sensitivity and specificity
confusionMatrix(evaluate_5, test$Digit)

##Accuracy : 0.9591
#Sensitivity           0.98453   0.9846  0.95929  0.93372  0.96057
#Specificity           0.99708   0.9960  0.99449  0.99569  0.99506



# Model with C =10
Model_10 <- ksvm(Digit~ ., data = train, scale = FALSE, kernel = "vanilladot",C=10)

# Predicting the model results 
evaluate_10<- predict(model_10, test)

# Confusion Matrix - Finding accuracy, Sensitivity and specificity
confusionMatrix(evaluate_10, test$Digit)

#Accuracy : 0.9588 
#Sensitivity           0.98472   0.9851  0.95911  0.93462  0.95867
#Specificity           0.99700   0.9961  0.99455  0.99544  0.99485

#######WE CONCLUDE THAT WE GET HIGHEST ACCURACY  AFTER PUTTING VALUE OF c TO 5 
 # we get 95.91 that is highest among all value from C 1 to 10

############################################################################3



#####################################################################
# Hyperparameter tuning and Cross Validation  - Linear - SVM

#####################################################################
                 
# We will use the train function from caret package to perform crossvalidation
        

trainControl <- trainControl(method="cv", number=5)
#Selected  number of folds is 5  
# Number - Number of folds 
# Method - cross validation

metric <- "Accuracy"

fit.svm <- train(Digit~., data=train, method="svmLinear", metric=metric, tuneGrid=grid, trControl=trainControl)

set.seed(100)


# making a grid of C values. 
grid <- expand.grid(C=seq(1, 10, by=1))

# Printing cross validation result
print(fit.svm)
# Best tune at C=5, 
# Accuracy - 0.959

# Plotting "fit.svm" results
plot(fit.svm)







#################################################################
###Now using with kernel = polydot
# Model with C =1
Model_1 <- ksvm(Digit~ ., data = train, scale = FALSE, kernel = "polydot",C=1)

# Predicting the model results 
evaluate_1<- predict(model_1, test)

# Confusion Matrix - Finding accuracy, Sensitivity and specificity
confusionMatrix(evaluate_1, test$Digit)
########    Accuracy : 0.9491  ############

#Sensitivity           0.98377   0.9832  0.94535  0.91728  0.94800
#Specificity           0.99710   0.9950  0.99362  0.99472  0.99368

#########################################################################3
# Model with C =5 and kernel =polydot
Model_5 <- ksvm(Digit~ ., data = train, scale = FALSE, kernel = "polydot",C=5)

# Predicting the model results 
evaluate_5<- predict(model_5, test)

# Confusion Matrix - Finding accuracy, Sensitivity and specificity
confusionMatrix(evaluate_5, test$Digit)

## ######   Accuracy : 0.9591 ######
#Sensitivity           0.98453   0.9846  0.95929  0.93372  0.96057
#Specificity           0.99708   0.9960  0.99449  0.99569  0.99506


###################################################################
# Model with C =10 and kernel =polydot
Model_10 <- ksvm(Digit~ ., data = train, scale = FALSE, kernel = "polydot",C=10)

# Predicting the model results 
evaluate_10<- predict(model_10, test)

# Confusion Matrix - Finding accuracy, Sensitivity and specificity
confusionMatrix(evaluate_10, test$Digit)


### Accuracy : 0.9589    #################################
###Sensitivity           0.98472   0.9851  0.95911  0.93462  0.95886
##Specificity           0.99700   0.9961  0.99457  0.99544  0.99485

######################################################################
# Hyperparameter tuning and Cross Validation  - Linear - SVM

#####################################################################

# We will use the train function from caret package to perform crossvalidation


trainControl <- trainControl(method="cv", number=5)
#Selected  number of folds is 5  
# Number - Number of folds 
# Method - cross validation

metric <- "Accuracy"

fit.svm <- train(Digit~., data=train, method="svmpoly", metric=metric, tuneGrid=grid, trControl=trainControl)

set.seed(100)


# making a grid of C values. 
grid <- expand.grid(C=seq(1, 10, by=1))

# Printing cross validation result
print(fit.svm)
# Best tune at C=10, 
# Accuracy - 0.959

# Plotting "fit.svm" results
plot(fit.svm)



#########################################################################

##3 Non-Linear - Kernels ######3
######################################################################

# RBF kernel 
model_rbf <- ksvm(Digit ~ ., data =train,scale=FALSE, kernel = "rbfdot")

# Predicting the model results 
Eval_RBF<- predict(model_rbf, test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test$Digit)
# Accuracy    : 0.9268
# Sensitivity : 0.9593        
# Specificity : 0.8768 

#####################################################################
#Hyperparameter tuning and Cross Validation - Non-Linear - SVM 
######################################################################

# We will use the train function from caret package to perform crossvalidation

# Making grid of "sigma" and C values. 
grid <- expand.grid(.sigma=seq(0.01, 0.05, by=0.01), .C=seq(1, 5, by=1))

# Performing 5-fold cross validation
fit.svm_radial <- train(Digit~., data=train, method="svmRadial", metric=metric, tuneGrid=grid, trControl=trainControl)

# Printing cross validation result
print(fit.svm_radial)
# Best tune at sigma = 0.03 & C=2, Accuracy - 0.9501

# Plotting model results
plot(fit.svm_radial)

######################################################################
# Checking overfitting - Non-Linear - SVM
######################################################################

# Validating the model results on test data
evaluate_non_linear<- predict(fit.svm_radial, test)
confusionMatrix(evaluate_non_linear, test$spam)

# Accuracy    - 0.9501
# Sensitivity - 0.9569
# Specificity - 0.9044

######################################################################

## Coming to the conclusion######
## firstly we applied Linear model with vanilladot 

## we get accuracy with C=1 ##Accuracy : 0.9496 
## we get accuracy with C=2 ##Accuracy : 0.9496 
## we get accuracy with C=5 ##Accuracy : 0.9591 
## we get accuracy with C=10 ##Accuracy : 0.9582 
## we can  selectt  as C=5  is having best accuracy

## secondly we applied Linear model with Polydot 

## we get accuracy with C=1 ##Accuracy : 0.9491 
## we get accuracy with C=5 ##Accuracy : 0.9591 
## we get accuracy with C=10 ##Accuracy : 0.9589 
## we can  selectt  as C=5  is having best accuracy


## lastly we appied model with method Svmdial
# Accuracy    : 0.9501   with sigma 0.03 and C = 2


## hence we select rbf kernel for this as gives best accuracy 
