#### Assignment on LInear regtression####
###installing neccesary packages for function
install.packages("MASS")
install.packages("car")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("dplyr")
install.packages("tidyr")
#############################


carprice <- read.csv("CarPrice_Assignment.csv")
#Let us examine the structure of the dataset
str(carprice)


##################################
#To chek  for blank and missing values
length(which(is.na(carprice)))  ### shows no NA values 
sapply(carprice,function(x)length(which(x=="")))  ## it shows no blank values
sum(duplicated(carprice))  ### shows no duplicate values 

#########################
## till now we  are sure that data contains no missing NA and duplicates 

### Split the column named Carname into company name and model 
library(dplyr)
library(tidyr)


carprice <- separate(carprice,CarName,into=c("CompanyName","Model"),sep=" ")
##removed the model which if no use  
carprice <- carprice[,-4]

#let us now view the table
View(carprice)
str(carprice)

##### Finding the outliers ###
boxplot(carprice$car_ID)
boxplot(carprice$symboling)
boxplot(carprice$enginesize)


### to check the missing values ##3

length(which(carprice$car_ID==""))

length(which(carprice$symboling==""))
length(which(carprice$CarName==""))
length(which(carprice$fueltype==""))

length(which(carprice$enginelocation==""))
length(which(carprice$drivewheel==""))
###########################################
 

###########################################
## as in data set there are many values under company name like maxda , vokswagen 
##3 which are incorrect so using gsub change the names 
 

carprice$CompanyName <- gsub("maxda","mazda",carprice$CompanyName)
carprice$CompanyName <- gsub("Nissan","nissan",carprice$CompanyName)
carprice$CompanyName <- gsub("porcshce","porsche",carprice$CompanyName)
carprice$CompanyName <- gsub("toyouta","toyota",carprice$CompanyName)
carprice$CompanyName <- gsub("vokswagen","volkswagen",carprice$CompanyName)
carprice$CompanyName <- gsub("vw","volkswagen",carprice$CompanyName)

## to convert the character into factors or levels to apply dummies
carprice$CompanyName <- as.factor(carprice$CompanyName)
str(carprice)
View(carprice)
length(carprice$fueltype)

##make dummy variables to all categorical  variables 
str(carprice$fueltype)


##To check total number of diesel and gas 
summary(factor(carprice$fueltype))



# One simple way to convert fueltype variable to numeric is to replace the levels-  diesel  and gas with 1 and 0 is:

levels(carprice$fueltype)<-c(0,1)


# Now store the numeric values in the same variable
carprice$fueltype<- as.numeric(levels(carprice$fueltype))[carprice$fueltype]

summary(factor(carprice$fueltype))

###carprice$fueltype
##it will give the all fuel type in 1 and 0

######################################################################
# Do the same for other such categorical variables like aspirations 
levels(carprice$aspiration)<-c(1,0)
carprice$aspiration<- as.numeric(levels(carprice$aspiration))[carprice$aspiration]
###carprice$aspiration

summary(carprice$aspiration)
View(carprice)

#############################################33333
## Do the same for other such categorical variables like doornumber
levels(carprice$doornumber)<-c(1,0)
carprice$doornumber<- as.numeric(levels(carprice$doornumber))[carprice$doornumber]


summary(carprice$doornumber)
#######
View(carprice)

##### Do the same for other such categorical variables like enginelocation
levels(carprice$enginelocation)<-c(1,0)
carprice$enginelocation<- as.numeric(levels(carprice$enginelocation))[carprice$enginelocation]

carprice$enginelocation

View(carprice)
##################################################################
# Now we come across variables having more than 3 levels.

summary(factor(carprice$CompanyName))
#Converting "CompanyName" into dummies . 
dummy_1 <- data.frame(model.matrix( ~CompanyName, data = carprice))

length(dummy_1)
## it is giving 22 as there are 22 levels 

#check the dummy_1 data frame.
View(dummy_1)

##there generate one column named X.intercept remove this column from dummie tables
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable 
dummy_1 <- dummy_1[,-1]
View(dummy_1)
### after removing  attach this dummi to main table and named carprice_1

carprice <- cbind(carprice[,-3], dummy_1)
View(carprice)
###################################################################3

summary(factor(carprice_1$carbody))
#Converting "carbody" into dummies . 
dummy_2 <- data.frame(model.matrix( ~carbody, data = carprice))

length(dummy_2)

#check the dummy_1 data frame.
View(dummy_2)

##there generate one column named X.intercept remove this column from dummie tables
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "carbody". 
dummy_2<- dummy_2[,-1]
View(dummy_2)
### after removing  attach this dummi to main table and named carprice_1

carprice <- cbind(carprice[,-6], dummy_2)
View(carprice)


summary(factor(carprice$carbody))
View(carprice)
####################################

#Converting "drivewheel" into dummies . 
summary(factor(carprice$drivewheel))
dummy_3 <- data.frame(model.matrix( ~drivewheel, data = carprice))



#check the dummy_1 data frame.
View(dummy_3)

##there generate one column named X.intercept remove this column from dummie tables
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "carbody". 
dummy_3 <- dummy_3[,-1]
View(dummy_3)
### after removies attach this dummi to main table and named carprice_1

carprice<- cbind(carprice[,-6], dummy_3)
View(carprice)


#######################


###for fuelsystem with 8 levels
#Converting "fuelsystem" into dummies . 
summary(factor(carprice$fuelsystem))
dummy_4 <- data.frame(model.matrix( ~fuelsystem, data = carprice))



#check the dummy_2 data frame.
View(dummy_4)

##there generate one column named X.intercept remove this column from dummie tables
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "fuelsystem". 
dummy_4 <- dummy_4[,-1]
View(dummy_4)
### after removing  attach this dummi to main table and named carprice_1

carprice <- cbind(carprice[,-15], dummy_4)
View(carprice)
str(carprice)

###for enginetype with 8 levels
#Converting "enginetype" into dummies . 
summary(factor(carprice$enginetype))
dummy_5 <- data.frame(model.matrix( ~enginetype, data = carprice))



#check the dummy_2 data frame.
View(dummy_5)

##there generate one column named X.intercept remove this column from dummie tables
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "fuelsystem". 
dummy_5 <- dummy_5[,-1]
View(dummy_5)
View(carprice)
### after removing  attach this dummi to main table and named carprice_1

carprice <- cbind(carprice[,-12], dummy_5)
View(carprice)


###
###for cylinder number  with 7levels
#Converting "cylindernumber" into dummies . 
summary(factor(carprice$cylindernumber))
dummy_6 <- data.frame(model.matrix( ~cylindernumber , data = carprice))



#check the dummy_2 data frame.
View(dummy_6)

##there generate one column named X.intercept remove this column from dummie tables
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "drivewheel". 
dummy_6 <- dummy_6[,-1]
View(dummy_6)
View(carprice)
### after removing  attach this dummi to main table and named carprice_1

carprice <- cbind(carprice[,-12], dummy_6)
View(carprice)



####################################################################333
# Divide into training and test data set

#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(carprice), 0.7*nrow(carprice))
# generate the train data set
train = carprice[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = carprice[-trainindices,]


###################################################3
#Execute the first model_1 multilinear model in the training set. 
model_1 <-lm(price~.,data=train)


# Check the summary of model. 
summary(model_1)
library(MASS)
model_1 <- stepAIC(model_1,data=train) 

# Check if the correlation matrix givessome insight.
corrs = cor(carprice)
View(corrs)  


##############
#For the calculation of VIF you need to install the package "car", 
# First let's install this package in your R system
install.packages("car")

# load the package
library("car")

# Pass the model_1 in the vif function
vif(model_1)

# Look at summary of the model again to see the P values
summary(model_1)
step(model_1)
#####################################################################3
model_2 <- lm(formula = price ~ car_ID  + aspiration + enginelocation + 
     carwidth + curbweight + enginesize + stroke + compressionratio + 
     peakrpm + citympg + CompanyNamebmw + CompanyNamechevrolet + 
     CompanyNamedodge + CompanyNamehonda + CompanyNameisuzu + 
     CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
     CompanyNamenissan + CompanyNamepeugeot + CompanyNameplymouth + 
     CompanyNameporsche + CompanyNamerenault + CompanyNamesaab + 
     CompanyNamesubaru + CompanyNametoyota + CompanyNamevolkswagen + 
     CompanyNamevolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
     carbodywagon + drivewheelrwd + fuelsystem2bbl + fuelsystem4bbl + 
     fuelsystemmpfi + cylindernumberfive, data = train)

###check the  VIF value of model_2#########
length(model_2)

vif(model_2)

##########  to check the value of p used summary of model_2  
summary(model_2)

 step(model_2)
 
#### Company name volve is removed  as it contain  high vif  

 model_3 <- lm(formula = price ~ car_ID + aspiration + enginelocation + carwidth + 
      curbweight + enginesize + stroke + peakrpm + citympg + CompanyNamebmw + 
      CompanyNamechevrolet + CompanyNamedodge + CompanyNamehonda + 
      CompanyNameisuzu + CompanyNamemazda + CompanyNamemercury + 
      CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
      CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
      CompanyNamesaab + CompanyNamesubaru + CompanyNametoyota + 
      CompanyNamevolkswagen + carbodyhardtop + 
      carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
      fuelsystem2bbl + fuelsystem4bbl + fuelsystemmpfi + cylindernumberfive, 
    data = train)

 vif(model_3) 

 
 summary(model_3) 
 ##########################################3
 #####remove company name mazda n is removed
 
 model_4  <- lm(formula = price ~ car_ID + aspiration + enginelocation + carwidth + 
      curbweight + enginesize + stroke + peakrpm + citympg + CompanyNamebmw + 
      CompanyNamechevrolet + CompanyNamedodge + CompanyNamehonda + 
      CompanyNameisuzu + CompanyNamemercury + 
      CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
      CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
      CompanyNamesaab + CompanyNamesubaru + CompanyNametoyota + 
      CompanyNamevolkswagen + carbodyhardtop + 
      carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
      fuelsystem2bbl + fuelsystem4bbl + fuelsystemmpfi + cylindernumberfive, 
    data = train)

 
 summary(model_4) 

 
 vif(model_4) 
 summary(model_4)
 
 ######################3
 step(model_4)
#########################################33
 model_5 <- lm(formula = price ~ car_ID + aspiration + enginelocation + carwidth + 
      curbweight + enginesize + stroke + peakrpm + citympg + CompanyNamebmw + 
      CompanyNamechevrolet + CompanyNamedodge + CompanyNamehonda + 
      CompanyNameisuzu + CompanyNamemercury + 
      CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
      CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
       CompanyNamesubaru + CompanyNametoyota + 
      CompanyNamevolkswagen + carbodyhardtop + 
      carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
      fuelsystem2bbl + fuelsystem4bbl + fuelsystemmpfi + cylindernumberfive, 
    data = train)

 
  summary(model_5) 
  
  vif(model_5)
  #################################################
  model_6 <- lm(formula = price ~ car_ID + enginelocation + carwidth + 
                  curbweight + enginesize + stroke + peakrpm + citympg + CompanyNamebmw + 
                  CompanyNamechevrolet + CompanyNamedodge + CompanyNamehonda + 
                  CompanyNameisuzu + CompanyNamemercury + 
                  CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                  CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
                  CompanyNamesubaru + CompanyNametoyota + 
                  CompanyNamevolkswagen + carbodyhardtop + 
                  carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                  fuelsystem2bbl + fuelsystem4bbl + fuelsystemmpfi + cylindernumberfive, 
                data = train)
  
  
  summary(model_6)
vif(model_6)
######################################################33
model_7 <- lm(formula = price ~ car_ID + enginelocation + carwidth +  + enginesize + stroke + peakrpm + citympg + CompanyNamebmw + 
                CompanyNamechevrolet + CompanyNamedodge + CompanyNamehonda + 
                CompanyNameisuzu + CompanyNamemercury + 
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
                CompanyNamesubaru + CompanyNametoyota + 
                CompanyNamevolkswagen + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                fuelsystem2bbl + fuelsystem4bbl + fuelsystemmpfi + cylindernumberfive, 
              data = train)

summary(model_7)  

vif(model_7)

model_8 <- lm(formula = price ~ car_ID + enginelocation + carwidth + 
                 + enginesize + stroke + citympg + CompanyNamebmw + 
                CompanyNamechevrolet + CompanyNamedodge + CompanyNamehonda + 
                CompanyNameisuzu + CompanyNamemercury + 
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
                CompanyNamesubaru + CompanyNametoyota + 
                CompanyNamevolkswagen + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                fuelsystem2bbl + fuelsystem4bbl + fuelsystemmpfi + cylindernumberfive, 
              data = train)

summary(model_8)
vif(model_8)

step(model_8)
#################################################################33
model_9 <- lm(formula = price ~ car_ID + enginelocation + carwidth + 
                + enginesize + stroke +  CompanyNamebmw + 
                CompanyNamechevrolet + CompanyNamedodge + CompanyNamehonda + 
                CompanyNameisuzu + CompanyNamemercury + 
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
                CompanyNamesubaru + CompanyNametoyota + 
                CompanyNamevolkswagen + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                fuelsystem2bbl + fuelsystem4bbl + fuelsystemmpfi + cylindernumberfive, 
              data = train)

summary(model_9)
vif(model_9)
######################################################################33
model_10 <- lm(formula = price ~ car_ID + enginelocation + carwidth + 
                + enginesize + stroke +  CompanyNamebmw + 
                CompanyNamechevrolet + CompanyNamedodge +  
                CompanyNameisuzu + CompanyNamemercury + 
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
                CompanyNamesubaru + CompanyNametoyota + 
                CompanyNamevolkswagen + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                fuelsystem2bbl + fuelsystem4bbl + fuelsystemmpfi + cylindernumberfive, 
              data = train)

summary(model_10)
vif(model_10)
########################################################

model_11 <- lm(formula = price ~ car_ID + enginelocation + carwidth + 
                 + enginesize + stroke +  CompanyNamebmw + 
                 CompanyNamechevrolet + CompanyNamedodge +  
                 CompanyNameisuzu + CompanyNamemercury + 
                 CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                 CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
                 CompanyNamesubaru + CompanyNametoyota + 
                 CompanyNamevolkswagen + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon +  
                 fuelsystem2bbl + fuelsystem4bbl + fuelsystemmpfi + cylindernumberfive, 
               data = train)

summary(model_11)
vif(model_11)
###############################################################3
model_12 <- lm(formula = price ~ car_ID + enginelocation + carwidth + 
                 + enginesize + stroke +  CompanyNamebmw + 
                 CompanyNamechevrolet + CompanyNamedodge +  
                 CompanyNameisuzu + CompanyNamemercury + 
                 CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                 CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
                 CompanyNamesubaru + CompanyNametoyota + 
                 CompanyNamevolkswagen + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon +  
                  fuelsystem4bbl + fuelsystemmpfi + cylindernumberfive, 
               data = train)

summary(model_12)

vif(model_12)
########################################333
model_13 <- lm(formula = price ~ car_ID + enginelocation + carwidth + 
                 + enginesize + stroke +  CompanyNamebmw + 
                 CompanyNamechevrolet + CompanyNamedodge +  
                 CompanyNameisuzu + CompanyNamemercury + 
                 CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                 CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
                 CompanyNamesubaru + CompanyNametoyota + 
                 CompanyNamevolkswagen + carbodyhardtop + 
                 carbodyhatchback +  carbodywagon +  
                 fuelsystem4bbl + fuelsystemmpfi + cylindernumberfive, 
               data = train)

summary(model_13)
vif(model_13)
#########################################33
model_14<- lm(formula = price ~ car_ID + enginelocation + carwidth + 
                 + enginesize + stroke +  CompanyNamebmw + 
                 CompanyNamechevrolet + CompanyNamedodge +  
                 CompanyNameisuzu + CompanyNamemercury + 
                 CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                 CompanyNameplymouth +  CompanyNamerenault + 
                 CompanyNamesubaru + CompanyNametoyota + 
                 CompanyNamevolkswagen + carbodyhardtop + 
                 carbodyhatchback +  carbodywagon +  
                 fuelsystem4bbl + fuelsystemmpfi + cylindernumberfive, 
               data = train)

summary((model_14))
vif(model_14)
#######################################33
model_15<- lm(formula = price ~  enginelocation + carwidth + 
                + enginesize + stroke +  CompanyNamebmw + 
                CompanyNamechevrolet + CompanyNamedodge +  
                CompanyNameisuzu + CompanyNamemercury + 
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth +  CompanyNamerenault + 
                CompanyNamesubaru + CompanyNametoyota + 
                CompanyNamevolkswagen + carbodyhardtop + 
                carbodyhatchback +  carbodywagon +  
                fuelsystem4bbl + fuelsystemmpfi + cylindernumberfive, 
              data = train)


summary(model_15)
vif(model_15)
########################################################
model_16<- lm(formula = price ~  enginelocation + carwidth + 
                + enginesize + stroke +  CompanyNamebmw + 
                CompanyNamechevrolet + CompanyNamedodge +  
                CompanyNameisuzu + CompanyNamemercury + 
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth +  CompanyNamerenault + 
                CompanyNamesubaru + CompanyNametoyota + 
                CompanyNamevolkswagen + carbodyhardtop + 
                carbodyhatchback +  carbodywagon +  
                fuelsystem4bbl  + cylindernumberfive, 
              data = train)



summary(model_16)
vif(model_16)
#########################################333333
model_17<- lm(formula = price ~  enginelocation + carwidth + 
                + enginesize + stroke +  CompanyNamebmw + 
                CompanyNamechevrolet + CompanyNamedodge +  
                CompanyNameisuzu + CompanyNamemercury + 
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth +  CompanyNamerenault + 
                CompanyNamesubaru + CompanyNametoyota + 
                CompanyNamevolkswagen + carbodyhardtop + 
                carbodyhatchback +     fuelsystem4bbl  + cylindernumberfive, 
              data = train)

summary(model_17)
vif(model_17)
#######################################################33
model_18<- lm(formula = price ~  enginelocation + carwidth + 
                + enginesize + stroke +  CompanyNamebmw + 
                CompanyNamechevrolet + CompanyNamedodge +  
                CompanyNameisuzu  + 
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth +  CompanyNamerenault + 
                CompanyNamesubaru + CompanyNametoyota + 
                CompanyNamevolkswagen + carbodyhardtop + 
                carbodyhatchback +     fuelsystem4bbl  + cylindernumberfive, 
              data = train)


summary(model_18)
vif(model_18)
#########################
model_19<- lm(formula = price ~  enginelocation + carwidth + 
                + enginesize + stroke +  CompanyNamebmw + 
                CompanyNamechevrolet + CompanyNamedodge +  
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth +  CompanyNamerenault + 
                CompanyNamesubaru + CompanyNametoyota + 
                CompanyNamevolkswagen + carbodyhardtop + 
                carbodyhatchback +     fuelsystem4bbl  + cylindernumberfive, 
              data = train)


summary(model_19)
vif(model_19)
#####################################################
model_20<- lm(formula = price ~  enginelocation + carwidth + 
                + enginesize + stroke +  CompanyNamebmw + 
                CompanyNamechevrolet +  
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth +  CompanyNamerenault + 
                CompanyNamesubaru + CompanyNametoyota + 
                CompanyNamevolkswagen + carbodyhardtop + 
                carbodyhatchback +     fuelsystem4bbl  + cylindernumberfive, 
              data = train)


summary(model_20)
################
model_21<- lm(formula = price ~  enginelocation + carwidth + 
                + enginesize + stroke +  CompanyNamebmw + 
                CompanyNamechevrolet +  
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth +  CompanyNamerenault + 
                CompanyNamesubaru + CompanyNametoyota + 
                CompanyNamevolkswagen  + 
                carbodyhatchback +     fuelsystem4bbl  + cylindernumberfive, 
              data = train)


summary(model_21)
vif(model_21)
##############################################################3
model_22<- lm(formula = price ~  enginelocation + carwidth + 
                + enginesize + stroke +  CompanyNamebmw + 
                CompanyNamechevrolet +  
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot +  CompanyNamerenault + 
                CompanyNamesubaru + CompanyNametoyota + 
                CompanyNamevolkswagen  + 
                carbodyhatchback +     fuelsystem4bbl  + cylindernumberfive, 
              data = train)
summary(model_22)
vif(model_22)
#################################################
model_23<- lm(formula = price ~  enginelocation + carwidth + 
                + enginesize + stroke +  CompanyNamebmw + 
                CompanyNamechevrolet +  
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot +  CompanyNamerenault + 
                CompanyNamesubaru  + 
                CompanyNamevolkswagen  + 
                carbodyhatchback +     fuelsystem4bbl  + cylindernumberfive, 
              data = train)
summary(model_23)
vif(model_23)
################################################33
model_24<- lm(formula = price ~  enginelocation + carwidth + 
                + enginesize + stroke +  CompanyNamebmw + 
                CompanyNamechevrolet +  
                CompanyNamemitsubishi + CompanyNamepeugeot +  CompanyNamerenault + 
                CompanyNamesubaru  + CompanyNamevolkswagen  + 
                carbodyhatchback +     fuelsystem4bbl  + cylindernumberfive, 
              data = train)

summary(model_24)
#################################################
model_25<- lm(formula = price ~  enginelocation + carwidth + 
                + enginesize + stroke +  CompanyNamebmw + 
                CompanyNamechevrolet +  
                CompanyNamemitsubishi + CompanyNamepeugeot +  CompanyNamerenault + 
                CompanyNamesubaru   + 
                carbodyhatchback +     fuelsystem4bbl  + cylindernumberfive, 
              data = train)
summary(model_25)
#####################################################33
model_25<- lm(formula = price ~  enginelocation + carwidth + 
                + enginesize + stroke +  CompanyNamebmw + 
                CompanyNamechevrolet +  
                CompanyNamemitsubishi + CompanyNamepeugeot +  CompanyNamerenault + 
                CompanyNamesubaru   + 
                carbodyhatchback +     fuelsystem4bbl ,  
              data = train)



summary(model_25)
vif(model_25)

##########################################
model_26<- lm(formula = price ~  enginelocation + carwidth + 
                + enginesize + stroke +  CompanyNamebmw + 
                CompanyNamechevrolet + CompanyNamepeugeot +  CompanyNamerenault + 
                CompanyNamesubaru   + 
                carbodyhatchback +     fuelsystem4bbl ,  
              data = train)

summary(model_26)
vif(model_26)
##############################################333

model_27<- lm(formula = price ~  enginelocation + carwidth + 
                + enginesize + stroke +  CompanyNamebmw + 
                CompanyNamechevrolet + CompanyNamepeugeot +  CompanyNamerenault + 
                CompanyNamesubaru  +     fuelsystem4bbl ,  
              data = train)

summary(model_27)
vif(model_27)
#########################################33
model_28<- lm(formula = price ~  enginelocation + carwidth + 
                + enginesize + stroke +  CompanyNamebmw + 
                CompanyNamechevrolet + CompanyNamepeugeot + 
                CompanyNamesubaru  +     fuelsystem4bbl ,  
              data = train)

summary(model_28)
vif(model_28)
####################################################
model_29<- lm(formula = price ~  enginelocation + carwidth + 
                + enginesize + stroke +  CompanyNamebmw + 
                CompanyNamechevrolet  + 
                CompanyNamesubaru  +     fuelsystem4bbl ,  
              data = train)

summary(model_29)
vif(model_29)
#################################################3
model_30<- lm(formula = price ~  enginelocation + carwidth + 
                + enginesize + stroke +  CompanyNamebmw + 
                CompanyNamechevrolet  +     fuelsystem4bbl ,  
              data = train)

summary(model_30)
vif(model_30)
# Predict the house prices in the testing dataset
Predict_1 <- predict(model_30,test[,])
test$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2

# check R-squared
rsquared
