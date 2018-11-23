##################################### HR Analytics ##################################
#-------------------------------- Logistic Regression ------------------------------#
#
# CRISP-DM approach followed to address the XYZ company attrition 
#
# ########################## Business understanding #################################
#
# A large company named XYZ, employs, at any given point of time, around 4000 
# employees. However, every year, around 15% of its employees leave the company and
# need to be replaced with the talent pool available in the job market.The management
# believes that this level of attrition (employees leaving, either on their own or
# because they got fired) is bad for the company.
#
# -------------------------- Business Objective ----------------------------------- #
#
# To retain the employees for the following reason:
# 1. The former employees' projects get delayed, which makes it difficult to meet
# timelines, resulting in a reputation loss among consumers and partners.
#
# 2. A sizeable department has to be maintained, for the purposes of recruiting 
# new talent
#
# 3. More often than not, the new employees have to be trained for the job and/or 
# given time to acclimatise themselves to the company
#
# ------------------------- Goal of Data Analysis ----------------------------------#
#
# The goal of the data analysis is to understand what factors they should focus on,
# in order to curb attrition. In other words, XYZ Comapany want to know what changes
# they should make to their workplace, in order to get most of their employees to stay.
# Also, they want to know which of these variables is most important and needs to be
# addressed right away.
#
############################### Data Understanding ##################################
#
# Source of data - Provided by XYZ firm in 5 categories
# Data Description and preperation needs
# 1. employee_survey_data - Contains employee satisfaction about Environment, job and
# work life balance. Check for duplicate and missing values
#
# 2. general_data - contains employee general details like age, department, JobLevel,
# JobRole, MonthlyIncome, YearsSinceLastpromotion, MaritalStatus etc. Check for
# duplicate and missing value. Also to identify not usefull variables and remove them.
#
# 3. manager_survey_data - contains performanceRating and JobInvolvementRating. Check
# duplicate and missing values
#
# 4. in_time - contains in time of the employee. check for duplicate and missing
# values. Convert date and time to standard format. Add column name to employeeId
# check for dulicate and remove NAs
#
# 5. out_time - contains out time of the employee. check for duplicate and missing
# values. Convert date and time to standard format. Add column name to employeeId
# check for dulicate and remove NAs
#
################################## Data Preperation #################################
# 
# Load Required Libraries
# install pacman package to load and/or install libraires
# reference - https://cran.r-project.org/web/packages/pacman/vignettes/Introduction_to_pacman.html
install.packages("pacman")

pacman::p_load(data.table, dplyr, ggplot2, stringr, DT, tidyr, corrplot, lubridate,
              scales, gridExtra, MASS, car, caret, cowplot, caTools,
              GGally, e1071, Information, InformationValue)

# Set working directory to read data

setwd("D:/Lectures/Data Science/Upgrad/Predictive Analytics - Course III/HR Analytics")

# loading data to R and check structure for dataset

employee_survey_data <- read.csv(file="employee_survey_data.csv",
                                 stringsAsFactors = FALSE)
str(employee_survey_data)

general_data <- read.csv(file="general_data.csv",
                         stringsAsFactors = FALSE)
str(general_data)

manager_survey_data <- read.csv(file="manager_survey_data.csv",
                                stringsAsFactors = FALSE)
str(manager_survey_data)

in_time <- read.csv("in_time.csv", stringsAsFactors = FALSE)
glimpse(in_time) # contains NA values

out_time <- read.csv("out_time.csv", stringsAsFactors = FALSE)
glimpse(out_time) # contains NA value

# Check for duplicates using key field EmployeeID - zero means no duplicate

sum(duplicated(general_data$EmployeeID))         #Return 0
sum(duplicated(employee_survey_data$EmployeeID)) #Return 0
sum(duplicated(manager_survey_data$EmployeeID))  #Return 0
sum(duplicated(in_time$EmployeeID))              #Return 0
sum(duplicated(out_time$EmployeeID))             #Return 0

#------------------------------- Data Cleanining -----------------------------------#
# Check individual dataset for NA values

sum(is.na(general_data)) # 28
mean(is.na(general_data)) # not even 1% NA values
sum(is.na(employee_survey_data)) # 83
mean((is.na(employee_survey_data))) # Not even 1% NA values
sum(is.na(manager_survey_data)) # 0 means 0% NA values

# Check coloumn wise NA Values in general_data dataset

sapply(general_data, function(x) length(which(is.na(x))))

# There are 19 NA's in "NumCompaniesWorked"  and 9 NA's in TotalWorkingYears

# NumCompaniesWorked: replace NA with 2 if difference of TotalWorkingYears and
# YearsAtCompany is greater than or equal to 1 or else with 1. But
# TotalWorkingYears have 9 NAs. So,
# TotalWorkingYears: replace NA with YearsAtCompany if NumCompaniesWorked is 0 or 1
# else replace withh (YearsAtCompany + NumCompaniesWorked) assuming employee worked
# for atleast 1 year in each company

general_data$TotalWorkingYears[(which(is.na(general_data$TotalWorkingYears)))] <- 
   ifelse(general_data$NumCompaniesWorked[(which
      (is.na(general_data$TotalWorkingYears)))] == 0, 
         general_data$YearsAtCompany[(which
            (is.na(general_data$TotalWorkingYears)))], 
   ifelse(general_data$NumCompaniesWorked[(which
      (is.na(general_data$TotalWorkingYears)))] == 1,
         general_data$YearsAtCompany [(which
            (is.na(general_data$TotalWorkingYears)))],
               (general_data$YearsAtCompany [(which
                  (is.na(general_data$TotalWorkingYears)))]
                      + general_data$NumCompaniesWorked[(which
                          (is.na(general_data$TotalWorkingYears)))])))

sapply(general_data, function(x) length(which(is.na(x)))) # 0 NA in TotalWorkingYears

# Calculate difference of TotalWorkingYears and YearsAtCompany to replace NA in
# general_data$NumCompaniesWorked

general_data$year_diff <- general_data$TotalWorkingYears - general_data$YearsAtCompany

general_data$NumCompaniesWorked[(which(is.na(general_data$NumCompaniesWorked)))] <- 
  ifelse(general_data$year_diff[(which
    (is.na(general_data$NumCompaniesWorked)))] == 0, 1, 2) 

sapply(general_data, function(x) length(which(is.na(x)))) #0 NA in NumCompaniesWorked

# Remove year_diff from general_data

general_data <- general_data[, -25] # last column

# Check coloumn wise NA Values in employee_survey_data dataset

sapply(employee_survey_data, function(x) length(which(is.na(x))))

# There are 25 NA in EnvironmentSatisfaction, 20 NA in JobSatisfaction and 38 NA in 
# WorkLifeBalance. Since all are are categorical variable it can be removed with Mode.

employee_survey_data$EnvironmentSatisfaction[which
    (is.na(employee_survey_data$EnvironmentSatisfaction))] <- which.max(table(employee_survey_data$EnvironmentSatisfaction))

employee_survey_data$JobSatisfaction[which
     (is.na(employee_survey_data$JobSatisfaction))] <- which.max(table(employee_survey_data$JobSatisfaction))

employee_survey_data$WorkLifeBalance[which
   (is.na(employee_survey_data$WorkLifeBalance))] <- which.max(table(employee_survey_data$WorkLifeBalance))

sapply(employee_survey_data, function(x) length(which(is.na(x)))) # 0 Na Value

# Data Cleaning for in_time and out_time dataset
# Convert wide to long format using gather function, remove X from date, 

in_time <- gather(in_time, Date_in, Time_in, X2015.01.01:X2015.12.31)
out_time <- gather(out_time, Date_out, Time_out, X2015.01.01:X2015.12.31)

# Remove prefix X from date column and add column name EmployeeId in column 1

in_time$Date_in <- str_replace(in_time$Date_in, "X", "")
out_time$Date_out <- str_replace(out_time$Date_out, "X", "")

colnames(in_time)[1] <- "EmployeeId"
colnames(out_time)[1] <- "EmployeeId"

# Merge both dataset

time_sheet <- merge(in_time, out_time, by.x = c("EmployeeId", "Date_in"), 
                    by.y = c("EmployeeId", "Date_out"))
glimpse(time_sheet)

# Check for NA values and mismatch in in-out time.

sum(is.na(time_sheet$Time_in))     # 109080 NA Values 
mean(is.na(time_sheet$Time_in))    # Approx. 9% of the data
sum(is.na(time_sheet$Time_out))   # 109080 NA Values
mean(is.na(time_sheet$Time_out))  # Approx. 9% of the data

# Number of missing values are matching, it means there is no mismtach in in-out time.
# check on in and out time on date basis 

sum(is.na(time_sheet$Time_in) & !is.na(time_sheet$Time_out)) # no mismatch
sum(!is.na(time_sheet$Time_in) & is.na(time_sheet$Time_out)) # no mismatch

# We can remove NA value from the datasets.

time_sheet <- time_sheet[!is.na(time_sheet$Time_in),]
sapply(time_sheet, function(x) length(which(is.na(x)))) # 0 Na vlaue

# Convert to date and time format

time_sheet$Time_in <- parse_date_time(time_sheet$Time_in, c("Ymd_HMS"))
time_sheet$Time_out <- parse_date_time(time_sheet$Time_out, c("Ymd_HMS"))
str(time_sheet)

# Calculate employee's working hours - in and out time difference

time_sheet$Time_diff <-  difftime(time_sheet$Time_out, time_sheet$Time_in, units = c("hours"))

# Calculate average working hours for each employee

avg_work_hour <- aggregate(time_sheet$Time_diff, by = list(time_sheet$EmployeeId),
                        mean, na.rm=TRUE)
# Add column names and round of avg_working_hrs upto 2 decimal palces

colnames(avg_work_hour) <- c("EmployeeID", "avg_working_hrs")
avg_work_hour$avg_working_hrs <- round(avg_work_hour$avg_working_hrs, 2)

# check for key value and merge all dataset - avg_work_hour, employee_survey_data,
# general-data, manager_survey_data

setdiff(general_data$EmployeeID, avg_work_hour$EmployeeID)         # 0 - key are matchings
setdiff(general_data$EmployeeID, employee_survey_data$EmployeeID)  # 0 - key are matchings
setdiff(general_data$EmployeeID, manager_survey_data$EmployeeID)   # 0 - key are matchings

master_data <- merge(general_data, avg_work_hour, by = "EmployeeID")
master_data <- merge(master_data, employee_survey_data, by = "EmployeeID")
master_data <- merge(master_data, manager_survey_data, by = "EmployeeID")
str(master_data)              # 4410 observation and 30 variable
sum(is.na(master_data))        # No NA Values
sapply(master_data, function(x) length(which(x == "")))  # NO Blank Space

# Check for unique columns and remove
# EmployeeCount, Over18, StandardHours are unique columns

sapply(master_data, function(x) length(unique(x)))  # length = 1 are unique
remove_col <- c("EmployeeCount", "Over18", "StandardHours")
master_data[, remove_col] <- NULL

# Check Attrition percentage and plot the graph(% and count of attrition)
# Approx 16% (count = 711) of the employees left the company in 2015.  

master_data_copy <- master_data %>% group_by(Attrition) %>%
                        summarise(count1 = (n()))

attrition <- ggplot(master_data_copy, aes(x= Attrition, y = count1, fill = Attrition)) + 
               geom_bar(stat = "identity") + 
                 geom_text(aes(label = c(round((count1/nrow(master_data)) * 100)), vjust = 0)) +
                   geom_text(aes(label = count1), vjust = 2)
attrition
rm(master_data_copy)

#--------------------------------- Exploratory Data Analysis ---------------------------------# 
#
# Stacked Bar chart analysis for categorical variables 

theme_1 <- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot_1 <- ggplot(master_data, aes(x = BusinessTravel, fill = Attrition)) + geom_bar() + theme_1
plot_2 <- ggplot(master_data, aes(x = Department, fill = Attrition)) + geom_bar() + theme_1 
plot_3 <- ggplot(master_data, aes(x = EducationField, fill = Attrition)) + geom_bar() + theme_1 
plot_4 <- ggplot(master_data, aes(x = Gender,fill = Attrition)) + geom_bar() + theme_1
plot_5 <- ggplot(master_data, aes(x = JobLevel, fill = Attrition))+ geom_bar() + theme_1 
plot_6 <- ggplot(master_data, aes(x = MaritalStatus, fill = Attrition)) + geom_bar() + theme_1

plot_grid(plot_1, plot_2, plot_3, plot_4, plot_5, plot_6)                   
rm(plot_1,plot_2, plot_3,plot_4,plot_5,plot_6)

# Conclusions from the chart:
# Those who travelled rarely tend to leave company more.
# Attrition is less for HR dept. as low proportion of HR employees are there in an organization. Also Research & 
# Development Dept. have high attrition 
# In correlation with Department, HR have less attrition if considering education field
# Attrition rate is more among male employees as we can assume compared to female employees they tend to have more
# dependants which results in seeking a good package
# Assuming Joblevel 1 denotes employees who have just entered the role/field and 5 denoting highly experienced or 
# experts of the field. From the plot we understand that as the level increases people are less tend to leave 
# company
# Attrition is more among singles and less among divorcees

plot_7 <- ggplot(master_data, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar() + theme_1
plot_8 <- ggplot(master_data, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar() + theme_1
plot_9 <- ggplot(master_data, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar() + theme_1
plot_10 <- ggplot(master_data, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar() + theme_1
plot_11 <- ggplot(master_data, aes(x=JobInvolvement,fill=Attrition))+ geom_bar() + theme_1
plot_12 <- ggplot(master_data, aes(x= factor(PerformanceRating),fill=Attrition))+ geom_bar() + theme_1

plot_grid(plot_7, plot_8, plot_9, plot_10, plot_11, plot_12)
rm(plot_7, plot_8, plot_9, plot_10, plot_11, plot_12)

# Conclusions from the chart:
# StockOptionLevel - More employees quit in lower levels
# EnvironmentSatisfaction - More employees quit where the statisfaction level is 1 compared to other levels. But not
# showing any significant trend
# JobSatisfaction - Higher attrition among Job Satisfaction level 1.
# WorkLifeBalance - if considering the proportion of data, attrition is more in level 1 which is as expected.
# JobInvolvement  - Attrition is more with involvement level 3
# PerformanceRating - compared to rating 4, rating 3 has high attrition amount.
#######################################################################################
# Histogram and Boxplots for numeric variables 

box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

# No outliers in the numeric variables

plot_grid(ggplot(master_data, aes(PercentSalaryHike))+ geom_histogram(binwidth = 5),
          ggplot(master_data, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1) + coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(master_data, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 5),
          ggplot(master_data, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(master_data, aes(DistanceFromHome))+ geom_histogram(binwidth = 5),
          ggplot(master_data, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

# outliers in the  numeric variables

plot_grid(ggplot(master_data, aes(YearsAtCompany))+ geom_histogram(binwidth = 5),
          ggplot(master_data, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(master_data, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 5),
          ggplot(master_data, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(master_data, aes(MonthlyIncome))+ geom_histogram(),
          ggplot(master_data, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

# few outliers in  MonthlyIncome, YearsAtCompany, YearsSinceLastPromotion,
# which is quite possible so ignoring

##################################################################################################################
# creating new field to understand whether employee is doing extended working hours or not
# As the standard working hours is 8 from data, those who have an average working hours more 8 can be considered 
# for doing extended working hours
##################################################################################################################

master_data$extended_working_hrs <- ifelse(master_data$avg_working_hrs > 8,"Yes","No")

master_data_copy_1 <- master_data %>% group_by(Attrition, extended_working_hrs ) %>% summarise(countb = n())

plot_extended_wrkhrs <- ggplot(master_data_copy_1, aes(extended_working_hrs, y=countb, fill = extended_working_hrs))+
  geom_bar(stat="identity") + facet_grid(~Attrition) + geom_text(aes(label=countb),vjust = 2)

plot_extended_wrkhrs
rm(master_data_copy_1)

# Conclusion: There is a relatively higher amount of people working in extended working hours, in the group of 
# those who left the company

##################################################################################################################
# JobTenure: People who have tendency of changing jobs frequently are tend to leave the company within short
# periods. This can be found out from Jobtenure field.
##################################################################################################################
master_data$JobTenure <- ifelse(master_data$NumCompaniesWorked!=0,
                              round(master_data$TotalWorkingYears/master_data$NumCompaniesWorked,2),0)

plot_tenure <- ggplot(master_data,aes(JobTenure)) + geom_density() + facet_grid(~Attrition)
plot_tenure
rm(plot_tenure)

# Conclusion: This clearly shows the trend as the number of years per job is less for those who quit.

# Boxplots of numeric variables relative to Attrition 

plot_grid(ggplot(master_data, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(master_data, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(master_data, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

# Correlation between numeric variables

ggpairs(master_data[, c("MonthlyIncome", "PercentSalaryHike", "Age")])

##################################################################################################################
# Employee number/ID not required as it is not contributing towards attrition
##################################################################################################################

master_data <- master_data[, - 1]

str(master_data)

# converting target variable Attrition from No/Yes character to factorwith levels 0/1 

master_data$Attrition <- ifelse(master_data$Attrition =="Yes",1,0)

# converting extended_working_hrs from No/Yes character to factorwith levels 0/1 

master_data$extended_working_hrs <- ifelse(master_data$extended_working_hrs =="Yes",1,0)

##################################################################################################################
# converting categorical to factors
##################################################################################################################

# creating dataframe for categorical features and numerical features

master_data_chr<- master_data[,c(3:4, 6:11, 15, 22:27 )]
master_data_num <- master_data[,-c(3:4, 6:11, 15, 22:27 )]

# Standardising numerical data

str(master_data_num)
master_data_num$avg_working_hrs <- as.numeric(master_data_num$avg_working_hrs)
master_data_num_scale <- master_data_num [, c(1,3:13)]
master_data_num_scale <- data.frame(sapply(master_data_num_scale, function(x) scale(x)))
master_data_num       <-  data.frame(Attrition = master_data_num$Attrition)

str(master_data_chr)

# converting categorical attributes to factor

master_data_fact<- data.frame(sapply(master_data_chr, function(x) factor(x)))
str(master_data_fact)

# Making short of long variable names

levels(master_data_fact$BusinessTravel) <- c("NT", "TF", "TR")
levels(master_data_fact$Department)     <- c("HRD", "RNDD", "SD")
levels(master_data_fact$EducationField) <- c("HR", "LS", "MRK", "MED", "NA", "TD")
levels(master_data_fact$JobRole)        <- c("HC", "HR", "Lab", "Man", "MDir", "RsD", "RsSci", "SlEx", "SlRep")

# Checking for summary

sapply(master_data_fact, function(x) summary(x))

# creating dummy variables for factor attributes

dummies<- data.frame(sapply(master_data_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =master_data_fact))[,-1]))

# Confirming for 2 level categorical fields denotions
sum(dummies$Gender)       #Returns 2646 which is the number of males, so male are represented by 1 and females by 0
sum(dummies$PerformanceRating) #Returns 678 which is the number of records with rating 4, so for rating 3 it is 0
sum(dummies$extended_working_hrs) #Returns 1305 so extended working hours 1 otherwise 0

# Final dataset

base_data_final <- cbind(master_data_num, master_data_num_scale, dummies) #4410 rows with 58 variables

str(base_data_final)

# write master_data in .csv file

write.csv(master_data, "master_data.csv", row.names = FALSE)


# checking for correlation between Attrition and other variables

correlation_matrix <- cor(base_data_final)
corrplot(correlation_matrix, method = "number", title = "Correlation Map", mar=c(0,0,0,0),
         type = "lower", order = "FPC",col = c("red", "orange", "blue", "green"), number.cex = .5, tl.cex = .7)

# Conclusion from correlation matrix 
# Findings: Performance Rating & PercentageSalaryHike is highly correlated so does yearswithchurnmanager
# & yearsatcompany, Also totatl working years is directly correlated with age

################################## Data Modelling ####################################

# splitting the data between train and test

set.seed(100)

indices = sample.split(base_data_final$Attrition, SplitRatio = 0.7)

train = base_data_final[indices,]

test = base_data_final[!(indices),]

##################################################################################################################
# Logistic Regression: 
##################################################################################################################

# Initial model

model_1 <- glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 2093.7....58 coeff..nullDev 2670.8...resDev 1977.7

# Model created using STEPAIC function

model_2 <- stepAIC(model_1, direction="both")

summary(model_2)

sort((vif(model_2)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

# Removing EducationField.xNA +  as VIF and p value is high

model_3 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                Age + MonthlyIncome + TotalWorkingYears + BusinessTravel.xTF + 
                BusinessTravel.xTR + Education.x5 + EducationField.xLS + 
                EducationField.xMRK + EducationField.xMED +  
                EducationField.xTD + JobLevel.x2 + JobLevel.x5 + JobRole.xLab + 
                JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
              family = "binomial", data = train)

summary(model_3)

sort((vif(model_3)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

# Removing YearsAtCompany   as VIF and p value is high

model_4 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Age + MonthlyIncome + TotalWorkingYears + BusinessTravel.xTF + 
                BusinessTravel.xTR + Education.x5 + EducationField.xLS + 
                EducationField.xMRK + EducationField.xMED +  
                EducationField.xTD + JobLevel.x2 + JobLevel.x5 + JobRole.xLab + 
                JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
              family = "binomial", data = train)

summary(model_4)

sort((vif(model_4)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

# Removing BusinessTravel.xTR +   as VIF and p value is high

model_5 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Age + MonthlyIncome + TotalWorkingYears + BusinessTravel.xTF + 
                Education.x5 + EducationField.xLS + 
                EducationField.xMRK + EducationField.xMED +  
                EducationField.xTD + JobLevel.x2 + JobLevel.x5 + JobRole.xLab + 
                JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
              family = "binomial", data = train)

summary(model_5)

sort((vif(model_5)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing EducationField.xLS +    as VIF and p value is high

model_6 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Age + MonthlyIncome + TotalWorkingYears + BusinessTravel.xTF + 
                Education.x5 + 
                EducationField.xMRK + EducationField.xMED +  
                EducationField.xTD + JobLevel.x2 + JobLevel.x5 + JobRole.xLab + 
                JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
              family = "binomial", data = train)

summary(model_6)

sort((vif(model_6)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing WorkLifeBalance.x2 +  as VIF and p value is high

model_7 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Age + MonthlyIncome + TotalWorkingYears + BusinessTravel.xTF + 
                Education.x5 + 
                EducationField.xMRK + EducationField.xMED +  
                EducationField.xTD + JobLevel.x2 + JobLevel.x5 + JobRole.xLab + 
                JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
              family = "binomial", data = train)

summary(model_7)

sort((vif(model_7)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing EducationField.xMRK  as p value is high

model_8 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Age + MonthlyIncome + TotalWorkingYears + BusinessTravel.xTF + 
                Education.x5 + 
                EducationField.xMED +  
                EducationField.xTD + JobLevel.x2 + JobLevel.x5 + JobRole.xLab + 
                JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
              family = "binomial", data = train)

summary(model_8)

sort((vif(model_8)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing  EducationField.xMED +  as p value is high

model_9 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Age + MonthlyIncome + TotalWorkingYears + BusinessTravel.xTF + 
                Education.x5 + 
                EducationField.xTD + JobLevel.x2 + JobLevel.x5 + JobRole.xLab + 
                JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
              family = "binomial", data = train)

summary(model_9)

sort((vif(model_9)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing  EducationField.xTD +  as p value is high

model_10 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + MonthlyIncome + TotalWorkingYears + BusinessTravel.xTF + 
                 Education.x5 + 
                 JobLevel.x2 + JobLevel.x5 + JobRole.xLab + 
                 JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
               family = "binomial", data = train)

summary(model_10)

sort((vif(model_10)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing  StockOptionLevel.x3 + as p value is high

model_11 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + MonthlyIncome + TotalWorkingYears + BusinessTravel.xTF + 
                 Education.x5 + 
                 JobLevel.x2 + JobLevel.x5 + JobRole.xLab + 
                 JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
               family = "binomial", data = train)

summary(model_11)

sort((vif(model_11)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing   JobRole.xLab + as p value is high

model_12 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + MonthlyIncome + TotalWorkingYears + BusinessTravel.xTF + 
                 Education.x5 + 
                 JobLevel.x2 + JobLevel.x5 + 
                 JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
               family = "binomial", data = train)

summary(model_12)

sort((vif(model_12)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing    MonthlyIncome + as p value is high

model_13 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 Education.x5 + 
                 JobLevel.x2 + JobLevel.x5 + 
                 JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
               family = "binomial", data = train)

summary(model_13)

sort((vif(model_13)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing    JobLevel.x5 + as p value is high

model_14 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 Education.x5 + 
                 JobLevel.x2 +  
                 JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
               family = "binomial", data = train)

summary(model_14)

sort((vif(model_14)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing    MaritalStatus.xMarried +  as p value is high

model_15 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 Education.x5 + 
                 JobLevel.x2 +  
                 JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
               family = "binomial", data = train)

summary(model_15)

sort((vif(model_15)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing     JobRole.xRsSci +  as p value is high

model_16 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 Education.x5 + 
                 JobLevel.x2 +  
                 JobRole.xMDir + JobRole.xRsD + JobRole.xSlEx + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
               family = "binomial", data = train)

summary(model_16)

sort((vif(model_16)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing     StockOptionLevel.x1 + as p value is high

model_17 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 Education.x5 + 
                 JobLevel.x2 +  
                 JobRole.xMDir + JobRole.xRsD + JobRole.xSlEx + 
                 MaritalStatus.xSingle +  
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
               family = "binomial", data = train)

summary(model_17)

sort((vif(model_17)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing  JobRole.xRsD + as p value is high

model_18 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 Education.x5 + 
                 JobLevel.x2 +  
                 JobRole.xMDir +  JobRole.xSlEx + 
                 MaritalStatus.xSingle +  
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
               family = "binomial", data = train)

summary(model_18)

sort((vif(model_18)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing  JobInvolvement.x3 + as p value is high

model_19 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 Education.x5 + 
                 JobLevel.x2 +  
                 JobRole.xMDir +  JobRole.xSlEx + 
                 MaritalStatus.xSingle +  
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 +  extended_working_hrs, 
               family = "binomial", data = train)

summary(model_19)

sort((vif(model_19)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing  JobLevel.x2 +  as p value is high

model_20 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 Education.x5 + 
                 JobRole.xMDir +  JobRole.xSlEx + 
                 MaritalStatus.xSingle +  
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 +  extended_working_hrs, 
               family = "binomial", data = train)

summary(model_20)

sort((vif(model_20)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing   JobRole.xSlEx + as p value is high

model_21 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 Education.x5 + 
                 JobRole.xMDir +  
                 MaritalStatus.xSingle +  
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 +  extended_working_hrs, 
               family = "binomial", data = train)

summary(model_21)

sort((vif(model_21)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable


#Removing   Education.x5 + as p value is high

model_22 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 JobRole.xMDir +  
                 MaritalStatus.xSingle +  
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 +  extended_working_hrs, 
               family = "binomial", data = train)

summary(model_22)

sort((vif(model_22)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable


#Removing   TrainingTimesLastYear +  as p value is high

model_23 <-glm(formula = Attrition ~ NumCompaniesWorked + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 JobRole.xMDir +  
                 MaritalStatus.xSingle +  
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 +  extended_working_hrs, 
               family = "binomial", data = train)

summary(model_23)

sort((vif(model_23)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing WorkLifeBalance.x4 +  as p value is high

model_24 <-glm(formula = Attrition ~ NumCompaniesWorked + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 JobRole.xMDir +  
                 MaritalStatus.xSingle +  
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 extended_working_hrs, 
               family = "binomial", data = train)

summary(model_24)

sort((vif(model_24)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing JobSatisfaction.x3 +   as p value is high

model_25 <-glm(formula = Attrition ~ NumCompaniesWorked + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 JobRole.xMDir +  
                 MaritalStatus.xSingle +  
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 extended_working_hrs, 
               family = "binomial", data = train)

summary(model_25)

sort((vif(model_25)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing JobSatisfaction.x2 +  as p value is high

model_26 <-glm(formula = Attrition ~ NumCompaniesWorked + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 JobRole.xMDir +  
                 MaritalStatus.xSingle +  
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 +  
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 extended_working_hrs, 
               family = "binomial", data = train)

summary(model_26)

sort((vif(model_26)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing WorkLifeBalance.x3 +   as p value is high

model_27 <-glm(formula = Attrition ~ NumCompaniesWorked + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 JobRole.xMDir +  
                 MaritalStatus.xSingle +  
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 +  
                 JobSatisfaction.x4 +  
                  extended_working_hrs, 
               family = "binomial", data = train)

summary(model_27)

sort((vif(model_27)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

# With 13 significant variables in the model

final_model<- model_27
summary(final_model)

################################# Model Evaluation ###################################
#Test Data

# predicted probabilities of Attrition for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])

# Let's see the summary 

summary(test_pred)
test$prob <- test_pred
View(test)

# Let's use the probability cutoff of 50%.

test_pred_Attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_Attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))
table(test_actual_Attrition,test_pred_Attrition)


# Checking for other levels of cut off

# At 0.40 

test_pred_Attrition_1 <- factor(ifelse(test_pred >= 0.40, "Yes", "No")) 
table(test_pred_Attrition_1, test_actual_Attrition)


# At 0.30

test_pred_Attrition_2 <- factor(ifelse(test_pred >= 0.30, "Yes", "No")) 
table(test_pred_Attrition_2, test_actual_Attrition)


# At 0.25

test_pred_Attrition_3 <- factor(ifelse(test_pred >= 0.25, "Yes", "No")) 
table(test_pred_Attrition_3, test_actual_Attrition)


# install.packages("e1071")
library(e1071)

test_conf <- confusionMatrix(test_pred_churn, test_actual_churn)
test_conf

# Finding the optimal probalility cutoff value

perform_fn <- function(cutoff) 
{
  predicted_Attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_Attrition, test_actual_Attrition)
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability

summary(test_pred)
s = seq(.01,.80,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = c(perform_fn(s[i]))
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff #0.1536364

# Let's choose a cutoff value of 0.1536364 

test_cutoff_Attrition <- factor(ifelse(test_pred >=0.1536364, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_Attrition, test_actual_Attrition)

acc <- conf_final$overall[1]
acc

sens <- conf_final$byClass[1]
sens

spec <- conf_final$byClass[2]
spec

# Lift & Gain Chart 
# plotting the lift chart

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}
Attrition_decile = lift(test_actual_Attrition, test_pred, groups = 10)

# majority of resp is in top 4 deciles
#plot the lift chart 

plot(Attrition_decile$Cumlift, type="l", lwd=2, col="red",
     xlim = c(0,10),
     ylim = c(0,4),
     main = "Lift Chart",
     xlab = "Decile",
     ylab = "Lift")
abline(h=1, col="brown")
axis(1, 1:10)
abline(h=0:10, v=0:10, lty=3)

#Plot Gain Chart 

ks_plot(test_actual_Attrition, test_cutoff_Attrition) # Gain chart plot

# Correlation of final fields of the model

cor(base_data_final$Attrition,base_data_final$NumCompaniesWorked)           # 0.04200648
cor(base_data_final$Attrition,base_data_final$YearsSinceLastPromotion)      #-0.03301878
cor(base_data_final$Attrition,base_data_final$YearsWithCurrManager)         #-0.1563666
cor(base_data_final$Attrition,base_data_final$Age)                          #-0.15621
cor(base_data_final$Attrition,base_data_final$TotalWorkingYears)            #-0.1693355
cor(base_data_final$Attrition,base_data_final$BusinessTravel.xTF)           #0.1110897
cor(base_data_final$Attrition,base_data_final$JobRole.xMDir)                #-0.04374527
cor(base_data_final$Attrition,base_data_final$MaritalStatus.xSingle)        #0.1720183
cor(base_data_final$Attrition,base_data_final$EnvironmentSatisfaction.x2)   #-0.014466
cor(base_data_final$Attrition,base_data_final$EnvironmentSatisfaction.x3)   #-0.04393481
cor(base_data_final$Attrition,base_data_final$EnvironmentSatisfaction.x4)   #-0.04844704
cor(base_data_final$Attrition,base_data_final$JobSatisfaction.x4)           #-0.08616976
cor(base_data_final$Attrition,base_data_final$extended_working_hrs)         #.2295856

################################################################################################################
# Conclusion:
# If the number of companies worked by a person is more, they tend to quit more.
# If an employee works with the same manager for a longer period of time the lesser are the chances that 
# employee will leave the company.
# As the age increases the people are less tend to quit, which shows more comfortable with environment
# People with more experience as they are less likely to leave the company. 
# Environment Satisfaction, Job Satisfaction are some of the main features that need to be taken for retaining
# employees
# The more an employee works extended work hours on an average the more are the chances that he/she will leave
# the company.
# Employees who are unmarried are prone to leaving the company.
################################################################################################################