# Required Library
install.packages("stringr")
install.packages("DescTools")
install.packages("tidyverse")
install.packages("gridExtra")
install.packages("ggplot2")
install.packages("lubridate")
library(stringr)
library(DescTools)
library(tidyverse)
library(gridExtra)
library(ggplot2)
library(lubridate)

# Read the master Loan Dataset and store in loan dataframe.

loan <- read.csv("loan.csv", stringsAsFactors = FALSE,
                 na.strings = c(""," ", "N/A", "n/a"))

# checking first few records

head(loan)

# structure of data shows the data types

str(loan)

#------------------------------------Data cleaning------------------------------------------------#
#
# 1. Below listed coloumns can be eliminated as it contains only NAs and zeros. These values will 
#    not add sightfull information for the analysis.
#-------------------------------------------------------------------------------------------------#

loan$acc_now_delinq <- NULL                 # - contains 0 only
loan$acc_open_past_24mths <-  NULL          # - contains NA only
loan$all_util <- NULL                      #  - contains NA only
loan$annual_inc_joint <- NULL              #  - contains NA only
loan$application_type <- NULL              #  - contains only one type individual
loan$avg_cur_ba <- NULL                    #  - contains NA only
loan$bc_open_to_buy <- NULL                #  - contains NA only
loan$bc_util <- NULL                       #  - contains NA only
loan$chargeoff_within_12_mths <- NULL      #  - contains 0 and blanks
loan$collections_12_mths_ex_med <- NULL    #  - contains 0 only
loan$delinq_amnt <- NULL                   #  - contains 0 only
loan$dti_joint <- NULL                     #  - contains NA only
loan$il_util <- NULL                       #  - contains NA only
loan$inq_fi <- NULL                        #  - contains NA only
loan$inq_last_12m <- NULL                  # - contains NA only
loan$max_bal_bc <- NULL                    # - contains NA only
loan$mo_sin_old_il_acct <- NULL            # - contains NA only
loan$mo_sin_old_rev_tl_op <- NULL          #  - conatins NA only
loan$mo_sin_rcnt_rev_tl_op <- NULL         # - contains NA only
loan$mo_sin_rcnt_tl <- NULL                # - contains NA only
loan$mort_acc <- NULL                      # - contains NA only
loan$mths_since_last_major_derog <- NULL   # - contains NA only
loan$mths_since_rcnt_il <- NULL            # - contains NA only
loan$mths_since_recent_bc <- NULL          # - contains NA only
loan$mths_since_recent_bc_dlq <- NULL      # - contains NA only
loan$mths_since_recent_inq <- NULL         # - contains NA only
loan$mths_since_recent_revol_delinq <- NULL # - contains NA only
loan$num_accts_ever_120_pd <- NULL         #  - contains NA only
loan$num_actv_bc_tl <- NULL                #  - contains NA only
loan$num_actv_rev_tl <- NULL               #  - contains NA only
loan$num_bc_sats <- NULL                   # - contains NA only
loan$num_bc_tl <- NULL                     # - contains NA only
loan$num_il_tl <- NULL                     # - contains NA only
loan$num_op_rev_tl <- NULL                 # - contains NA only 
loan$num_rev_accts <- NULL                 # - contains NA only
loan$num_rev_tl_bal_gt_0 <- NULL           #  - contains NA only
loan$num_sats <- NULL                      #  - contains NA only
loan$num_tl_120dpd_2m <- NULL              # - contains NA only
loan$num_tl_30dpd <- NULL                  # - contains NA only
loan$num_tl_90g_dpd_24m <- NULL            #  - contains NA only
loan$num_tl_op_past_12m <- NULL            #  - contains NA only
loan$open_acc_6m <- NULL                   # - contains Na only
loan$open_il_12m <- NULL                   # - contains Na only
loan$open_il_24m <- NULL                   #  - contains NA only
loan$open_il_6m <- NULL                    #  - contains NA only
loan$open_rv_12m <- NULL                   #  - contains NA only
loan$open_rv_24m <- NULL                   #  - contains NA only
loan$out_prncp <- NULL                     #  - contains 0 only
loan$out_prncp_inv <- NULL                 #  - contains 0 only
loan$pct_tl_nvr_dlq <- NULL                #  - contains NA only
loan$percent_bc_gt_75 <- NULL              #  - contains NA only
loan$pymnt_plan <- NULL                    # - contains n only
loan$tot_coll_amt <- NULL                  #  - contains Na only
loan$tot_cur_bal <- NULL                   #  - contains NA only
loan$tot_hi_cred_lim <- NULL               # - contains NA only
loan$total_bal_ex_mort <- NULL             #  - contains NA only
loan$total_bal_il <- NULL                  #  - contains NA only
loan$total_bc_limit <- NULL                #  - contains Na only
loan$total_cu_tl <- NULL                   #  - contains NA only
loan$total_il_high_credit_limit <- NULL    #  - contains NA only
loan$total_rev_hi_lim <- NULL              #  - contains Na only
loan$tax_liens <- NULL                     # - contain 0 only
loan$verification_status_joint <- NULL     # contains NA only
loan$avg_cur_bal<- NULL                    # contains only NA

# 2. "url" can be eliminated as it has been orginated from single website and require additiona
#    credentials.

loan$url <- NULL                           #  - no specific detail

# 3. "desc' can be eliminated as each description have their own requirement provied by borrower.

loan$desc <- NULL                          #  - no use

# 4. "initial_list_status" and 'policy_code" can be eliminated as they contain identical values in
#   their respective coloumns.

loan$initial_list_status <- NULL           # - contains f only
loan$policy_code <- NULL                   #  - contains only 1

# 5. "title" can be eliminated

loan$title <- NULL                         #  - not relevant


# 6. "next_pymnt_d" can be eliminated as it missing for approx. 97% records.

loan$next_pymnt_d <- NULL                  # - most of the data is missing

# 7. loan_status = "CURRENT" can be eliminated as it is ongoing and does not lead to fixed status.

loan<-loan[-which(toupper(loan$loan_status) == "CURRENT"), ]


# 8. id, member_id are unique and analysis is based on group analysis rather that member level
#     level analysis. So, these can be eliminated

loan$id <- NULL
loan$member_id <- NULL

# 9. remove % symbol from interest rate and revol_util

loan$int_rate <- gsub("\\%", "", loan$int_rate)
loan$int_rate <- as.factor(round(as.numeric(loan$int_rate)))
loan$revol_util <- gsub("\\%", "", loan$revol_util)

# 10. Treating invalid values, any variable having more than 15% of data points
# missing is not eligible for imputation hence it makes sense to compute
# and drop those variables

missing_values <- loan %>%
  summarise_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values,key='feature',value = 'missing_percentage')

missing_values %>%
  ggplot(aes(x=reorder(feature,-missing_percentage),y=missing_percentage)) +
  geom_bar(stat = 'identity',fill='red') +
  coord_flip()

# 11. There are some columns with redundant values need to be removed
# finding out relevant/good columns where the missing % < 15%

good_columns <- filter(missing_values,missing_percentage<0.15)

good_columns <- good_columns$feature 

# 12. Removing all the column which have redundant information. 

loan <- loan[,(colnames(loan) %in% good_columns)]

# 13. Let's summarise the data

summary(loan)

# 14. "pub_rec_bankruptcies" has large number of zero observation. Remove "pub_rec_bankruptcies"

loan$pub_rec_bankruptcies <- NULL

# 15. count of NA values by column: just to verify if the NA values cleared

loan %>% summarise_all(funs(sum(is.na(.))))  # 0

sum(is.na(loan)) # 0 

# 16. Format Income, loan amount and dti as bin

loan$income_bin <- round(loan$annual_inc / 10000, digits = 0) * 10000
loan$income_bin <- as.factor(loan$income_bin)

loan$loan_amnt_bin<-round(loan$loan_amnt/1000,digits = 0)*1000

loan$dti_bin<-round(loan$dti,digits = 0)

# 17. Date conversions

loan$issue_d <- as.Date(loan$issue_d,"%b-%d")

loan$last_pymnt_d <- as.Date(loan$last_pymnt_d, "%b-%d")

# 18. split date column - Issue_dyear (Year) Issue_dmonth (Month)

issue_dyear <- data.frame(issue_dyear = year(loan$issue_d))
loan <- cbind(loan, issue_dyear)

issue_dmonth <- data.frame(issue_dmonth = month(loan$issue_d))
loan <- cbind(loan, issue_dmonth)

loan$issue_dyear <- as.factor(loan$issue_dyear)
loan$issue_dmonth <- as.factor(loan$issue_dmonth)

#---------------------------------------------Data cleaning finished----------------------------------------------#
#
#-----------------------------------------------------------------------------------------------------------------#
#                                            Data Analysis                                                        #
#-----------------------------------------------------------------------------------------------------------------#
# 1. Check and remove outliers of Annual income column 

boxplot(loan$annual_inc)

quantile(loan$annual_inc,c(1:100)/100)

loan<-subset(loan,loan$annual_inc<=89000)

boxplot(loan$annual_inc)

# 2. Box Plot - Annual income vs Loan Status

ggplot(loan, aes(loan$loan_status, loan$annual_inc)) + geom_boxplot() + ylim(0,150000) +
  labs(title = "Annual income vs Loan Status",
       x = "Loan Status",
       y = "Annual income")

# 3. Bar Chart - Verification vs Status

ggplot(loan, aes(x=factor(loan$verification_status), fill = loan$loan_status)) +
  geom_bar(position = "dodge") + 
  labs(title = "Verification vs Status",
       x = "Verification Status",
       y = "Count")


# 4. Bar Chart - Grade vs Status

ggplot(loan, aes(x=factor(loan$grade), fill = loan$loan_status)) + 
  geom_bar(position = "dodge") +
  labs(title = "Grade vs Status",
       x = "Grade",
       y = "Count")


# 5. Address state

ggplot(data = loan, aes(reorder(addr_state, -table(addr_state)[addr_state]))) + 
  geom_bar(stat="count") + scale_y_continuous(trans='log2') + 
  geom_text(stat='count',aes(label=..count..),vjust=-0.5)+ 
  xlab("State as per the Address") + ylab("Count of Applicants in each State") + 
  ggtitle("Applicants on the basis of States ")

# 6. Zip_code

ggplot(data = loan, aes(reorder(zip_code, zip_code)))  +
  geom_point(stat="count") + xlab("Zip Codes") + ylab("Count of Applicants in each zip code") +
  ggtitle("Applicants Across the Zip Codes ")

# 7. Check Verification status

ggplot(data = loan, aes(reorder(verification_status, -table(verification_status)[verification_status]))) +
  geom_bar(stat="count") + geom_text(stat='count',aes(label=..count..),
                                     vjust=-0.5)+xlab("Verification Status of Applicant ")+ 
  ylab("Applicants Count")+ ggtitle("Verification Status of Applicant's Income")

# 8. Grade & Sub-Grade

p1 <- ggplot(data = loan, aes(grade)) + geom_bar(stat="count")+ geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.9), vjust=-0.25) + xlab("Grade") + ylab("Count of Applicants") + ggtitle("No of Applicants in each Grade")
p2 <- ggplot(data = loan, aes(sub_grade)) + geom_bar(stat="count") +geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.9), vjust=-0.25)+ xlab("Sub Grade") + ylab("Count of Applicants") + ggtitle("No of Applicants in each Sub Grade")
grid.arrange(p1, p2, ncol=2)
rm(p1)
rm(p2)

# 9. Funded Amount & Funded Amount invested 

nrow(loan[loan$funded_amnt == loan$funded_amnt_inv, ])
boxplot(loan$funded_amnt)
summary(loan$funded_amnt)
boxplot(loan$funded_amnt_inv)
summary(loan$funded_amnt_inv)

#----------------------------------------------------Univariate Analysis------------------------------------------#

# Plot to show loan status based on Annual Income 

p1 <-ggplot(subset(loan, loan$loan_status != "Charged Off"), aes(income_bin)) + 
  geom_bar(fill='red') + ylim(0,6000) + ggtitle('Annual Income (Loan Paid off)')

p2 <- ggplot(subset(loan, loan$loan_status == "Charged Off"), aes(income_bin)) + 
  geom_bar(fill='blue') + ylim(0,6000) + ggtitle('Annual Income (Loan Defaulted)')

grid.arrange(p1, p2, ncol=2)
rm(p1)
rm(p2)

# Plot to show loan status based on term (duration of loan)

p1 <- ggplot(subset(loan, loan$loan_status != "Charged Off"), aes(term)) +
  geom_bar(fill='red') + ylim(0,25000) + ggtitle('Term (Loan Paid off)')

p2 <- ggplot(subset(loan,loan$loan_status == "Charged Off"), aes(term)) +
  geom_bar(fill='blue') + ylim(0,25000) + ggtitle('Term (Loan Defaulted)')

grid.arrange(p1, p2, ncol=2)
rm(p1)
rm(p2)

# Plot to show Loan Amount which are most paid and most defaulted

p1 <-ggplot(subset(loan, loan$loan_status != "Charged Off"), aes(loan_amnt_bin)) +
  geom_bar(fill ='red') + ylim(0,3000) + ggtitle('Loan Amount (Loan Paid off)')

p2 <- ggplot(subset(loan,loan$loan_status == "Charged Off"), aes(loan_amnt_bin)) +
  geom_bar(fill = 'blue') + ylim(0,3000) + ggtitle('Loan Amount (Loan Defaulted)')

grid.arrange(p1, p2, ncol=2)
rm(p1)
rm(p2)

# Plot to show paid and defaulted loans as per grade

p1 <-ggplot(subset(loan, loan$loan_status != "Charged Off"), aes(grade)) +
  geom_bar(fill = 'red') + ylim(0,9000) + ggtitle('Grade (Loan Paid off)')

p2 <- ggplot(subset(loan, loan$loan_status == "Charged Off"), aes(grade)) +
  geom_bar(fill = 'blue') + ylim(0,9000) + ggtitle('Grade (Loan Defaulted)')

grid.arrange(p1, p2, ncol=2)
rm(p1)
rm(p2)

# Emp_length

p1 <- ggplot(subset(loan, loan$loan_status != "Charged Off"), aes(emp_length)) +
  geom_bar(fill = 'red') + ylim(0,5000) + ggtitle('Employee Length (Loan Paid off)')

p2<- ggplot(subset(loan, loan$loan_status == "Charged Off"), aes(emp_length))+
  geom_bar(fill = 'Blue') + ylim(0,5000) + ggtitle('Employee Length (Loan Defaulted)')

grid.arrange(p1, p2, ncol=2)
rm(p1)
rm(p2)

# Plot to show status of loan based on home ownership

p1 <- ggplot(subset(loan, loan$loan_status != "Charged Off"), aes(home_ownership)) +
  geom_bar(fill='red') + ylim(0,15000) + ggtitle('Home Ownership (Loan Paid off)')

p2 <- ggplot(subset(loan, loan$loan_status == "Charged Off"), aes(home_ownership)) +
  geom_bar(fill = 'blue') + ylim(0,15000) + ggtitle('Home Ownership (Loan Defaulted)')

grid.arrange(p1, p2, ncol=2)
rm(p1)
rm(p2)

# Plot to show loan status based on purpose of loan

p1 <- ggplot(subset(loan, loan$loan_status != "Charged Off"), aes(purpose)) +
  geom_bar(fill = 'red') + ylim(0,12500) + ggtitle('Purpose (Loan Paid off)')

p2 <- ggplot(subset(loan, loan$loan_status == "Charged Off"), aes(purpose)) +
  geom_bar(fill = 'blue') + ylim(0,12500) + ggtitle('Purpose (Loan Defaulted)')

grid.arrange(p1, p2, ncol=2)
rm(p1)
rm(p2)

# Plot to show status of laon based on Delinq_2yr

p1 <- ggplot(subset(loan, loan$loan_status != "Charged Off"), aes(delinq_2yrs)) +
  geom_bar(fill = 'red') + ylim(0,25000) + ggtitle('Delinq_2yrs (Loan Paid off)')

p2 <- ggplot(subset(loan, loan$loan_status == "Charged Off"), aes(delinq_2yrs)) +
  geom_bar(fill = 'Blue') + ylim(0,25000) + ggtitle('Delinq_2yrs (Loan Defaulted)')

grid.arrange(p1, p2, ncol=2)
rm(p1)
rm(p2)

# Plot to show loan status based on Public Record

p1 <- ggplot(subset(loan, loan$loan_status != "Charged Off"), aes(pub_rec)) +
  geom_bar(fill = 'red') + ylim(0,25000) + ggtitle('UA of Public Record (Loan Paid off)')

p2 <- ggplot(subset(loan, loan$loan_status == "Charged Off"), aes(pub_rec)) +
  geom_bar(fill = 'Blue') + ylim(0,25000) + ggtitle('Public Record (Loan Defaulted)')

grid.arrange(p1, p2, ncol=2)
rm(p1)
rm(p2)

# Plot to show loan status based on Verification Status

p1 <- ggplot(subset(loan, loan$loan_status != "Charged Off"), aes(verification_status)) +
  geom_bar(fill = 'red') + ylim(0,13000) + ggtitle('Verification Status (Loan Paid off)')

p2 <- ggplot(subset(loan, loan$loan_status == "Charged Off"), aes(verification_status)) +
  geom_bar(fill = 'Blue') + ylim(0,13000) + ggtitle('Verification Status (Loan Defaulted)')

grid.arrange(p1, p2, ncol=2)
rm(p1)
rm(p2)

# Plot to show loan status based on DTI

p1 <- ggplot(subset(loan, loan$loan_status != "Charged Off"), aes(dti_bin)) +
  geom_bar(fill = 'red') + ylim(0,1500) + ggtitle('DTI (Loan Paid off)')

p2 <- ggplot(subset(loan, loan$loan_status == "Charged Off"), aes(dti_bin)) +
  geom_bar(fill = 'blue') + ylim(0,1500) + ggtitle('DTI (Loan Defaulted)')

grid.arrange(p1, p2, ncol=2)
rm(p1)
rm(p2)

# plot to show loan status based on Open_acc

p1 <- ggplot(subset(loan, loan$loan_status != "Charged Off"), aes(open_acc)) +
  geom_bar(fill = 'red') + ylim(0,3000) + ggtitle('Open_acc (Loan Paid off)')

p2 <- ggplot(subset(loan, loan$loan_status == "Charged Off"), aes(open_acc)) +
  geom_bar(fill = 'blue') + ylim(0,3000) + ggtitle('Open_acc (Loan Defaulted)')

grid.arrange(p1, p2, ncol=2)
rm(p1)
rm(p2)

# Plot to show loan status based on Total_acc

p1 <- ggplot(subset(loan, loan$loan_status != "Charged Off"), aes(total_acc)) +
  geom_bar(fill = 'red') + ylim(0,1500) + ggtitle('Total_acc (Loan Paid off)')

p2 <- ggplot(subset(loan, loan$loan_status == "Charged Off"), aes(total_acc)) +
  geom_bar(fill = 'blue') + ylim(0,1500) + ggtitle('Total_acc (Loan Defaulted)')

grid.arrange(p1, p2, ncol=2)
rm(p1)
rm(p2)

# No of inquiries in last 6 months with loan status

ggplot(loan, aes(x=loan$inq_last_6mths, fill = factor(loan$loan_status))) +
  geom_bar(stat = "count") + scale_y_continuous(trans='log2') +
  geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.9), vjust=-0.25) +
  xlab("No. of Inquiries") + ylab("Count") + ggtitle("No. of Enquiries VS status") 


#----------------------------------------Bivariate Analysis-------------------------------------------------------#
# Plot to show Loan Issued per Month along with loan status

ggplot(loan, aes(loan$issue_dmonth, fill = factor(loan$loan_status))) + 
  geom_histogram(stat="count", bandwidth = 10) + 
  labs(title = "Issue MONTH and frequency",
       x = "Issued per Month",
       y = "Count")

# Grade Distribution among the data set( Frequency and Percent)

Desc(loan$grade, main="Grade Distribution", plotit = 1)

# Loan Term distribution among the data set

Desc(loan$term, main="Loan Term Distribution", plotit = 1)

# Loan Amount distribution among the data set

Desc(loan$loan_amnt, main="Loan Amount Distribution", plotit = 1)

# Loan Amount By Term among the data set

ggplot(loan, aes(loan$term,loan$loan_amnt)) + geom_boxplot(aes(fill = loan$term)) +
  labs(title = "Loan amount by term",
       x = "Term",
       y = "Loan amount")

# Lending amount per month

lending_amt = loan %>% 
  select(issue_dmonth, loan_amnt) %>%
  group_by(issue_dmonth) %>%
  summarise(Amount = sum(loan_amnt))

summary(lending_amt)
lending_amt

ggplot(lending_amt, aes(x = issue_dmonth, y = Amount)) + geom_point() + 
  labs(title = "Loan amount issued by month",
       x = "Issued Month",
       y= "Total Amount")
rm(lending_amt)

# Loan Amount status distribution

Desc(loan$loan_status, main = "Loan amount status distribution", plotit = 1)

# Loan Status by Grade 

ggplot(loan, aes(loan$grade, fill = factor(loan$loan_status))) + geom_bar(position = "fill") + 
  labs(title = "Loan status by grade",
       x = "Grade",
       y = "Rate")

# Loan Status by Grade - Stacked Representation

ggplot(loan, aes(loan$grade, fill = factor(loan$loan_status))) + geom_bar(position = "stack") + 
  labs(title = "Loan status by grade",
       x = "Grade",
       y = "Rate")


# Loan disbursement growth rate

amt_group_table = loan %>% 
  select(issue_dmonth, loan_status, loan_amnt) %>% 
  group_by(issue_dmonth, loan_status) %>% 
  summarise(Amount = sum(loan_amnt))

summary(amt_group_table)

ggplot(amt_group_table, aes(x = issue_dmonth, y = Amount, col = factor(loan_status))) + geom_point() + 
  labs(title = "Loan amount distribution among loan statuses",
       x = "Issued date",
       y = "Amount")
rm(amt_group_table)

# Annual Income distribution of borrowers

Desc(loan$annual_inc, main = "Annual Income distribution", plotit = 1)

# Annual Income by Grade for each loan status

ggplot(loan, aes(grade,annual_inc, fill = factor(loan$loan_status))) + geom_boxplot() + ylim(0,100000) +
  labs(title = "Annual Income by Grade",
       x = "Grade",
       y = "Annual income")

# Purpose distribution of the Loan Amount

Desc(loan$purpose, main = "Purpose Distribution of Loan Amount", plotit = 1)

# Loan disbursement Vs Loan Status and Grade

Loand_group_table = loan %>% 
  select(loan_status, loan_amnt, grade) %>% 
  group_by(loan_status, grade) %>% 
  summarise(Amount = sum(loan_amnt))

summary(Loand_group_table)

ggplot(Loand_group_table, aes(x = grade,y = Amount, fill = loan_status)) + 
  geom_bar(stat="identity",position = "dodge") + geom_text(aes(label = Amount), position= position_dodge(width=0.9), vjust=-.5, color="black") +
  theme(legend.position = "bottom") +
  labs(title = "Loan amount distribution Vs Loan Status per Grades",
       x = "Grades",
       y = "Amount")
rm(Loand_group_table)

# Loan disbursement Vs Verification Status and Loan Status

Loand_group_table = loan %>% 
  select(loan_status, loan_amnt, verification_status) %>% 
  group_by(loan_status, verification_status) %>% 
  summarise(Amount = sum(loan_amnt))

summary(Loand_group_table)

ggplot(Loand_group_table, aes(x = verification_status,y = Amount, fill = loan_status)) + 
  geom_bar(stat="identity",position = "dodge") + geom_text(aes(label = Amount), position= position_dodge(width=0.9), vjust=-.5, color="black") +
  theme(legend.position = "bottom") +
  labs(title = "Loan amount distribution Vs Loan Status per Grades",
       x = "Grades",
       y = "Amount")
rm(Loand_group_table)

# Analysis based on home ownership

ggplot(loan, aes(x= loan_status,  group=home_ownership)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Loan Status") +
  facet_grid(~home_ownership) +
  scale_y_continuous(labels = scales::percent)

# Analysis based on loan purpose
# 1.
ggplot(loan, aes(x= loan_status,  group=purpose)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Status") +
  facet_grid(~purpose) +
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels=abbreviate)

# 2.

ggplot(loan, aes(x = loan_amnt, y = purpose, col = loan_status )) + 
  geom_point( alpha = 0.2 ) + geom_jitter()


# Analysis based on grade

ggplot(loan[loan$emp_length != "N/A",], aes(x= loan_status,  group=grade)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Status") +
  facet_grid(~grade) +
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels=abbreviate)

#--------------------------Writing file for Tableau representation-------------------------------------------------#
#
write.csv(loan,file = "analysed_loan_data.csv", na = "")
#
#-----------------------------------------------------End Of Analysis----------------------------------------------#

