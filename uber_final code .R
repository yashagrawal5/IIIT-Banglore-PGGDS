#install necessary packages for using functions 
install.packages("stringr")

library(stringr)

install.packages("lubridate")

library(lubridate)

install.packages("xlsx")
#read csv file from specific location

uber_data <- read.csv("Uber Request Data.csv",header =TRUE,stringsAsFactors = FALSE)
View(uber_data)
str(uber_data)
#after viewing the structure it is understood that Request.timestamp and drop.timestamp is char


## Data cleaning is first the step
#for clening figure out duplicates values 
duplicate_values <- uber_data[duplicated(uber_data),]


#successfully run means no duplicates 
 #next  for data cleaning finding missing values 
na_values <- is.na(uber_data)
summary((uber_data))


#blank values 
length(which(uber_data$Request.id==""))
length(which(uber_data$Pickup.point==""))
length(which(uber_data$Driver.id==""))
length(which(uber_data$Status==""))
length(which(uber_data$Request.timestamp==""))
length(which(uber_data$Drop.timestamp==""))
# this all result to "0" it means no blank value is there

#hence this R is case sensitive so convert strings to lower format 
uber_data$Pickup.point <- tolower(uber_data$Pickup.point)
uber_data$Status <- tolower(uber_data$Status)
#view(uber_data)
#successfully completed strings to lower for proper implimentation

#for making  time format to same platform remove "/" to "-"
uber_data$Request.timestamp<- str_replace_all(uber_data$Request.timestamp,"/","-")
uber_data$Drop.timestamp <- str_replace_all(uber_data$Drop.timestamp,"/","-")

typeof(uber_data$Request.timestamp)
typeof(uber_data$Drop.timestamp)
#it give charcter
View(uber_data)
#convert to date format standard format
uber_data$Request.timestamp <- parse_date_time(uber_data$Request.timestamp,c("%d-%m-%Y %H:%M"))

View(uber_data)

 uber_data$request_hour <- format(uber_data$Request.timestamp,"%H")
View(uber_data)
#after this the view contain another column having names request_hour containg only hours 
uber_data$time_slots <- ifelse(uber_data$request_hour>="00" &  uber_data$request_hour <= "04","Early Morning",
                                ifelse(uber_data$request_hour>="05" & uber_data$request_hour <="09","Morning",
                                       ifelse (uber_data$request_hour>="10" & uber_data$request_hour<="12","Late Morning",
                                               ifelse(uber_data$request_hour>="13" & uber_data$request_hour <="16","Afternoon",
                                                      ifelse(uber_data$request_hour>="17" & uber_data$request_hour <=  "19","Evening","Night")))))
View(uber_data)

#till now there is one more column contain timeslot on the basis of requested_hour
#starting with visualisation using ggpot
install.packages("ggplot2")



library(ggplot2)

#bargraph to show number of trips completed during different time slots

trips_completed <- subset(uber_data,uber_data$Status=="trip completed")
Timeslot_bar <- ggplot(trips_completed,aes(x=time_slots))
plot_1 <- Timeslot_bar+geom_bar(stat="count",col="black",fill="green")+
  ggtitle("Trips completed during different Time Slots")+
  labs(x="time_slots",y="trip completed")+
  geom_text(stat='count',aes(label=..count..),vjust=-1)+
  guides(fill=FALSE)+
  scale_x_discrete(limits=c("Early Morning","Morning","Late Morning",
                            "Afternoon","Evening","Night"))









plot_1

#for problem 1 
#Plot1 to visualise the frequency of requests that get cancelled or show 'no cars available

plot1 <-ggplot(uber_data,aes(x=(factor(uber_data$Status,levels =names(sort(table(Status),decreasing = TRUE)))),fill=factor(uber_data$Pickup.point)))+geom_bar(position = "stack")+ggtitle("Frequency of status with pick point")+ labs(x="Status of cars",y="count") 

plot1
#for problem 2
#identify the most problematic types of requests (city to airport / airport to city etc.) and the time slots (early mornings, late evenings etc.) 
plot2 <-ggplot(uber_data,aes(x=factor(uber_data$time_slots,levels =names(sort(table(time_slots),decreasing = TRUE)))),fill=factor(uber_data$Status))+geom_bar(position = "stack")+ggtitle("Status of cab in different period of day")+labs(x="different time slots of day",y="count")
plot2
#####################################################################
#make data frames names uber_data_pick which is subset using airport 
#Get to know that there is no cars available in evening from airport and in night 


uber_data_pick <- subset(uber_data,uber_data$Pickup.point=="airport")
plot3 <- ggplot(uber_data_pick,aes(x=factor(uber_data_pick$time_slots,levels = (names(sort(table(time_slots),decreasing = TRUE)))),fill=factor(uber_data_pick$Status)))+geom_bar(position = "stack")+ggtitle("Graph showing status with different slots WRT airport")+labs(x="Time slots WRT to airport",y="count")
plot3


uber_data_citypick <- subset(uber_data,uber_data$Pickup.point=="city")
plot4 <- ggplot(uber_data_citypick,aes(x=factor(uber_data_citypick$time_slots,levels = (names(sort(table(time_slots),decreasing = TRUE)))),fill=factor(uber_data_citypick$Status)))+geom_bar(position = "stack")+ggtitle("Graph showing status with different slots WRT city")+labs(x="Time slots WRT city pickups",y="count")
plot4

#city_demand denotes that length of morning in city 
city_demand <- length(which(uber_data_citypick$time_slots=="Morning"))
city_demand


#gives 1677  in demand 
#city supply gives length of morning in city and trips which are completed 

city_supply<- length(which((uber_data_citypick$time_slots=="Morning")&(uber_data_citypick$Status=="trip completed")))


city_supply 
#gives 472 trips are completed remaining 1200 trips are cancelled or cars not availbale 


airport_demand <- length(which(uber_data_pick$time_slots=="Evening"))
airport_demand

#airport_demand gives 1079 trips in evening 
#airprt_supply gives around 238 completed 
airport_supply<- length(which((uber_data_pick$time_slots=="Evening")&(uber_data_pick$Status=="trip completed")))
airport_supply
