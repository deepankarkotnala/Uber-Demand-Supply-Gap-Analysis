##########################################################################################
##                                                                                      ##
##   Project     : Uber Demand Supply Case Study                                        ##
##                                                                                      ##
##   Description : Analyse Uber Cab's data and suggest ways in which the demand-supply  ##
##                 gap can be reduced and better availability of cabs can be provided   ##
##                 to the customers.                                                    ##
##                                                                                      ##
##   Date        : 09-Aug-2018                                                          ##
##                                                                                      ##
##   Author      : Deepankar Kotnala                                                    ##
##                                                                                      ##
##########################################################################################


##########################################################################################
##                                                                                      ##
##                      Installing and Loading required libraries                       ##
##                                                                                      ##
##########################################################################################


# Clearing the previously loaded objects. 
remove(list = ls())
# Suppressing warnings
options(warn = -1)

# Install the required packages and load libraries:

# Uncomment these three lines if the packages are not installed.
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("lubridate")
library(dplyr)
library(ggplot2)
library("lubridate")

##########################################################################################
##                                                                                      ##
##                          Importing and Exploring the Data                            ##
##                                                                                      ##
##########################################################################################

# Import the data from input file
uber_data <- read.csv("Uber Request Data.csv",stringsAsFactors = F)

# Get a summary of the data
str(uber_data)
summary(uber_data)
na_count <- as.data.frame(sapply(uber_data, function(x) length(which(is.na(x)))))
colnames(na_count)[1] <- "Count"
na_count
# Request.id, Pickup.point , Status, Request.timestamp have no NA values.
# Driver.id & Drop.timestamp columns have 2650 and 3914 NA values respectively. 
 
table(uber_data$Status)
# This gives us the count in each of the three cases
# Cancelled         : 1264
# No cars available : 2650
# Trip Completed    : 2831

##########################################################################################
##                                                                                      ##
##                                  Cleaning the data                                   ##
##                                                                                      ##
##########################################################################################

uber_data$Request.timestamp <- parse_date_time(uber_data$Request.timestamp,
                                               c('dmy_HMS','dmy_HM'))
uber_data$Drop.timestamp    <- parse_date_time(uber_data$Drop.timestamp,
                                               c('dmy_HMS','dmy_HM'))

# Check whether the request time is not after drop time
sum(uber_data$Request.timestamp > uber_data$Drop.timestamp, na.rm=T)
# Returns 0. So the time part is correct.

##########################################################################################
##                                                                                      ##
##                                  Derived columns                                     ##
## Extracting meaningful information from Request.Timestamp and Drop.Timestamp columns  ##
##                                                                                      ##
##########################################################################################

uber_data$Req.day       <- weekdays(uber_data$Request.timestamp)
uber_data$Drop.day      <- weekdays(uber_data$Drop.timestamp)
uber_data$Date          <- qday(uber_data$Request.timestamp)
uber_data$Req.hour      <- hour(uber_data$Request.timestamp)
uber_data$Drop.hour     <- hour(uber_data$Drop.timestamp)
uber_data$Trip.duration <- round(difftime(uber_data$Drop.timestamp,
                                          uber_data$Request.timestamp, 
                                          units = "mins"))

#Req.hour in 24 hrs format
uber_data$Req.hour24 <- paste(uber_data$Req.hour,":00",sep="")


##########################################################################################
##                                                                                      ##
##                                 Analysing the data                                   ##
##                                                                                      ##
##########################################################################################

# Let's start analysing the data

##--------------------------------------------------------------------------------------##
## Number of drivers who travel from Airport to City or vice-Versa.                     ##
##--------------------------------------------------------------------------------------##

length(unique(uber_data$Driver.id))

# There are 301 Drivers
# This means that there are 301 cabs running between Airport and the City


##--------------------------------------------------------------------------------------##
## Number of Pickup Requests from Airport to city and from City to Airport              ##
##--------------------------------------------------------------------------------------##

req <- table(uber_data$Pickup.point)
# From Airport to City : 3238 Requests
# From City to Airport : 3507 Requests


airport_req_per  <- round(req[1]/(req[1] + req[2]) * 100)
airport_req_per
city_request_per <- round(req[2]/(req[1]+ req[2]) * 100)
city_request_per
# From Airport: 48% Pickup Requests are raised
# From City   : 52% Pickup Requests are raised
# There is a high demand of cabs from City to Airport as compared to 
# the demand of pickups from Airport to City.


# Let's plot the Demand Comparison chart from Airport and City
ggplot(uber_data,aes(x=uber_data$Pickup.point, fill=uber_data$Pickup.point)) +
      geom_bar(aes()) + ggtitle("Plot of Demand in Airport vs City") + 
      xlab("Pickup Point") + 
      ylab("Number of Pickup Requests") + 
        guides(fill=guide_legend(title="Pickup Point")) +
        geom_text(stat='count', aes(label=..count..), vjust=-1) +
        theme_light() +
        theme(axis.text = element_text(size =12)) + theme(
          plot.title = element_text(size=14, face="bold"),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14)
        )


##---------------------------------------------------------------------------------------##
##                               Plot the status of trips                                ##
##---------------------------------------------------------------------------------------##

table(uber_data$Status)
#Cancelled        : 1264
#No Cars Available: 2650
#Trip Completed   : 2831

percent(prop.table(table(uber_data$Status)))
#Cancelled        : 19%
#No Cars Available: 39%
#Trip Completed   : 42%

ggplot(uber_data,aes(x=uber_data$Status, fill=uber_data$Status)) +geom_bar(width=0.5) + 
  ggtitle("Plot of Status of Trips") + geom_bar(width=0.6) + xlab("Status") + 
  ylab("Count in each category") + 
  guides(fill=guide_legend(title="Pickup Point")) + 
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  theme_light() +
  theme(axis.text = element_text(size =12)) + theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14)
  )

# Around 19% of the trips are cancelled. This leads to a loss for the business.
# Cancellation of cabs should be reduced to enhance business profitability and reduce losses.

# Around 39% times, there is unavailability of Cabs.
# This means that there is a gap of demand-supply by 39% here.
# So if cabs were available at this time, then Uber would have earned that profit.


##--------------------------------------------------------------------------------------##
##                               Dividing each day into Slots                           ##
##                 (Early Morning, Morning, Afternoon, Evening, and Night)              ##
##--------------------------------------------------------------------------------------##


uber_data$Req.hour <- as.numeric(uber_data$Req.hour)
  str(uber_data)
# Now we will divide the day into 5 slots based on the findings from previous step: 

uber_data$Time.slot <- sapply(uber_data$Req.hour, function(x){
  if (x %in% c(0,1,2,3,4))
    return("Early Morning")
  else if (x %in% c(5,6,7,8,9))
    return("Morning")
  else if (x %in% c(10,11,12,13,14,15,16))
    return("Day Time")
  else if (x %in% c(17,18,19,20,21))
    return("Evening")
  else if ((x %in% c(22,23)))
    return("Night")
})


# These slots are taken after considering the high demand of cabs during Morning and Night time.

# Converting the slots into factors for plotting a frequency graph.
uber_data$Time.slot <- factor(uber_data$Time.slot,
                              levels =c("Early Morning","Morning","Day Time","Evening","Night"))

table(uber_data$Time.slot)
# Early Morning : 578
# Morning       : 2103
# Day Time      : 1224
# Evening       : 2342
# Night         : 498

# We can clearly see that there is a very high demand of cabs in the Morning and at the Evening slots.

# Lets plot a bar graph to visualize it.
# Plotting Time-Slot wise frequency of Demands.

##--------------------------------------------------------------------------------------##
##                      Hourly demand of Cabs (Both at airport and City)                ##
##--------------------------------------------------------------------------------------##

ggplot(uber_data,aes(uber_data$Req.hour, fill=uber_data$Status)) + 
  geom_bar() +
  xlab("Hour of the Day") + 
  ylab("Number of Requests") + 
  ggtitle("Plot of Hourly demand of Cabs - Overall") +
  guides(fill=guide_legend(title="Status of Requests")) +
  theme_minimal() +
  theme(axis.text = element_text(size =10)) + theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14)
  )

# We can see that there is a high demand of Cabs from 5:00 hrs to 9:59 hrs 
# and from 17:00 hrs to 22:59 hrs
# the demand is way higher that the number of cabs available (i.e. 301 cabs)


ggplot(uber_data, aes(x=(uber_data$Req.hour), fill=uber_data$Time.slot)) + 
  geom_bar(width=0.6) + 
  ggtitle("Plot of Demand in Each Time Slot") + xlab("Time Slot") + 
  ylab("Number of Requests") + 
  guides(fill=guide_legend(title="Time Slot")) + 
  theme_light() +
  theme(axis.text = element_text(size =12)) + theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14)
  )

# From this chart, we can see that there is huge demand during Morning(5:00 hrs - 9:59 hrs)
# and Evening(17:00 hrs - 21:59 hrs) slots.



##--------------------------------------------------------------------------------------------------------------##
##                          Day-wise demand of cabs at the Airport                                              ##
##--------------------------------------------------------------------------------------------------------------##

# Getting data for pickups at airport
uber_airport <- uber_data[which(uber_data$Pickup.point =="Airport"),]

uber_airport$Req.day <- factor(uber_airport$Req.day,
                              levels =c("Monday","Tuesday","Wednesday","Thursday","Friday"))

ggplot(uber_airport, aes(x=(uber_airport$Req.day), fill=uber_airport$Req.day)) + 
  geom_bar(width=0.6) +
  ggtitle("Plot of Demand on each Day of the Week - Airport") + xlab("Weekday") + 
  ylab("Number of Requests") + 
  guides(fill=guide_legend(title="WeekDay")) + 
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  theme_light() +
  theme(axis.text = element_text(size =12)) + theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14)
  )

# The demand stays nearly constant at around 600+ cabs daily 
# throughout the week(Monday - Friday)


##--------------------------------------------------------------------------------------------------------------##
##                            Day-wise demand of cabs at the City                                               ##
##--------------------------------------------------------------------------------------------------------------##

uber_city <- uber_data[which(uber_data$Pickup.point =="City"),]

uber_city$Req.day <- factor(uber_city$Req.day,
                            levels =c("Monday","Tuesday","Wednesday","Thursday","Friday"))

ggplot(uber_city, aes(x=(uber_city$Req.day), fill=uber_city$Req.day)) + 
  geom_bar(width=0.6) +
  ggtitle("Plot of Demand on each Day of the Week - City") + xlab("Weekday") + 
  ylab("Number of Requests") + 
  guides(fill=guide_legend(title="WeekDay")) + 
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  theme_light() +
  theme(axis.text = element_text(size =12)) + theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14)
  )

# The demand of cabs is on a lower side on Tuesday and Wednesdays 
# and is a bit higher on Monday, Thursday and Friday



##--------------------------------------------------------------------------------------------------------------##
##                                      Hourly demand at City                                                   ##
##--------------------------------------------------------------------------------------------------------------##

# Hourly demand at the City
Freq_city <-table(uber_city$Req.hour24)
Freq_city

# From 05:00 to 09:00 (i.e. The morning hours), there is a demand of more than 301 cabs.
# Here is a shortage of Supply as compared to the Demand in the morning hours at the City.

# Plotting hourly demand at City
ggplot(uber_city,aes(uber_city$Req.hour, fill=uber_city$Status)) + 
  geom_bar() +
  xlab("Hour of the Day") + 
  ylab("Number of Requests") + 
  ggtitle("Plot of Hourly demand at City") +
  guides(fill=guide_legend(title="Number of Requests")) +
  theme_minimal() +
  theme(axis.text = element_text(size =10)) + theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14)
  )

# We can clearly see from the Chart that the demand of Cabs at the City
# is fairly high from 5:00 hrs to 9:00 hrs (i.e. The morning hours).
# And this is way higher that the number of Drivers/Uber Cabs available



##--------------------------------------------------------------------------------------------------------------##
##                                  Hourly demand at the Airport                                                ##
##--------------------------------------------------------------------------------------------------------------##

# Hourly demand at the Airport
Freq_airport <- table(uber_airport$Req.hour24)
Freq_airport

# Since there are 301 drivers available
# Let's find out at what times there is a demand of more than 300 drivers
which(Freq_airport > 301)

# From 17:00 to 21:00(i.e. the Evening hours), there is a demand of more than 301 cabs.
# There is a shortage of Cabs as compared to the Demand in the Evening hours at the Airport.



# Plotting hourly demand at Airport
ggplot(uber_airport,aes(uber_airport$Req.hour, fill=uber_airport$Status)) + 
  geom_bar() +
  xlab("Hour of the Day") + 
  ylab("Number of Requests") + 
  ggtitle("Plot of Hourly demand at Airport") +
  guides(fill=guide_legend(title="Number of Requests")) +
  theme_minimal() +
  theme(axis.text = element_text(size =10)) + theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14)
  )

# We can clearly see from the Chart that from 17:00 hrs to 21:00 hours
# there is a fairly high demand of cabs at the Airport.
# And this is way higher that the number of Drivers/Uber Cabs available
# We can see that there are huge numbers of "No Cars Available" status shown 
# during this time period.


##--------------------------------------------------------------------------------------------------------------##
##                              Taking the Morning Slot Data for City                                           ##
##--------------------------------------------------------------------------------------------------------------##

morning_city <- uber_city[which(uber_city$Time.slot=="Morning"),]


##--------------------------------------------------------------------------------------------------------------##
##                           Taking the Evening Slot Data for the Airport                                       ##
##--------------------------------------------------------------------------------------------------------------##

evening_airport <- uber_airport[which(uber_airport$Time.slot=="Evening"),]

# Analyse the count of Status for getting some clarity at what actually is happening.

table(morning_city$Status)
# Cancelled     No Cars Available    Trip Completed 
#    922              477                 577

table(evening_airport$Status)
# Cancelled     No Cars Available    Trip Completed 
#     92             1076                 350 

# From the above findings, we can say that the number of cancellations 
# are TOO HIGH in the city in Morning slot.
# Also, the Demand of cabs at the Airport in the Evening slot is TOO HIGH.
# So, what's happening here could be that the drivers are "Cancelling" a lot of Pickup Requests
# in the city at the Morning time, because of which, the number of cabs available at the Airport
# in the evening is very low and hence the SUPPLY is DRASTICALLY less as compared to the DEMAND!!

# The drivers deny to go to airport from the city because there is less DEMAND at the airport, 
# and hence the idle time is more than what it should be. But returning to the city without a
# passenger would incur double cost to the Driver fir the trip from city to the airport.
# That's why, drivers prefer to wait for getting a Pickup Request from Airport to the City 
# instead of returning back to the city at their own expense.


##--------------------------------------------------------------------------------------------------------------##
##               Average trips completed by drivers at Airport and City                                         ##
##--------------------------------------------------------------------------------------------------------------##

cnt_airport <- sum(!is.na(uber_airport$Drop.timestamp))
cnt_airport
# 1327 trips are completed by drivers from Airport to city.
avg_airport <- round(cnt_airport/length(unique(uber_data$Driver.id)))
avg_airport

# 4 trips are currently being made by each driver from Airport to the City.


cnt_city <- sum(!is.na(uber_city$Drop.timestamp))
cnt_city
# 1504 trips are completed by drivers from City to the Airport.
avg_city <- round(cnt_city/length(unique(uber_data$Driver.id)))
avg_city

# 5 trips are currently being made by each driver from City to the Airport. 

# So, a driver completes around 9 trips between City and the Airport throughout the 5 days of the week.

# This was the SUPPLY part.

##--------------------------------------------------------------------------------------------------------------##

# Now lets find out the DEMAND part.

cnt_airport_demand <- sum(!is.na(uber_airport$Request.timestamp))
cnt_airport_demand
# 3238 trips are required to be completed by the drivers from Airport to the city.
avg_airport_demand <- round(cnt_airport_demand/length(unique(uber_data$Driver.id)))
avg_airport_demand
# 11 trips are required to be made by each driver from Airport to the City.


cnt_city_demand <- sum(!is.na(uber_city$Request.timestamp))
cnt_city_demand
# 3507 trips are required to be completed by the drivers from City to the Airport.
avg_city_demand <- round(cnt_city_demand/length(unique(uber_data$Driver.id)))
avg_city_demand
# 12 trips are required to be made by each driver from City to the Airport.

# The following steps can be taken to meet the demands and reduce the Demand-Supply gap:
# 1. A driver needs to complete around 23 trips between Airport and the City in 5 days of the week
#    to meet the demand.
#    This could be achieved by reducing the "idle time" of the drivers at the airport.
# 2. There can be a restriction levied on the drivers that they can not Cancel more that 5 rides per week or so.
#    If they do so, they will be penalised with a fee.
# 3. When the demand is high as compared to the supply, then Surcharge can be levied on the customer,
#    which would be a certain fraction of multiple of the total fare amount.
#    This way, the drivers would also get benefit and would be compelled to complete more trips.

##--------------------------------------------------------------------------------------------------------------##

# Export the data to plot charts in Tableau.

write.csv(uber_data,"uber_data.csv")

##---------------------------------------  E N D  O F  S C R I P T  --------------------------------------------##



