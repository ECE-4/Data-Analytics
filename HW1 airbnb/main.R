library(stringr)
library(dplyr)

#####################
#### Importation ####
#####################
cat("Getting Calendars... ")
#Get the data
malagaCal <- read.csv(file = 'data/malaga/calendar.csv')
mallorcaCal <- read.csv(file = 'data/mallorca/calendar.csv')
sevillaCal <- read.csv(file = 'data/sevilla/calendar.csv')

#Add the city
malagaCal$city="malaga"
mallorcaCal$city="mallorca"
sevillaCal$city="sevilla"

#concatenate all the data
calendars <- rbind(malagaCal, mallorcaCal, sevillaCal)

cat("Done\n")


cat("Getting Listing... ")
#Get the data
malagaList <- read.csv(file = 'data/malaga/listings.csv')
mallorcaList <- read.csv(file = 'data/mallorca/listings.csv')
sevillaList <- read.csv(file = 'data/sevilla/listings.csv')

#Add the city
malagaList$city="malaga"
mallorcaList$city="mallorca"
sevillaList$city="sevilla"

#concatenate all the data
listings <- dplyr::bind_rows(malagaList, mallorcaList, sevillaList)
#Note: The mallorca dataframe doesnt have the same columns as the other sets, so the bind_rows function will put NA when there is no data

cat("Done\n")


#####################
##### Analysis 1 ####
##### Point 1&2 #####
#####################
#preprocess the availability
calendars$available[calendars$available=="t"] <- 1
calendars$available[calendars$available=="f"] <- 0
calendars$available <- as.numeric(calendars$available)

#Compute the availability over 30 days
avgAvailability <- aggregate(calendars$available, by=list(Category=calendars$city), FUN=mean)
names(avgAvailability) <- c("listing_id","availability")
avgAvailability$availability <- avgAvailability$availability * 30

#Printing
cat("\nAnalysis 1 - Q1&2\n")
print(avgAvailability)


#####################
##### Analysis 1 ####
##### Point 3&4 #####
#####################
#Cleaning the prices
calendars$adjusted_price <- str_replace_all(calendars$adjusted_price,"\\$","")
calendars$adjusted_price <- str_replace_all(calendars$adjusted_price,",","")
calendars$adjusted_price[calendars$adjusted_price==""] <- 0
calendars$adjusted_price <- as.numeric(calendars$adjusted_price)

#Computing the average revenue over 30 days
calendars$revenue <- (1-calendars$available) * calendars$adjusted_price
avgRevenue <- aggregate(calendars$revenue, by=list(Category=calendars$city), FUN=mean)
names(avgRevenue) <- c("listing_id","revenue")

#Printing
cat("\nAnalysis 1 - Q3&4\n")
print(avgRevenue)


###################
#### Analysis 1 ###
##### Point 5 #####
###################



###################
#### Analysis 1 ###
##### Point 6 #####
###################



###################
#### Analysis 1 ###
##### Point 7 #####
###################



###################
#### Analysis 1 ###
##### Point 8 #####
###################




###################
#### Analysis 2 ###
##### Point 1 #####
###################


#Computing the proportion
rooms <- aggregate(listings$room_type, by=list(Category=listings$room_type), FUN=length)
names(rooms) <- c("room_type","proportion")
rooms$proportion <- rooms$proportion / nrow(listings)

#Printing
cat("\nAnalysis 2 - Q1\n")
print(rooms)

###################
#### Analysis 2 ###
##### Point 2 #####
###################



###################
#### Analysis 2 ###
##### Point 3 #####
###################



###################
#### Analysis 2 ###
##### Point 4 #####
###################



###################
#### Analysis 2 ###
##### Point 5 #####
###################



###################
#### Analysis 2 ###
##### Point 6 #####
###################



###################
#### Analysis 2 ###
##### Point 7 #####
###################



###################
#### Analysis 2 ###
##### Point 8 #####
###################