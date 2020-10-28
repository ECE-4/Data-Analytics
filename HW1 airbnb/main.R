library(stringr)

#Get the data
#malagaCal <- read.csv(file = 'data/malaga/calendar.csv')
#mallorcaCal <- read.csv(file = 'data/mallorca/calendar.csv')
#sevillaCal <- read.csv(file = 'data/sevilla/calendar.csv')

#Add the city
malagaCal$city="malaga"
mallorcaCal$city="mallorca"
sevillaCal$city="sevilla"

#concatenate all the data
calendars <- rbind(malagaCal, mallorcaCal, sevillaCal)


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

head(avgAvailability)


#####################
##### Analysis 1 ####
##### Point 3&4 #####
#####################
#Cleaning the prices
calendars$adjusted_price <- str_replace_all(calendars$adjusted_price,"\\$","")
calendars$adjusted_price <- as.numeric(calendars$adjusted_price)

#Computing the average revenue over 30 days
calendars$revenue <- (1-calendars$available) * calendars$adjusted_price
avgRevenue <- aggregate(calendars$revenue, by=list(Category=calendars$city), FUN=mean)
names(avgRevenue) <- c("listing_id","revenue")

head(avgRevenue)


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