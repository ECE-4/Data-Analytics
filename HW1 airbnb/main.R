library(stringr)
library(dplyr)
library(reshape)

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


#Will be useful to print pretty proportions
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(x * 100, format = format, digits = digits, ...), " %")
}



#####################
##### Analysis 1 ####
##### Point 1&2 #####
#####################
#preprocess the availability
mutate(calendars, available = ifelse(available=="t", 1, 0))

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

res <- aggregate(listings$availability_30, by = list(listings$city), FUN=mean)
names(res) <- c("city","avg_next_30d")

#Printing
cat("\nAnalysis 1 - Q5\n")
print(res)

###################
#### Analysis 1 ###
##### Point 6 #####
###################

listings$price <- str_replace_all(listings$price,"\\$","")
listings$price <- str_replace_all(listings$price,",","")
listings$price[listings$price==""] <- 0
listings$price <- as.numeric(listings$price)

listings$revenue_30 <- listings$availability_30 * listings$price
res <- aggregate(listings$revenue_30 , by = list(listings$city), FUN=mean)
names(res) <- c("city","revenue_next_30d")

#Printing
cat("\nAnalysis 1 - Q6\n")
print(res)


###################
#### Analysis 1 ###
##### Point 7 #####
###################

revenue_by_bedroom_size <- cast(listings, city ~ bedrooms, length)

#Printing
cat("\nAnalysis 1 - Q7\n")
print(revenue_by_bedroom_size)

###################
#### Analysis 1 ###
##### Point 8 #####
###################

revenue_by_roomtype<- cast(listings, city ~ room_type, length)

#Printing
cat("\nAnalysis 1 - Q7\n")
print(revenue_by_roomtype)


###################
#### Analysis 2 ###
##### Point 1 #####
###################
#Computing the proportion
roomTypes <- aggregate(listings$room_type, by=list(Category=listings$room_type), FUN=length)
names(roomTypes) <- c("room_type","proportion")
roomTypes$proportion <- roomTypes$proportion / sum(roomTypes$proportion) #compute a %
roomTypes <- arrange(nbhProp, desc(proportion)) #sort
roomTypes$proportion <- percent(roomTypes$proportion) #convert to %

#Printing
cat("\nAnalysis 2 - Q1\n")
print(roomTypes)


###################
#### Analysis 2 ###
##### Point 2 #####
###################
#Computing the proportion
houseSize <- aggregate(listings$bedrooms, by=list(Category=listings$bedrooms), FUN=length)
names(houseSize) <- c("bedrooms","proportion")
houseSize$proportion <- houseSize$proportion / sum(houseSize$proportion) #compute a %
houseSize <- arrange(nbhProp, desc(proportion)) #sort
houseSize$proportion <- percent(houseSize$proportion) #convert to %

#Printing
cat("\nAnalysis 2 - Q2\n")
print(houseSize)


###################
#### Analysis 2 ###
##### Point 3 #####
###################
#Computing the neighbourhood proportion
nbhProp <- aggregate(listings$neighbourhood_group_cleansed, by=list(Category=listings$neighbourhood_group_cleansed), FUN=length)
names(nbhProp) <- c("neighbourhood_group_cleansed","proportion")
nbhProp$proportion <- nbhProp$proportion / sum(nbhProp$proportion) #compute a %
nbhProp <- arrange(nbhProp, desc(proportion)) #sort
nbhProp$proportion <- percent(nbhProp$proportion) #convert to %

#Printing
cat("\nAnalysis 2 - Q3\n")
print(nbhProp)


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