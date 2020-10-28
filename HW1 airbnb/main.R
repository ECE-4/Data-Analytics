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

#preprocess the availability
calendars$available[calendars$available=="t"] <- 1
calendars$available[calendars$available=="f"] <- 0
calendars$available <- as.numeric(calendars$available)

#Compute the availability over 30 days
avgAvailability <- aggregate(calendars$available, by=list(Category=calendars$city), FUN=mean)
names(avgAvailability) <- c("listing_id","availability")
avgAvailability$availability <- avgAvailability$availability * 30

#Point 1&2
head(avgAvailability)

#Point 3&4

#Point 5

#Point 6

#Point 7

#Point 8

