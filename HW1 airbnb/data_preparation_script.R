library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)
library(reshape)

setwd("C:/Users/alexd/Documents/travail/Ing5/Data-Analytics/HW1 airbnb")

# a generic function to prepare data for a specific city
prepare_data <- function(city)
{
    # Cleaning listings dataframe
    
    # suppose raw data is stored in data_raw/city/listings.csv.gz
    listings_url <- file.path("data", city, "listings.csv")
    # suppose raw data is stored in data_raw/city/data_date/calendar.csv.gz
    calendar_url <- file.path("data", city, "calendar.csv")
    
    print(paste0("reading data from ", listings_url))
    listings <- read.csv(gzfile(listings_url))
    print(paste0("reading data from ", calendar_url))
    calendar <- read.csv(gzfile(calendar_url))
    
    ## Add Keys: columns city and day date
    listings$city <- city
    
    ## Select interesting columns
    ### Most columns don't contain interesting information
    columns_listings <- c("city", "id", "neighbourhood_cleansed", 
                          "latitude", "longitude", 
                          "property_type", "room_type", "accommodates", "bedrooms", 
                          "beds", "price", "minimum_nights",  "maximum_nights")
    
    listings <- listings %>% 
        select(columns_listings) %>% 
        arrange(id)
    
    # Cleaning calendar dataframe
    
    ## arrange by id and date
    calendar <- calendar %>% 
        arrange(listing_id, date)
    
    ## add day number (starting first day)
    calendar <- calendar %>%
        group_by(listing_id) %>%
        mutate(day_nb = row_number()) %>%
        ungroup()
    
    ## change available column to binary
    calendar <- calendar %>%
        mutate(available = ifelse(available=="t", 1, 0))
    
    ## clean price column and transform to numeric
    calendar <- calendar %>%
        mutate(price = str_replace(price, "\\$", ""),
               adjusted_price = str_replace(adjusted_price, "\\$", ""))
    calendar <- calendar %>%
        mutate(price = str_replace(price, ",", ""),
               adjusted_price = str_replace(adjusted_price, ",", ""))
    calendar <- calendar %>%
        mutate(price = as.numeric(price),
               adjusted_price = as.numeric(adjusted_price))
    
    ## calculate estimated revenue for upcoming day
    calendar <- calendar %>%
        mutate(revenue = price*(1-available))
    
    ## calculate availability, price, revenue for next 30, 60 days ... for each listing_id
    calendar <- calendar %>%
        group_by(listing_id) %>%
        summarise(availability_30 = sum(available[day_nb<=30], na.rm = TRUE),
                  #availability_60 = sum(available[day_nb<=60], na.rm = TRUE),
                  #availability_90 = sum(available[day_nb<=90], na.rm = TRUE),
                  #availability_365 = sum(available[day_nb<=365], na.rm = TRUE),
                  price_30 = mean(price[day_nb<=30 & available==0], na.rm = TRUE),
                  #price_60 = mean(price[day_nb<=60 & available==0], na.rm = TRUE),
                  #price_90 = mean(price[day_nb<=90 & available==0], na.rm = TRUE),
                  #price_365 = mean(price[day_nb<=365 & available==0], na.rm = TRUE),
                  revenue_30 = sum(revenue[day_nb<=30], na.rm = TRUE),
                  #revenue_60 = sum(revenue[day_nb<=60], na.rm = TRUE),
                  #revenue_90 = sum(revenue[day_nb<=90], na.rm = TRUE),
                  #revenue_365 = sum(revenue[day_nb<=365], na.rm = TRUE)           
        )
    
    listings_cleansed <- listings %>% left_join(calendar, by = c("id" = "listing_id"))
    
    dir.create(file.path("data_cleansed", city), recursive = TRUE)
    
    write.csv(listings_cleansed, file.path("data_cleansed", city, "listings.csv"))
    print(paste0("saving data into ", file.path("data_cleansed", city, "listings.csv")))
    
}  

# Example: Reading data for malaga:
# Preparing data 

# city <- "malaga"
# prepare_data(city)

# Example: Prepare data for multiple cities

cities <- c("malaga", "mallorca", "sevilla")

for(i in 1:length(cities)){
    city <- cities[i]
    print("-------------------------------------------------")
    print(paste(c("Preparing data for", city), collapse = " "))
    prepare_data(city)
}

# Clean Environment
rm(list=ls())

## Once data for multiple cities are prepared
## We can read these data and concatenate them together into one dataframe

# Reading cleansed data
cities <- c("malaga", "mallorca", "sevilla")

files_paths <- c()

# Read data in cities between min_date and max_date
for(city in cities){
    file_dir <- file.path(".", "data_cleansed", city)
    file_subdirs <- list.dirs(file_dir)
    file_subdirs <- file_subdirs[-1]
    
    for(file_subdir in file_subdirs){
        if(file_subdir < file.path(file_dir, min_date) | file_subdir > file.path(file_dir, max_date)  )
            file_subdirs = file_subdirs[file_subdirs != file_subdir]
    }
    files_paths <- c(files_paths, file_subdirs)
}
files_paths <- file.path(files_paths, "listings.csv")

#listings <- 
#    do.call(rbind,
#            lapply(files_paths, read.csv))

malaga <- read.csv(file="data_cleansed/malaga/listings.csv")
mallorca <- read.csv(file="data_cleansed/mallorca/listings.csv")
sevilla <- read.csv(file="data_cleansed/sevilla/listings.csv")

listings <- rbind(malaga, mallorca, sevilla)
    
## Preprocess
listings$bedrooms <- ifelse(listings$bedrooms >= 5, "5+", listings$bedrooms)


                        ################ Work ################ 
typeof(listings)
## Analysis 1 ##

#Q1
first <- ggplot(listings, aes(x=city, y=availability_30)) + 
    geom_boxplot() + stat_summary(fun.y=mean ,geom="point",color="red", aes(x=listings$city, y=listings$availability_30))

first

#Q2
avgRevenue <- aggregate(listings$revenue_30, by=list(Category=listings$city), FUN=mean)

second <- ggplot(listings, aes(x=city, y=revenue_30)) + 
    geom_boxplot() + ylim(0,5000) + stat_summary(fun.y=mean ,geom="point",color="red", aes(x=listings$city, y=listings$revenue_30))

abline(h = mean(revenue_30))
second

#Q3
third <- ggplot(listings, aes(x=availability_30, color=listings$city)) + 
    geom_histogram(fill="grey", position="dodge") 
third

#Q4
listing_bis <- listings %>% filter(revenue_30 > 0)

fourth <- ggplot(listing_bis, aes(x=revenue_30, color=city)) + 
    geom_histogram(fill="grey", position="dodge") + xlim(0,10000)
fourth

#Q5
fifth <- ggplot(listings, aes(x = as.factor(bedrooms), y=revenue_30, fill=as.factor(city))) + 
    geom_bar(position = position_dodge() ,stat="summary") 
fifth

#Q6

sixth <- ggplot(listings, aes(x = as.factor(room_type), y=revenue_30, fill=as.factor(city))) + 
  geom_bar(position = position_dodge() ,stat="summary") 
sixth


## Analysis 2 ##

#Q1
seventh <- ggplot(listings, aes(x = as.factor(room_type))) + 
    geom_bar(position = "dodge", aes(fill = as.factor(listings$city)))
seventh

#Q2
eigth <- ggplot(listings, aes(x = as.factor(bedrooms))) + 
    geom_bar(position = "dodge", aes(fill = as.factor(listings$city)))
eigth

#Q3 We take Malaga city
#ninth <- ggplot(listings, aes(x = as.factor(neighbourhood_cleansed))) + 
#    geom_histogram(position = "dodge", aes(fill = as.factor(listings$city)))
#geom_histogram(position = "dodge", fill = as.factor(city))
#ninth



listings_malaga <- listings %>% filter(city == "malaga")

ggplot(listings_malaga, aes(x=neighbourhood_cleansed)) + geom_histogram(stat = "count") #for Malaga

#Q4

first <- ggplot(listings_malaga, aes(x=room_type, y=availability_30)) + 
  geom_boxplot() + stat_summary(fun.y=mean ,geom="point",color="red", aes(x=listings_malaga$room_type, y=availability_30))

first

snd <- ggplot(listings_malaga, aes(x=bedrooms, y=availability_30)) + 
  geom_boxplot() + stat_summary(fun.y=mean ,geom="point",color="red", aes(x=listings_malaga$bedrooms, y=availability_30))

snd

third <- ggplot(listings_malaga, aes(x=neighbourhood_cleansed, y=availability_30, fill=as.factor(x = neighbourhood_cleansed) )) + 
  geom_bar( stat="summary") 

third 



#Q5

first <- ggplot(listings_malaga, aes(x=room_type, y=revenue_30)) + 
  geom_boxplot() + stat_summary(fun.y=mean ,geom="point",color="red", aes(x=listings_malaga$room_type, y=revenue_30)) + ylim(0,4000)

first

snd <- ggplot(listings_malaga, aes(x=bedrooms, y=revenue_30)) + 
  geom_boxplot() + stat_summary(fun.y=mean ,geom="point",color="red", aes(x=listings_malaga$bedrooms, y=revenue_30)) +ylim(0,4000)

snd

third <- ggplot(listings_malaga, aes(x=neighbourhood_cleansed, y=revenue_30, fill=as.factor(x = neighbourhood_cleansed) )) + 
  geom_bar( stat="summary") 

third 

#Q6

first <- ggplot(listings_malaga, aes(x=room_type, y=availability_30)) + 
  geom_bar(stat = "identity") 

first

snd <- ggplot(listings_malaga, aes(x=bedrooms, y=availability_30)) + 
  geom_bar(stat = "identity") 

snd

third <- ggplot(listings_malaga, aes(x=neighbourhood_cleansed, y=availability_30, fill=as.factor(x = neighbourhood_cleansed) )) + 
  geom_bar( stat="identity") 

third 
# we can see that there are over 80k availibility days for Centro whereas there are a few for CAmpanillas

#Q7

first <- ggplot(listings_malaga, aes(x=room_type, y=revenue_30)) + geom_bar(stat = "identity") 

first
# We can see they will be over 7.5 milion dollars spent in malaga within the next 30 days only for home/apt. 

snd <- ggplot(listings_malaga, aes(x=bedrooms, y=revenue_30)) + geom_bar(stat = "identity") 

snd
#In Malaga, within the enxt 30 days, the apt with one room will generate near 2.5 Milion dollars

third <- ggplot(listings_malaga, aes(x=neighbourhood_cleansed, y=revenue_30, fill=as.factor(x = neighbourhood_cleansed) )) + 
  geom_bar( stat="identity") 

third 

#The centro will bring over 4.7 milion euro in the next 30 days 
