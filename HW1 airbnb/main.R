malagaCal <- read.csv(file = 'data/malaga/calendar.csv')
head(malagaCal)
#head(malagaCalendar["date"])

malagaList <- read.csv(file = 'data/malaga/listings.csv')
names(malagaList) #affiche le nom des colonnes