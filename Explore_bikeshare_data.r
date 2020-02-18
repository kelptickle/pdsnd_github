# read in files
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

head(wash)

head(chi)

# sort start times starting with most common by city
ny$startHour <- as.character(ny$Start.Time)
ny$startHour <- substr(ny$startHour,12,13)
print("ny")
sort(table(ny$startHour),decreasing=TRUE)[1]

wash$startHour <- as.character(wash$Start.Time)
wash$startHour <- substr(wash$startHour,12,13)
print("wash")
sort(table(wash$startHour),decreasing=TRUE)[1]

chi$startHour <- as.character(chi$Start.Time)
chi$startHour <- substr(chi$startHour,12,13)
print("chi")
sort(table(chi$startHour),decreasing=TRUE)[1]


# combine data sets and add city identifier
ny <- ny[,1:6]
ny$city <- 'New York'
wash <- wash[,1:6]
wash$city <- "Washington"
chi <- chi[,1:6]
chi$city <- "Chicago"
all<-rbind(ny,wash,chi)

# sort start times starting with most common with combined data
all$startHour <- as.character(all$Start.Time)
all$startHour <- substr(all$startHour,12,13)
print("all")
sort(table(all$startHour),decreasing=TRUE)[1]



#plot overall start times
library('ggplot2')
ggplot(aes(x = startHour),data = all) +
  geom_bar(stat="Count") +
  ggtitle ('Rides by Hour') +
  xlab("Ride Start Time") +
  ylab("Number of Rides")

#Plot start times by cities.
library('ggplot2')
ggplot(aes(x = startHour),data = all) +
  geom_bar(stat="Count") +
  ggtitle ('Start Hour by City') +
  xlab("Ride Start Time")+
  ylab("Number of Rides")+
  facet_wrap(~ city)

# define mode function to identify most common city
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# aggregate most common station by city
aggregate(Start.Station ~ city, all, Mode)
                     


# remove na values
allclean <- na.omit(all)
# show the average trip duration in minutes
trip.dur.city <- aggregate(Trip.Duration/60 ~ city, allclean ,mean)
trip.dur.city


#plot average trip duration by city
library('ggplot2')
ggplot(aes(x = city, y = `Trip.Duration/60`), data = trip.dur.city) +
  geom_bar(stat="Identity") +
  ggtitle ('Trip Duration by City') +
  xlab("City")+
  ylab("Trip Duration")



system('python -m nbconvert Explore_bikeshare_data.ipynb')
