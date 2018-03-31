library(dplyr)
library(ggmap)
library(ggplot2)
library(httr)
library(jsonlite)

kJCDAPIKey <- "f3af0867d0ae0de6957fa29b4469667fba4e229e"

ClassifyStation <- function (bike.stands) {
  # Classifies the bike station depending on the number of bike stands in it.
  #
  # Args:
  #   bike.stands: the number of bike stands in the station.
  #
  # Returns:
  #   Class of the station based on the number of bike stands - three possible
  #   classes are Small, Medium and Large.
  if (bike.stands <= 20) {
    return ("Small")
  } else if (bike.stands <= 30) {
    return ("Medium")
  } else {
    return ("Large")
  }
}

ClassifyUtilisation <- function (utilisation.percentage) {
  # Classifies the bike station depending on the proportion of bikes
  # in circulation.
  #
  # Args:
  #   utilisation.percentage: the proportion of bikes in circulation.
  #
  # Returns:
  #   Class of the station based on the utilisation percentage - three possible
  #   classes are Low, Medium and High.
  if (utilisation.percentage <= 33) {
    return ("Low")
  } else if (utilisation.percentage <= 66) {
    return ("Medium")
  } else {
    return ("High")
  } 
}

GetOpenStations <- function (df) {
  # Returns the number of stations which have their status as OPEN in a 
  # string format. This is specifically to be used for the Markdown report.
  #
  # Args:
  #   df: the data frame containing the bike info.
  #
  # Returns:
  #   A string that can be used directly in the report. In case all the
  #   stations have their status as OPEN, 'all' will be added as a prefix to
  #   the output.
  total.stations.count <- nrow(df)
  open.stations.count <- sum(df$status=="OPEN")
  result <- as.character(open.stations.count)
  if (open.stations.count == total.stations.count) {
    result <- paste('all', result)
  }
  return(result)
}

GetDistanceAndTime <- function (origins, destinations) {
  # Returns the distance and time of journey taking origins and destinations
  # as inputs. Hits Google's distance matrix API for this.
  #
  # Args:
  #   origins: latlong of origins
  #   destinations: latlong of destinations
  #
  # Returns:
  #   A vector of which the first element is the distance to the destination
  #   from the origin.
  google.distance.matrix.url <- paste0("http://maps.googleapis.com/maps/api/",
                                       "distancematrix/json?origins=",
                                       origins[1], ",", origins[2],
                                       "&destinations=",
                                       destinations[1], ",", destinations[2])
  distance.data <- GET(google.distance.matrix.url)
  distance.data.json <- content(distance.data, as="text", encoding="UTF-8")
  distance.data.df <- fromJSON(distance.data.json)
  distance <- distance.data.df$rows$elements[[1]]$distance$value
  duration <- distance.data.df$rows$elements[[1]]$duration$value
  return(c(distance, duration))
}

AddDistanceFromHere <- function (df, origin) {
  # A wrapper that calls GetDistanceAndTime().
  #
  # Args:
  #   df: the data frame containing the bike info.
  #   origin: latlong of origin.
  #
  # Returns:
  #   The inputted data frame with a new column added to it, distance. This is
  #   the distance to each station from the inputted origin latlong.
  df <- df %>% rowwise() %>%
    mutate(distance=GetDistanceAndTime(origins=origin,
                                       destinations=c(position_lat,
                                                      position_long))[1])
  return(df)
}

GetNearestStationsInfo <- function (df, origin, drop) {
  # Returns a data frame containing the info on 5 of the nearest bike stations.
  #
  # Args:
  #   df: the data frame containing the bike info.
  #   origin: latlong of origin.
  #   drop: a Boolean variable indicating whether the user wants to drop off a
  #         bike or borrow one. drop = TRUE will indicate that she is looking
  #         to return a borrowed bike.
  #
  # Returns:
  #   A data frame containing the info on 5 of the nearest bike stations
  df <- AddDistanceFromHere(df, origin)
  if (drop) {
    df <- df %>% 
      select(name, distance, available_bike_stands) %>%
      filter(available_bike_stands>0) %>%
      mutate(name_availability=paste0(
        name, " (", available_bike_stands, ")"
      ))
  } else {
    df <- df %>%
      select(name, distance, available_bikes) %>%
      filter(available_bikes>0) %>%
      mutate(name_availability=paste0(
        name, " (", available_bikes, ")"
      ))
  }
  return(data.frame(head(df, 5)))
}

PlotDistanceAvailability <- function (df, drop) {
  # Plots the bikes/bike stands available for the five nearest stations taking
  # into consideration the availability of the required resource(bike/bike
  # stand).
  #
  # Args:
  #   df: a data frame containing the info on the nearest bike stations
  #   drop: a Boolean variable indicating whether the user wants to drop off a
  #         bike or borrow one. drop = TRUE will indicate that she is looking
  #         to return a borrowed bike.
  if (drop) {
    x.label <- 'Stations (number of stands available)'
    title <- 'Stations and availability of stands'
  } else {
    x.label <- 'Stations (number of bikes available)'
    title <- 'Stations and availability of bikes'
  }
  qplot(x=reorder(name_availability, distance),
        y=distance, data=df, fill=distance, geom='blank', main=title) +
    geom_bar(stat='identity', position='dodge') +
    xlab(x.label) +
    ylab('Distance in metres') + 
    labs(fill = "Distance")
}

PlotNearestAvailableStations <- function (df, origin="Spire", drop=FALSE) {
  # Plots the bikes/bike stands available for the five nearest stations taking
  # into consideration the availability of the required resource(bike/bike
  # stand).
  #
  # Args:
  #   df: a data frame containing the info on the nearest bike stations
  #   origin: A string which will be used to find the location. dublin is added
  #           to the string to make it more specific. By default, the value is
  #           set to 'Spire'
  #   drop: a Boolean variable indicating whether the user wants to drop off a
  #         bike or borrow one. drop = TRUE will indicate that she is looking
  #         to return a borrowed bike. By default, the value is set to FALSE. 
  origin = tolower(origin)
  if (!grepl('dublin', origin)) {
    origin <- paste(origin, "dublin")
  }
  origin.coordinates <- geocode(origin)
  origin.latlong <- c(origin.coordinates[1, 2], origin.coordinates[1, 1])
  nearest.stations <- GetNearestStationsInfo(open.stations,
                                             origin.latlong,
                                             drop)
  PlotDistanceAvailability(nearest.stations, drop)
}

# Going ahead with Dublin
dublin.url <- paste0("https://api.jcdecaux.com/vls/v1/stations",
                    "?contract=Dublin",
                    "&apiKey=",
                    kJCDAPIKey)
dublin.data <- GET(dublin.url)
dublin.data.json <- content(dublin.data, as="text", encoding="UTF-8")
dublin.data.df <- fromJSON(dublin.data.json)
open.stations <- subset(dublin.data.df, status=="OPEN")
total.stands.count <- sum(open.stations$bike_stands)

position <- open.stations$position
open.stations$position <- NULL
open.stations$position_lat <- position$lat
open.stations$position_long <- position$lng

open.stations <- mutate(open.stations,
                         class=sapply(bike_stands, ClassifyStation),
                         utilisation_percentage=round(
                           available_bike_stands*100/bike_stands, 2),
                         utilisation_class=sapply(
                           utilisation_percentage, ClassifyUtilisation))
open.stations$class <- factor(open.stations$class,
                               levels=c('Small', 'Medium', 'Large'),
                               ordered=TRUE)
open.stations$utilisation_class <- factor(open.stations$utilisation_class,
                                           levels=c('Low', 'Medium', 'High'),
                                           ordered=TRUE)

utilisation.by.size <- table(open.stations$class,
                             open.stations$utilisation_class)
utilisation.by.size <- as.data.frame.matrix(utilisation.by.size)

open.stations <- group_by(open.stations, class)
class.summary <- summarise(open.stations,
                           total_bike_stands=sum(bike_stands),
                           total_available_stands=sum(available_bike_stands),
                           total_available_bikes=sum(available_bikes),
                           percentage_used=round(
                             total_available_stands*100/total_bike_stands, 2))
open.stations <- ungroup(open.stations)
class.summary <- select(class.summary, class, total_bike_stands,
                        percentage_used)
colnames(class.summary) <- c('Class', 'Total bike stands',
                             'Percentage utilisation')
