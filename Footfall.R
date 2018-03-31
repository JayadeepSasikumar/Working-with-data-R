library(dplyr)
library(ggplot2)
library(lubridate)
library(readODS)
library(readr)
library(reshape2)
library(rvest)
library(stringr)

kColNames <- c('time', 'mon_in', 'mon_out', 'tue_in', 'tue_out', 'wed_in',
               'wed_out', 'thu_in', 'thu_out', 'fri_in', 'fri_out', 'sat_in',
               'sat_out', 'sun_in', 'sun_out')
kEntranceOne <- "O'Connell Street at Clerys"
kEntranceTwo <- "South King St"

GetEntranceData <- function (sheet, entrance.name) {
  # Classifies the time into 8 buckets of 3 hours each and returns the bucket.
  #
  # Args:
  #   sheet: the ODS sheet containing a week's data of all the entrances.
  #   entrance.name: name of the entrance of which footfall data is to be
  #   extracted.
  #
  # Returns:
  #   A data frame containing the hourly traffic data of the relevant data
  #   frame.
  search.string <- paste("Entrance Name:", entrance.name)
  index <- which(sheet$time == search.string)
  start.index <- index + 3
  end.index <- index + 26
  entrance.df <- sheet[start.index:end.index, ]
  return(entrance.df)
}

GetWeekwiseData <- function (entrance.df, direction = 'in') {
  # Returns the molten weekwise data in the direction specified.
  #
  # Args:
  #   entrance.df: the data frame containing the info of an entrance.
  #   direction: direction of the traffic, either in or out.
  #
  # Returns:
  #   A data frame containing the molten weekwise data in the direction
  #   specified.
  out.variables <- c('time', 'mon_out', 'tue_out', 'wed_out', 'thu_out',
                     'fri_out', 'sat_out', 'sun_out')
  in.variables <- c('time', 'mon_in', 'tue_in', 'wed_in', 'thu_in', 'fri_in',
                    'sat_in', 'sun_in')
  col.nums <- 1:16
  names(col.nums) <- colnames(entrance.df)
  if (direction == 'in') {
    exclude.list <- col.nums[out.variables]
  } else {
    exclude.list <- col.nums[in.variables]
  }
  weekwise.df <- entrance.df %>%
    select(-exclude.list) %>%
    melt(id.vars = 'week_num') %>%
    group_by(week_num) %>%
    mutate(value = as.integer(value))
  if (direction == 'in') {
    weekwise.df <- summarise(weekwise.df, weekly_in = sum(value))
  } else {
    weekwise.df <- summarise(weekwise.df, weekly_out = sum(value))
  }
  return(weekwise.df)
}

ReadFromODSFile <- function () {
  # Reads from the ODS file all the data pertaining to the two entrances
  # chosen and writes it to 2 CSVs. This needs to be run only for the first
  # time. CSV files can be used for the Markdown report.
  #
  # THIS FUNCTION TOOK ~9.5 MINUTES TO RUN.
  #
  # Returns:
  #   A data frame containing the molten weekwise data in the direction
  #   specified.  
  entrance.one.data <- data.frame()
  entrance.two.data <- data.frame()
  for (sheet.num in 1:52) {
    sheet <- read_ods("pedestrianfootfall2013.ods", range = "A1:O100",
                      sheet = sheet.num)
    colnames(sheet) <- kColNames
    entrance.one.week.data <- GetEntranceData(sheet, kEntranceOne)
    entrance.one.week.data <- cbind(entrance.one.week.data, week_num=sheet.num)
    entrance.one.data <- rbind(entrance.one.data, entrance.one.week.data)
    
    entrance.two.week.data <- GetEntranceData(sheet, kEntranceTwo)
    entrance.two.week.data <- cbind(entrance.two.week.data, week_num=sheet.num)
    entrance.two.data <- rbind(entrance.two.data, entrance.two.week.data)
  }
  entrance.one.data <<- entrance.one.data
  entrance.two.data <<- entrance.two.data
  write_csv(entrance.one.data, 'entrance_one_data.csv')
  write_csv(entrance.two.data, 'entrance_two_data.csv')
}

ReadFromCSVFiles <- function () {
  # Reads from the two relevant CSVs and creates 2 global variables.
  entrance.one.data <<- read_csv('entrance_one_data.csv')
  entrance.two.data <<- read_csv('entrance_two_data.csv')
}

PlotWeekWiseComparison <- function () {
  # Plots the weekwise footfall patterns over the year for both the entrances.
  e1.weekwise.in <- GetWeekwiseData(entrance.one.data)
  e2.weekwise.in <- GetWeekwiseData(entrance.two.data)
  e1.weekwise.out <- GetWeekwiseData(entrance.one.data, direction='out')
  e2.weekwise.out <- GetWeekwiseData(entrance.two.data, direction='out')
  
  e1.weekwise <- inner_join(e1.weekwise.in, e1.weekwise.out, by = 'week_num') %>%
    melt(id.vars = 'week_num')
  e2.weekwise <- inner_join(e2.weekwise.in, e2.weekwise.out, by = 'week_num') %>%
    melt(id.vars = 'week_num')
  e1.weekwise <- cbind(e1.weekwise, entrance=kEntranceOne)
  e2.weekwise <- cbind(e2.weekwise, entrance=kEntranceTwo)
  weekwise <- rbind(e1.weekwise, e2.weekwise)
  weekwise$variable = factor(weekwise$variable, labels = c('In', 'Out'),
                             levels = c('weekly_in', 'weekly_out'))
  qplot(week_num, value, data=weekwise, geom='line', colour=variable) +
    facet_grid(. ~ entrance) +
    xlab('Week') + ylab('Footfall') + labs(colour = 'Direction') +
    labs(title = 'Footfall over the weeks')
}

ExtractExchangeRate <- function(exchange.string) {
  # Extracts and returns the exchange rate as a double from the string format
  # present in the table from poundSterlingLIVE.
  #
  # Args:
  #   exchange.string: the string containing the exchange rate information
  #                    as present in the table scraped from poundSterlingLIVE.
  #
  # Returns:
  #   The exchange rate (EUR against GBP) in double format.
  exchange.string <- str_split(exchange.string, " = ", simplify = TRUE)[, 2]
  exchange.string <- substr(exchange.string, 1, nchar(exchange.string) - 4)
  exchange.rate <- round(1 / as.double(exchange.string), 3)
  return(exchange.rate)
}

PlotFootfallVsExchangeRateTrend <- function (exchange.df) {
  # Plots the trends of Euro exchange rate and footfall at entrance 1
  # (Clerys) over the 52 weeks.
  #
  # Args:
  #   exchange.df: the data frame containing the datewise exchange rate.
  e1.weekwise.in <- GetWeekwiseData(entrance.one.data)
  
  footfall.and.exchange.rates <- exchange.df %>%
    inner_join(e1.weekwise.in, by = 'week_num') %>%
    mutate(scaled_exchange_rate = round(scale(exchange_rate)[, 1], 2)) %>%
    mutate(scaled_footfall = round(scale(weekly_in)[, 1], 2)) %>%
    select(-c(exchange_rate, weekly_in)) %>%
    melt(id.vars = 'week_num')
  footfall.and.exchange.rates$variable <- factor(
    footfall.and.exchange.rates$variable,
    levels = c('scaled_exchange_rate', 'scaled_footfall'),
    labels = c('EUR value', "Footfall at Clerys (In)"))
  qplot(week_num, value, data=footfall.and.exchange.rates,
        geom='line', colour=variable) +
    xlab('Week') + ylab('Footfall and EUR value') + labs(colour = 'Measure') +
    labs(title = "A comparison of trends") +
    labs(subtitle = "Footfall at Clerys, O'Connell St. and EUR rate against GBP")
}

PlotFootfallVsExchangeRateScatter <- function (exchange.df) {
  # Plots a scatterplot of Euro exchange rate and footfall at entrance 1
  # (Clerys) over the 52 weeks.
  #
  # Args:
  #   exchange.df: the data frame containing the datewise exchange rate.
  e1.weekwise.in <- GetWeekwiseData(entrance.one.data)
  footfall.and.exchange.rates <- exchange.df %>%
    inner_join(e1.weekwise.in, by = 'week_num')
  qplot(exchange_rate, weekly_in, data=footfall.and.exchange.rates) +
    xlab('EUR value') + ylab("Footfall at Clerys") +
    labs(colour = 'Measure') +
    labs(title = "Footfall at Clerys, O'Connell St vs EUR rate against GBP")
}

dublinked.url <- paste0("https://data.dublinked.ie/dataset/8204be0a-6348-459e",
                        "-96e9-65bb75600ec3/resource/384fe47a-2f25-4f52-8fc5",
                        "-8e61899951e9/download/pedestrianfootfall2013.ods")
download.file(dublinked.url, "pedestrianfootfall2013.ods")

entrance.one.data <- data.frame()
entrance.two.data <- data.frame()

ReadFromCSVFiles()

# 4.2
# Scraping the data
exchange.url <- paste0("https://www.poundsterlinglive.com/",
                       "bank-of-england-spot/historical-spot-exchange-rates/",
                       "gbp/GBP-to-EUR-2013#charts")
exchange.data <- read_html(exchange.url)
table.nodes <- html_nodes(exchange.data, 'table')
exchange.tables <- html_table(table.nodes)
exchange.df <- exchange.tables[[length(exchange.tables)]]

colnames(exchange.df) <- tolower(colnames(exchange.df))
exchange.df <- exchange.df %>%
  mutate(date = as.POSIXct(strptime(date, format = "%a, %d %b %Y"))) %>%
  mutate(week_num = week(date)) %>%
  filter(week_num < 53) %>%
  mutate(exchange_rate = ExtractExchangeRate(rate)) %>%
  select(-c(currency, rate, date)) %>%
  group_by(week_num) %>%
  summarise(exchange_rate = round(mean(exchange_rate), 3))
