library(chron)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readtext)
library(stringr)

kAssignmentDirectory <- paste0("~/MSc in Data Analytics/Working with Data/",
                               "Assignments/1")
kMidnight <- strptime(as.character('00:00:00'), format = "%H:%M:%S")
kThreeAm <- strptime(as.character('03:00:00'), format = "%H:%M:%S")
kSixAm <- strptime(as.character('06:00:00'), format = "%H:%M:%S")
kNineAm <- strptime(as.character('09:00:00'), format = "%H:%M:%S")
kNoon <- strptime(as.character('12:00:00'), format = "%H:%M:%S")
kThreePm <- strptime(as.character('15:00:00'), format = "%H:%M:%S")
kSixPm <- strptime(as.character('18:00:00'), format = "%H:%M:%S")
kNinePm <- strptime(as.character('21:00:00'), format = "%H:%M:%S")
kBucket1 <- 'Midnight to 3 AM'
kBucket2 <- '3 AM to 6 AM'
kBucket3 <- '6 AM to 9 AM'
kBucket4 <- '9 AM to Noon'
kBucket5 <- 'Noon to 3 PM'
kBucket6 <- '3 PM to 6 PM'
kBucket7 <- '6 PM to 9 PM'
kBucket8 <- '9 PM +'

ComputeTime <- function (command.time) {
  # Converts the timestamp into time. Uses chron package.
  #
  # Args:
  #   command.time: timestamp recorded in milliseconds; origin is 1970-01-01.
  #
  # Returns:
  #   The time in hh:mm:ss format.
  origin <- strptime("1/1/1970 00:00", "%d/%m/%Y %H:%M")
  date.time <- origin + (as.double(command.time) / 1000)
  date.time.string <- as.character(date.time)
  time.string <- substr(date.time.string, 12, 19)
  return(chron(times=time.string))
}

ClassifyTime <- function (command.time) {
  # Classifies the time into 8 buckets of 3 hours each and returns the bucket.
  #
  # Args:
  #   command.time: time represented in string format in hh:mm:ss format.
  #
  # Returns:
  #   The bucket in which command.time falls.
  command.time <- strptime(command.time, format = "%H:%M:%S")
  bucket <- kBucket1
  
  # TODO: Normal if elseif else blocks not working. Look into this.
  bucket <- ifelse(command.time < kThreeAm,
                   kBucket1,
                   ifelse(command.time < kSixAm,
                          kBucket2,
                          ifelse(command.time < kNineAm,
                                 kBucket3,
                                 ifelse(command.time < kNoon,
                                        kBucket4,
                                        ifelse(command.time < kThreePm,
                                               kBucket5,
                                               ifelse(command.time < kSixPm,
                                                      kBucket6,
                                                      ifelse(
                                                        command.time < kNinePm,
                                                             kBucket7,
                                                             kBucket8)))))))
  return(bucket)
}

PlotCommandsInBuckets <- function (commands.df) {
  # Plots the number of commands run in the console against the time buckets.
  #
  # Args:
  #   commands.df: the data frame containing the relevant commands and time of
  #                running.
  commands.df.2 <- commands.df %>%
    mutate(time_bucket = ClassifyTime(time)) %>%
    mutate(date_bucket = ifelse(date < '2017-11-20', 'Till Nov 19',
                                'After Nov 19'))
  commands.df.2$time_bucket <- factor(commands.df.2$time_bucket,
                                      levels = c(kBucket1, kBucket2, kBucket3,
                                                 kBucket4, kBucket5, kBucket6,
                                                 kBucket7, kBucket8))
  commands.df.2$date_bucket <- factor(commands.df.2$date_bucket,
                                      levels = c('Till Nov 19',
                                                 'After Nov 19'))
  
  time_bucket_analysis <- commands.df.2 %>%
    group_by(date_bucket, time_bucket) %>%
    summarise(commands.count = sum(!is.na(command)))
  
  qplot(x=time_bucket, y=commands.count, data = time_bucket_analysis,
        group=time_bucket,
        geom = 'blank', main = 'At what times am I less lazy?') +
    geom_line(aes(group = 1)) +
    # geom_bar(stat='identity', position = 'dodge', alpha = 0.4) +
    xlab("Time") + ylab('No. of R commands') +
    theme(axis.text.x = element_text(angle=90)) +
    facet_grid(. ~ date_bucket)
}

PlotCommandsVsDates <- function (commands.df) {
  # Plots the number of commands run in the console against the dates they were
  # run on.
  # 
  # Args:
  #   commands.df: the data frame containing the relevant commands and time of
  #                running.
  commands.df.2 <- commands.df %>%
    mutate(date=as.character(date)) %>%
    group_by(date) %>%
    summarise(commands.count = sum(!is.na(command)))
  
  qplot(x = date, y = commands.count, data = commands.df.2,
        group = date,
        geom = 'blank', main = 'Time management') +
    geom_line(aes(group = 1)) +
    # geom_bar(stat='identity', position = 'dodge', alpha = 0.4) +
    xlab("Time") + ylab('Number of R commands') +
    theme(axis.text.x = element_text(angle=90))
}

setwd.command <- paste0('setwd("', kAssignmentDirectory, '")')
connection <- file('history.txt', 'r')
history.txt <- readLines(con = connection)
is.assignment.directory <- FALSE

commands <- c()
times <- c()

# NUmber of lines in the imported file
initial.file.length <- length(history.txt)

# For the analysis, we need to consider only comments run while working on the
# assignment. Throughout the assignment, the working directory has been the
# same. This fact is made use of to identify which are the commands relevant
# to the analysis.
for (line.num in 1:length(history.txt)) {
  history.line <- history.txt[line.num]
  colon.index <- str_locate(history.line, ':')[1]
  time.component <- substr(history.line, 1, colon.index - 1)
  command <- substr(history.line, colon.index + 1, str_length(history.line))
  if (startsWith(command, 'setwd')) {
    if (startsWith(command, setwd.command)) {
      is.assignment.directory <- TRUE
    } else {
      if (!startsWith(command, 'setwd.command')) {
        is.assignment.directory <- FALSE
      }
    }
  }
  if (is.assignment.directory == TRUE) {
    times <- append(times, time.component)
    commands <- append(commands, command)
  }
}
close(connection)
commands.df <- data.frame(time = times, command = commands,
                          stringsAsFactors = FALSE)

commands.df <- commands.df %>%
  mutate(date = as.Date((as.double(time)) / 86400000,
                        origin = '1970-01-01')) %>%
  mutate(time = ComputeTime(time))
  

# Some of the rows can be removed
# Comments, Rmd chunk beginnings etc.
# Also, multi-line commands are represented as multiple lines in the history
# file. This also needs to be taken care of.

commands.df <- commands.df %>%
  filter(!startsWith(command, '```')) %>%
  filter(!startsWith(command, '#')) %>%
  filter(!startsWith(command, '"')) %>%
  filter(!startsWith(command, "'")) %>%
  filter(!startsWith(command, '}')) %>%
  filter(!startsWith(command, '**')) %>%
  mutate(previous_command = lag(command)) %>%
  filter(!endsWith(previous_command, ','))

final.commands.count <- nrow(commands.df)

help.used <- sum(startsWith(commands.df$command, '?'))
view.used <- sum(startsWith(commands.df$command, 'View'))
ggplot.used <- sum(startsWith(commands.df$command, 'qplot')) +
  sum(startsWith(commands.df$command, 'ggplot')) +
  sum(startsWith(commands.df$command, 'geom')) +
  sum(startsWith(commands.df$command, 'xlab')) +
  sum(startsWith(commands.df$command, 'ylab')) +
  sum(startsWith(commands.df$command, 'facet'))
dplyr.used <- sum(startsWith(commands.df$command, 'filter')) +
  sum(startsWith(commands.df$command, 'select')) +
  sum(startsWith(commands.df$command, 'mutate')) +
  sum(startsWith(commands.df$command, 'arrange')) +
  sum(startsWith(commands.df$command, 'summarise')) +
  sum(startsWith(commands.df$command, 'inner_join')) +
  sum(startsWith(commands.df$command, 'group_by')) +
  sum(startsWith(commands.df$command, 'left_join'))
require.statements <- sum(startsWith(commands.df$command, 'require'))
