#' ---
#' title: 'Dealing with local and summer time in R'
#' author: Bernardo Niebuhr - bernardo_brandaum@yahoo.com.br
#' ---

#' Problem: local time/timezones when dealing with dates and times 
#' may lead to unwanted transformations when the place under consideration is
#' subject to differences between summer and winter time. Here we write a 
#' function to remove the time differences added by this problem.
#' 
#' # Introduction
#' 
#' Let's say we have a table of dates and times in which time was recorded in
#' both UTC and the local timezone. We'll
#' consider the winter time as the standard and change the 
#' summer time to what it would be correspondent in the winter.
#' 
#' As an example we use GPS data from Norway, in which the changes between
#' times in 2018 were:
#' 
#' - Daylight saving (summer) time began on Sunday, 25 March 2018 at 02:00 (+1h)
#' - Summer time ended on Sunday, 28 October 2018 at 03:00 (-1h)
#' 

#----------------- label=load_packages, warning=FALSE, message=FALSE
# packages

library(knitr)
library(tidyverse)
library(lubridate)
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file()) # root project folder

# load data from several files, get individual ID, UTC and local LMT time
files  <- list.files(pattern="*.csv", recursive = T) %>% 
  grep(pattern = "new_collars", value = T)

# data <-  do.call(rbind, lapply(files, function(x) readr::read_csv(x))) %>% 
data <-  do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE))) %>% 
  tibble::as_tibble() %>%
  dplyr::mutate(UTC_timestamp = as.POSIXct(strptime(paste(UTC_Date, UTC_Time), format = "%Y-%m-%d %H:%M:%S", tz = 'UTC')),
         Local_timestamp = as.POSIXct(strptime(paste(LMT_Date, LMT_Time), format = "%Y-%m-%d %H:%M:%S", tz = 'CET'))) %>%
  dplyr::select("CollarID", dplyr::contains("timestamp"))

# data <-  do.call(rbind, lapply(files, function(x) readr::read_csv(x))) %>% 
#   mutate(UTC_timestamp = lubridate::ymd_hms(paste(UTC_Date, UTC_Time)),
#          Local_timestamp = as.POSIXct(strptime(paste(LMT_Date, LMT_Time), format = "%Y-%m-%d %H:%M:%S", tz = 'CET'))) %>% 
#   dplyr::select("CollarID", dplyr::contains("timestamp"))

data

data$UTC_timestamp[1:4]
data$Local_timestamp[1:4]

#' We're interested in the periods when there was such changes, let's take a look.

# dates
(summer.begin <- as.POSIXct(strptime("2018-03-25 03:00:00", format = "%Y-%m-%d %H:%M:%S", tz = 'CET')))
(summer.ends <- as.POSIXct(strptime("2018-10-28 02:30:00", format = "%Y-%m-%d %H:%M:%S", tz = 'CET')))

# summer time begins in 2018
data.begin <- data %>% 
  dplyr::filter(Local_timestamp < summer.begin + lubridate::days(1), Local_timestamp > summer.begin - lubridate::days(1)) %>% 
  dplyr::arrange(CollarID, Local_timestamp)
data.begin %>% print(n = 20)

data.end <- data %>% 
  dplyr::filter(Local_timestamp < summer.ends + lubridate::days(1), Local_timestamp > summer.ends - lubridate::days(1)) %>% 
  dplyr::arrange(CollarID, Local_timestamp)
data.end %>% print(n = 20)

#' Ouch! It seems this is not a good example - it kept 1h difference time between the UTC and the
#' local Norwegian time (GMT+1). Then I'll just apply some code here so that this is not forgotten, 
#' in case this is needed in the future. 
#' 
#' Just as an exercise, we'll add 1 hour of difference in the summer time, as we expected the time would be
#' presented in this data, instead of removing hours to adjust the time to UTC.

# we can look at the time zone of the data like that
format(data.end$Local_timestamp, format = "%Z")

# now we take data from one individual and add 1h to summer time points
test <- data.begin %>% 
  dplyr::filter(CollarID == '27948') %>% 
  dplyr::mutate(Local_timestamp_correct = ifelse(format(Local_timestamp, format = "%Z") == 'CEST', 
                                                 as.character(Local_timestamp), as.character(Local_timestamp + hours(1)))) 

test %>% 
  print(n = 20)

#' This is how we expected teh data would be presented. From this point, it would be possible to use the same
#' approach to correct (remove) the difference in hours from the summer time, to standardize a local time,
#' or to change all date-time information to UTC, for example.

