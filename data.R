
### Environment ####################################################################

rm(list = ls())
cat('\014')


library(data.table)
library(lubridate)
library(stringr)
# library(chron)
library(geosphere)


Sys.setlocale(category = "LC_TIME", locale="en_GB.UTF-8") 
Sys.setenv(TZ = 'UTC')


# Parameters
ACCEPTABLE_DISTANCE <- 100

# Window
LL_lat <- 50.08296
LL_lon <- 14.4397
UR_lat <- 50.10058
UR_lon <- 14.46716


# for analytical purposes, the day begins at a specified time
# this function creates a unified time column setting all the dates to minimum

unify_time <- function(data, 
                       day_begin_time = '18:00', # time when analytical day begins
                       day_before = TRUE, # should the beginning be set to the day before?
                       time_column = 'created_time', # variable names...
                       unified_time_column = 'unified_time',
                       unified_date_column = 'unified_date',
                       daytype_column = 'is_freeday',
                       holidays = as.Date(c('2016-01-01', '2016-03-25', '2016-03-28'))) {
  # get minimum date
  day0 <- as.POSIXct(as.Date(min(data[[time_column]])))
  # compute the unified time
  unified_time <- as.POSIXct(data[[time_column]]) - as.POSIXct(as.Date(data[[time_column]])) + day0
  
  # extract the time of the beginning
  time0 <- as.POSIXct(strptime(day_begin_time, format = '%H:%M'))
  time0 <- time0 - as.POSIXct(as.Date(time0) + as.numeric(day_before)) + day0
  
  if (day_before == TRUE) {
    unified_time <- ifelse(unified_time >= time0 + 24 * 60 * 60,
                           unified_time - 24 * 60 * 60,
                           unified_time)
  } else {
    unified_time <- ifelse(unified_time <= time0,
                           unified_time + 24 * 60 * 60,
                           unified_time)
  }
  
  data[, (unified_time_column) := as.POSIXct(unified_time, origin = '1970-01-01')]
  
  # compute unified date
  data[, (unified_date_column) := as.Date(data[[time_column]])]
  
  data[, (unified_date_column) := as.Date(ifelse(as.Date(data[[unified_time_column]]) == as.Date(min(data[[time_column]])), # if no change in unified time date occured
                                                 data[[unified_date_column]], # get the original value
                                                 data[[unified_date_column]] + 2 * day_before - 1), origin = '1970-01-01')] # otherwise add or substract 1 day depending on whether it is day before or not

  # get business days using the bizdays library
  # create calendar
  calendar <- bizdays::create.calendar('CZ', 
                                       holidays = holidays, # add holidays in the timespan
                                       weekdays = c("saturday", "sunday"),
                                       start.date = as.Date(min(data[[time_column]])) - 1, 
                                       end.date = as.Date(max(data[[time_column]])) + 1)
  
  # use is.bizday function to detect free days
  data[, (daytype_column) := !bizdays::is.bizday(data[[unified_date_column]], calendar)]
  
  return(data)
}

# Helper function

get_max_density <- function(time_data, 
                            center = strptime('2016-01-01 06:00', format = '%Y-%m-%d %H:%M')) {
  
  if (length(time_data) == 0) return(NA)
  if (length(time_data) == 1) return(time_data)
  
  # compute the density
  # timespan is expanded because density function is low close to midnight from both sides
  # the previous periods need to be added so that the density at midnight is computed correctly
  
  
  d <- density(as.numeric(c(time_data, time_data[time_data > center] - 24 * 60 * 60, time_data[time_data <= center] + 24 * 60 * 60)))
  # return the time with maximum density
  # for multimodal distributions the minimum value is selected
  max_d <- min(d$x[d$y == max(d$y, na.rm = TRUE)], na.rm = TRUE)
  
  # if max_d is out of the [0, 1] interval, add or substract 1
  if (max_d < (center - 12 * 60 * 60)) {
    max_d <- max_d + 24 * 60 * 60
  } else if (max_d > (center + 12 * 60 * 60)) {
    max_d <- max_d - 24 * 60 * 60
  }
  
  return(as.POSIXct(max_d, origin = '1970-01-01'))
}




### Load data ######################################################################

instagram_data <- fread('./Karlin_Geo_Analysis/instagram_data.csv')

instagram_data[, created_time := strptime(created_time, format = '%d/%m/%Y %H:%M')]
instagram_data[, weekday := ifelse(data.table::wday(created_time) - 1 == 0, 7, data.table::wday(created_time) - 1)]

instagram_data <- unify_time(instagram_data)


### Clean data #####################################################################


# compute the difference between actual and median distance for each location point
instagram_data[, dist_from_place := geosphere::distm(cbind(longitude, latitude), 
                                                     c(median(longitude), median(latitude)), 
                                                     fun = distGeo), by = location_id]

# remove the data with long distance
instagram_data <- instagram_data[dist_from_place <= ACCEPTABLE_DISTANCE]


# remove the data outside the selected window
instagram_data <- instagram_data[longitude >= LL_lon & latitude >= LL_lat & longitude <= UR_lon & latitude <= UR_lat, ]

# remove posts with missing id and duplicated
instagram_data <- instagram_data[id != '']
instagram_data <- unique(instagram_data)

 

### Locations data #################################################################

locations <- instagram_data[, 
                            .(lon_ = median(longitude),
                              lat_ = median(latitude),
                              time_peak = get_max_density(unified_time),
                              time_peak_workday = as.POSIXct(get_max_density(unified_time[is_freeday == FALSE])), # as.double fixes the issues with wrong type of column for NAs
                              time_peak_freeday = as.POSIXct(get_max_density(unified_time[is_freeday == TRUE])),
                              n_posts = .N,
                              f_posts_freeday = mean(is_freeday),
                              n_users = length(unique(user_id)),
                              n_likes_total = sum(likes_count),
                              n_comments_total = sum(comments_count)),
                            by = .(location_id, location_name)][order(-n_posts),]

locations[, n_posts_rank := data.table::frankv(n_posts, order = -1L, ties.method = 'min')]

### Spot time series ###############################################################

time_data <- instagram_data[, .(unified_time, unified_date, is_freeday)]

center <- strptime('2016-01-01 06:00', format = '%Y-%m-%d %H:%M')

time_data[, total := factor('Total', levels = c('Total', 'Working day', 'Free day'))]
time_data[, is_freeday := factor(is_freeday, levels = c(FALSE, TRUE), labels = c('Working day', 'Free day'))]

time_data[, time_boundary := as.POSIXct(ifelse(unified_time <= center, 
                                               unified_time + 24 * 60 * 60,
                                               unified_time - 24 * 60 * 60), 
                                        origin = '1970-01-01')]

time_data <- data.table::melt(time_data, measure.vars = c('unified_time', 'time_boundary'), variable.name = 'Time_type', value = 'Time')

time_data <- data.table::melt(time_data, measure.vars = c('total', 'is_freeday'), variable.name = 'Var', value = 'Group')

time_data[, Group := factor(Group, levels = c('Total', 'Free day', 'Working day'))]

time_data[, Weight := 1 / (length(unique(unified_date)) /
                             length(unique(time_data$unified_date))), 
          by = Group]



### Hashtags data ##################################################################

hashtags <- setNames(str_extract_all(tolower(instagram_data$caption), '#\\w+'), instagram_data$id)
hashtags <- setnames(data.table(stack(hashtags)), c('hashtag', 'id'))
hashtags[, total_N_occurences := .N, by = .(hashtag)]



setkey(hashtags, id)
setkey(instagram_data, id)

hashtags <- merge(hashtags, 
                  instagram_data[, .(id, unified_time, location_id, location_name)])

# move transform peak unified time to begin at 06:00
hashtags[, unified_time:= as.POSIXct(ifelse(as.numeric(unified_time) <= 
                                              as.numeric(
                                                as.POSIXct('2016-01-01 06:00')), 
                                            unified_time + 24 * 60 * 60,
                                            unified_time), origin = '1970-01-01')]

hashtags_freq <- hashtags[!is.na(hashtag),
                          .(N = .N,
                            time_peak = get_max_density(unified_time,
                                                        center = as.POSIXct('2016-01-01 18:00'))),
                          by = .(hashtag)][order(-N)]

hashtags_freq[, freq_rank := frankv(N, order = -1, ties.method = 'min')]


### Save data ######################################################################

save(hashtags, hashtags_freq, instagram_data, locations, time_data, file = './instagram_data_frames.rda')

