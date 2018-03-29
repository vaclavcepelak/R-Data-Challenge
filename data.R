
### Environment ####################################################################

rm(list = ls())
cat('\014')


library(data.table)
library(lubridate)
library(stringr)
library(chron)
library(geosphere)



# Parameters
HOLIDAYS <- as.Date(c('2016-01-01', '2016-03-25', '2016-03-28'))
ACCEPTABLE_DISTANCE <- 100

# Window
LL_lat <- 50.08296
LL_lon <- 14.4397
UR_lat <- 50.10058
UR_lon <- 14.46716


# Helper function

get_max_density <- function(time_data) {
  # compute the density
  # timespan is expanded because density function is low close to midnight from both sides
  # the previous periods need to be added so that the density at midnight is computed correctly
  d <- density(c(time_data, time_data[time_data > 0.5] - 1, time_data[time_data <= 0.5] + 1))
  # return the time with maximum density
  # hot fix: for multimodal distributions the minimum value is selected
  max_d <- min(d$x[d$y == max(d$y, na.rm = TRUE)], na.rm = TRUE)
  
  # if max_d is out of the [0, 1] interval, add or substract 1
  if (max_d < 0) max_d <- max_d + 1 else if (max_d > 1) max_d <- max_d - 1
  
  return(times(max_d))
}



### Load data ######################################################################

instagram_data <- fread('./Karlin_Geo_Analysis/instagram_data.csv')

instagram_data[, created_time := lubridate::dmy_hm(created_time)]
instagram_data[, weekday := lubridate::wday(created_time, label = TRUE, week_start = 1)]
instagram_data[, time := chron::times(hour(created_time) / 24 + minute(created_time) / 24 / 60)]

# get business days using the bizdays library
# create calendar
calendar <- bizdays::create.calendar('CZ', 
                                     holidays = HOLIDAYS, # add holidays in the timespan
                                     weekdays = c("saturday", "sunday"),
                                     start.date = as.Date('2016-01-01'), 
                                     end.date = as.Date('2016-03-31'))

# use is.bizday function
instagram_data[, free_day := !bizdays::is.bizday(instagram_data$created_time, calendar)]
instagram_data[, free_day_before := !bizdays::is.bizday(as.Date(instagram_data$created_time) + 1, calendar)]


### Clean data #####################################################################


# compute the difference between actual and median distance for each location point
instagram_data[, dist_from_place := geosphere::distm(cbind(longitude, latitude), 
                                                     c(median(longitude), median(latitude)), 
                                                     fun = distGeo), by = location_id]

# remove the data with long distance
instagram_data <- instagram_data[dist_from_place <= ACCEPTABLE_DISTANCE]


# remove the data outside the selected window
instagram_data <- instagram_data[longitude >= LL_lon & latitude >= LL_lat & longitude <= UR_lon & latitude <= UR_lat, ]

### Locations data #################################################################

locations <- instagram_data[, 
                            .(lon_median = median(longitude),
                              lat_median = median(latitude),
                              time_peak = get_max_density(time),
                              n_posts = .N,
                              n_users = length(unique(user_id)),
                              n_likes_total = sum(likes_count),
                              n_comments_total = sum(comments_count)),
                            by = .(location_id, location_name)]



### Hashtags data ##################################################################

hashtags <- setNames(str_extract_all(instagram_data$caption, '#\\w+'), instagram_data$id)
hashtags <- setnames(data.table(stack(hashtags)), c('hashtag', 'id'))
hashtags[, N := .N, by = .(hashtag)]

hashtags_freq <- unique(hashtags[order(-N), .(hashtag, N)])




### Analysis #######################################################################

# get most popular locations
# display them on a map

# clustering methods








karlin_map <- get_map(location = c(lon = median(instagram_data$longitude), lat = median(instagram_data$latitude)), maptype = "terrain", source = 'google', zoom = 15)

attr(karlin_map, 'bb')

map <- ggmap(karlin_map, extent = "device") + 
  geom_point(aes(x = longitude, y = latitude), colour = "red", alpha = 0.1, size = 2, data = instagram_data)

map

qs <- c(0, 0.001, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.999, 0.9999)
quantile(instagram_data$longitude, qs)
quantile(instagram_data$latitude, qs)

map1 <- plot_mapbox(dat, x = ~long, y = ~lat) %>% 
  add_paths(size = I(2)) %>%
  add_segments(x = -100, xend = -50, y = 50, 75) %>%
  layout(mapbox = list(
    zoom = 0,
    center = list(lat = ~median(lat), lon = ~median(long))
  ))


# hist(as.numeric(instagram_data$time))
# a <- density(as.numeric(instagram_data$time))
# plot(a)

# approx(a$x,a$y,xout = as.numeric(instagram_data$time[1]))






library(ggmap)

