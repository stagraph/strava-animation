#-----------------------------------------------------------------------------------
# this code reads exported data from Strava.com app and process them into animation
#-----------------------------------------------------------------------------------
library(readr)
library(dplyr)
library(lubridate)
library(sf)
library(purrr)
library(dplyr)
library(tidyr)
library(ggmap)
library(ggplot2)
library(geosphere)
library(gganimate)
library(transformr)

# read list of all stored activities
activities <- read_csv("C:/Users/x/Downloads/strava/activities.csv")


# get list of "WALK" type activities from defined time range
filtered_activities <- activities %>% 
	mutate(Activity.Date = mdy_hms(`Activity Date`))%>%
	select (
		id = `Activity ID`, 
		at = `Activity Type`, 
		dt = Activity.Date
	) %>% 
	filter(
		at == "Walk", 
		dt >= as.POSIXct("2022-05-09"), 
		dt <= as.POSIXct("2023-06-11")
	) %>%
	select(id)
	
# get list of GPX files that contains walk activities
gpx_files <- paste0("C:/Users/x/Downloads/strava/activities/", filtered_activities$id, ".gpx")

# function for reading gpx file and returning data.frame of selected data
read_gpx <- function(file_path) {
	gpx_data <- st_read(file_path, layer = "track_points", quiet = TRUE)
	coords <- st_coordinates(gpx_data) 
	gpx_data <- gpx_data %>%
		mutate(
			lat = coords[, "Y"],  
			lon = coords[, "X"]
		) %>%
		select(
			lat, 
			lon, 
			time
		) %>%
		mutate(datetime = as.POSIXct(time)) %>%
		select(
			lat, 
			lon, 
			datetime
		)
	return(gpx_data)
}

# read all gpx files into data.frame
routes <- map_dfr(gpx_files, function(file) {
	if (file.exists(file)) {
		data <- read_gpx(file)
		data$Activity.ID <- gsub(".gpx$", "", basename(file))
		return(data)
  }
  return(NULL)
})

# filter routes by lat range
routes <- routes %>% filter(lat <48.22)

# postprocess all data
routes <- routes %>%
	arrange(
		Activity.ID, 
		datetime
	) %>%  
	mutate(
		duration = if_else(
			Activity.ID != lag(Activity.ID, default = first(Activity.ID)), 
			1,  
			as.numeric(difftime(datetime, lag(datetime), units = "secs"))
		),
		duration_adjusted = if_else(
			is.na(duration), 
			0, 
			if_else(duration == 0, 1, duration)),
		cumulative_sum = cumsum(duration_adjusted),
		cumulative_sum_str = case_when(
			cumulative_sum >= 24 * 3600 ~ paste0(
				cumulative_sum %/% (24 * 3600), " days ",
				(cumulative_sum %% (24 * 3600)) %/% 3600, " hours ",
				(cumulative_sum %% 3600) %/% 60, " minutes"
			),
			cumulative_sum >= 3600 ~ paste0(
				cumulative_sum %/% 3600, " hours ",
				(cumulative_sum %% 3600) %/% 60, " minutes"
			  ),
			TRUE ~ paste0(
				cumulative_sum %/% 60, " minutes"
			)
		),
		Activity_ID_New = as.numeric(factor(Activity.ID)),
		distance = distHaversine(
			cbind(lag(lon), lag(lat)),
			cbind(lon, lat)
		),
		distance = if_else(
			is.na(distance) | Activity.ID != lag(Activity.ID, default = first(Activity.ID)), 
			0, 
			distance
		),  
		distance_cumsum = cumsum(distance)/1000
		
	) %>% 
	select(
		id = Activity_ID_New, 
		datetime,
		lat,
		lon, 
		duration = cumulative_sum_str, 
		distance  =distance_cumsum
	) %>%
	mutate(
		lon = as.numeric(lon),
		lat = as.numeric(lat)
	)
	
# get bounging area where is 90% of points located 
lon_bounds <- quantile(routes$lon, probs = c(0.05, 0.95), na.rm = TRUE)
lat_bounds <- quantile(routes$lat, probs = c(0.05, 0.95), na.rm = TRUE)

# create animation definition
animation <- 
	ggplot(
		routes, 
		aes(x = lon, y = lat, frame=id)
	) +
	geom_path(
		aes(group = id), 
		color = "red", 
		size = 1, 
		alpha = 0.1
	) +
	labs(title = "296 walks; 18 days 3 hours 42 minutes on feet; distance 1 745 km")+
	theme(
		panel.background = element_blank(), plot.background = element_blank(), panel.grid = element_blank(),       
		axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),        
		plot.title = element_text(hjust = 0.5, size = 26)  # Nastavenie titulu
	) + 
	coord_fixed(xlim = lon_bounds, ylim = lat_bounds) +
	transition_time(id) + 
	ease_aes('linear') +
	shadow_mark(past = TRUE, future = FALSE)

# save animation to GIF
animate(
	animation, 
	duration = 30, 
	width = 800, 
	height = 800, 
	renderer = gifski_renderer("output.gif")
)

