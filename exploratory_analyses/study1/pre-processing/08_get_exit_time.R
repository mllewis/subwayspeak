# get min distance (as the crow flies)  from city to exit
library(geosphere)
library(tidyverse)

CITY_PATH <-  "../data/all_locations.csv"
EXIT_PATH <-  "../data/highway_exits.csv"
DISTANCE_TO_EXIT <- "../data/distance_to_exit.csv"

exit_locations <- read_csv(EXIT_PATH)
city_locations <- read_csv(CITY_PATH)

exit_mat <- as.matrix(exit_locations[c("lon", "lat")])
city_mat <- as.matrix(city_locations[c("lon", "lat")])

dist_matrix <- distm(city_mat, exit_mat, fun = distHaversine) %>%
  as.data.frame() %>%
  mutate(city = city_locations$location) 

min_dist_df <- dist_matrix %>%
  gather("x", "distance_to_exit_meters", -city) %>%
  select(city, distance_to_exit_meters) %>%
  group_by(city) %>%
  summarize(min_distance_to_exit_meters = 
              min(distance_to_exit_meters))

write_csv(min_dist_df, DISTANCE_TO_EXIT)

