### GET PLACE IDS FOR ALL SUBWAYS IN US####
# queries google api with search string and write places ids to feather
####################################

# libraries
library(googleway)
library(tidyverse)
library(stringr)
library(feather)

# get API: https://developers.google.com/maps/documentation/javascript/

# params
PAUSE_LENGTH <- 15
APIKEY <- "AIzaSyDWiK6y5-MHJD4WC7unkhmUSTBAuAOXHfA"
SEARCH_STRING <- "subway in united states"
PLACE_TYPE <- "restaurant"
NUM_PAGES_SAVE <- 25 # write to feather every n*20 places 

current_page <- 1 # keeps track of all pages
write_page <- 1 # keeps track of how many pages since writing to file
current_token <- "start"

# loop until token is null
while(!is.null(current_token) ) {
  
    print(current_page)
    
    current_token = switch(current_token == "start", NULL, current_token) #ifelse, but switch bc return NULL
    
    page <- google_places(search_string = SEARCH_STRING, 
                             place_type = PLACE_TYPE,
                             page_token = current_token,
                             key = APIKEY)
    
    if(is.null(names(page))) {page = purrr::flatten(page)}
    
    Sys.sleep(runif(1, 0, PAUSE_LENGTH))
    
    places = as.data.frame(cbind(place_id = page$results$place_id,
                                 name = unlist(page$results$name),
                                 lat = page$results$geometry.location.lat,
                                 lon = page$results$geometry.location.lon,
                                 price_level = page$results$price_level,
                                 rating = page$results$rating,
                                 types =  page$results$types,
                                 address = page$results$formatted_address))
    
    if(write_page == 1){
      all_places = places
    } else {
      all_places = bind_rows(all_places, places)
    }
    
    # write to feather every NUM_PAGES_SAVE * 20 place_ds 
    if(write_page %% NUM_PAGES_SAVE == 0) {
      
      all_places <- cbind(all_places[!sapply(all_places, is.list)], # unlist all columns
          (t(apply(all_places[sapply(all_places, is.list)], 1, unlist))))
      
      write_feather(all_places, paste0("../data/place_ids/subway_place_ids_page_", current_page))
      write_page <- 0 
      all_places <- NULL
    }
    
    current_token <- page$next_page_token 
    current_page <- current_page + 1
    write_page <- write_page + 1
}