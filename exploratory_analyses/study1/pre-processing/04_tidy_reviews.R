# merge reviews, combind with metadata
library(tidyverse)


REVIEW_PATH <- "../data/reviews/"
PLACE_ID_PATH <- "../data/place_ids/"

OUTFILE <- "../data/tidy_reviews.csv"
all_reviews <- list.files(REVIEW_PATH, full.names = T) %>%
  map(function(x) {
    m = read_feather(x) 
    m$author.names <- NULL
    write_feather(m, x)})

all_reviews_clean <- all_reviews %>%
  bind_rows() %>%
  janitor::clean_names() %>%
  filter(!is.na(reviews)) %>%
  filter(reviews != "") %>%
  select(place_id, reviews, author_name)

all_place_id_meta <- list.files(PLACE_ID_PATH, full.names = T) %>%
  map(read_feather) %>%
  bind_rows()

all_place_id_meta_clean <- all_place_id_meta %>%
  janitor::clean_names() %>%
  rename(city = location) %>%
  select(place_id, city, types, name) %>%
  mutate_all(funs(as.factor)) %>%
  distinct(place_id, city, types, name) # some place ids are duplicated across cities

clean_text <- function(text){
  text %>%
    tolower() %>% 
    str_remove_all("[:punct:]") %>%
    str_trim() %>% 
    str_squish() 
}

# merge on place id
tidy_reviews <-  left_join(all_reviews_clean, all_place_id_meta_clean) %>%
  mutate(review_num = 1:nrow(.),
         review_tidy = clean_text(reviews)) %>%
  select(review_num, city, place_id, types, name, reviews, review_tidy) 

write_csv(tidy_reviews, OUTFILE)