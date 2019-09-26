# get review measures
library(tidyverse)
library(entropy)
library(tidytext)
library(parallel)
source("nsb.entropy.R")


TIDY_REVIEW_PATH <- "../data/tidy_reviews.csv"
ENTROPY_OUTPATH <- "../data/review_nsb_entropy_subway_city_corpus.csv"
NCLUSTERS <- 6

tidy_review_df <- read_csv(TIDY_REVIEW_PATH) %>%
  select(review_num, name, city, review_tidy) %>%
  filter(name == "SUBWAY®Restaurants") %>%
  group_by(city) %>%
  distinct(review_tidy, .keep_all = T) %>%
  summarize(review_corpus = reduce(review_tidy, paste, sep = " "))

### lexical diversity
words_by_city <-  tidy_review_df %>%
  unnest_tokens(word, review_corpus) %>%
  count(city, word) #%>%
  #complete(city, word, fill = list(n = 0))

word_counts_by_city <- words_by_city %>%
  select(-word) %>%
  group_by(city) %>%
  nest(.key = "word_counts") %>%
  mutate(word_counts = map(word_counts, ~unlist(.$n))) 

get_entropy_safe <- function(count_vec){
  tryCatch({ 
    nsb.entropy(count_vec) / log(2) # return entropy in bits
    #entropy.NSB(count_vec, unit= "log2", CMD="~/nsb-entropy-1.14/build/nsb-entropy")
    }, 
   error = function(e) {
    NA
   })
}



# wrapper function
parallel_wrapper <- function(id, this_df, outfile){
  city <- pluck(this_df[id,], "city")
  word_counts <- pluck(this_df[id,], "word_counts")[[1]]
  entropy_value <- get_entropy_safe(word_counts)
  out_df <- data.frame(city = city,
                       entropy = entropy_value)
  write_csv(out_df, outfile, append = T)
}


### DO THE THING (IN PARALLEL)
# initial cluster
cluster <- makeCluster(NCLUSTERS, type = "FORK")

parLapply(cluster,
          1:nrow(word_counts_by_city), 
          parallel_wrapper, 
          word_counts_by_city, 
          ENTROPY_OUTPATH)

#parallel_wrapper(16134, word_counts_by_review, ENTROPY_OUTPATH)

#write_entropy <- function(rn, wc, path){
#  entropy_value = get_entropy_safe(wc)
#  out_df <- data.frame(review_num = rn,
#                       entropy = entropy_value)
#  write_csv(out_df, path, append = T)
#}
#walk2(word_counts_by_review$review_num[3], 
#      word_counts_by_review$word_counts[3], 
#      write_entropy, 
 #     ENTROPY_OUTPATH)
#this_review_num = 32261
#word_counts <- filter(word_counts_by_review, review_num == this_review_num) %>%
#  pull(word_counts)
#get_entropy_safe(word_counts[[1]])

           

