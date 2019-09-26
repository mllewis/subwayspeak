# get review measures
library(tidyverse)
library(entropy)
library(tidytext)
library(parallel)
source("nsb.entropy.R")


TIDY_REVIEW_PATH <- "../data/tidy_reviews.csv"
ENTROPY_OUTPATH <- "../data/review_nsb_entropy.csv"
NCLUSTERS <- 6

tidy_review_df <- read_csv(TIDY_REVIEW_PATH) %>%
  select(review_num, review_tidy, city, name)

### lexical diversity
word_counts_by_review <-  tidy_review_df %>%
  group_by(city, name) %>%
  distinct(review_tidy, .keep_all = T) %>%
  unnest_tokens(word, review_tidy) %>%
  count(review_num, word) %>%
  select(-word) %>%
  group_by(review_num) %>%
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
  review_num <- pluck(this_df[id,], "review_num")
  word_counts <- pluck(this_df[id,], "word_counts")[[1]]
  entropy_value <- get_entropy_safe(word_counts)
  out_df <- data.frame(review_num = review_num,
                       entropy = entropy_value)
  write_csv(out_df, outfile, append = T)
}


### DO THE THING (IN PARALLEL)
# initial cluster
cluster <- makeCluster(NCLUSTERS, type = "FORK")

parLapply(cluster,
          1:nrow(word_counts_by_review), 
          parallel_wrapper, 
          word_counts_by_review, 
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

           

