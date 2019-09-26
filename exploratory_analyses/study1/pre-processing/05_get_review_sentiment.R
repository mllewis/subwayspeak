# get review sentiment
library(tidyverse)
library(tidytext)


TIDY_REVIEW_PATH <- "../data/tidy_reviews.csv"
SENTIMENT_PATH <- "../data/review_sentiment.csv"

tidy_review_df <- read_csv(TIDY_REVIEW_PATH) %>%
  select(review_num, review_tidy) %>%
  unnest_tokens(word, review_tidy)

sentiment_scores <- tidy_review_df %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(review_num) %>%
  summarize(sentiment_score_afinn = mean(score))

write_csv(sentiment_scores, SENTIMENT_PATH)