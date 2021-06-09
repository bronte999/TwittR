# top_tweets_interval
#
#
#' @import tidyverse
#' @import lubridate
#' @param data Twitter API User Timeline  data frame
#' @param num_tweets how many tweets to include (ex: top 10 tweets)
#' @param time_start start of time interval investigating
#' @param time_end end of time interval investigating
#'
#' @return A data frame consisting of highest liked & retweeted tweets
#' @export


top_tweets_interval <- function(data, num_tweets, time_start, time_end){
  time_frame <- interval(time_start, time_end)
  top_x_tweets <- data %>%
    filter(created_at %within% time_frame) %>%
    mutate(success_score = retweet_count + favorite_count) %>%
    arrange(desc(success_score))


  return(head(top_x_tweets, num_tweets))
}


# top_tweets_since_today
#
#
#' @import tidyverse#' @import lubridate
#' @param data Twitter API User Timeline  data frame
#' @param num_tweets how many tweets to include (ex: top 10 tweets)
#' @param n_days tweets from the past n_days
#'
#' @return A data frame consisting of highest liked & retweeted tweets
#' @export

top_tweets_since_today <- function(data, num_tweets, n_days){
  time_frame <- interval(today() - n_days, today())
  top_x_tweets <- data %>%
    filter(created_at %within% time_frame) %>%
    mutate(success_score = retweet_count + favorite_count) %>%
    arrange(desc(success_score))


  return(head(top_x_tweets, num_tweets))
}
