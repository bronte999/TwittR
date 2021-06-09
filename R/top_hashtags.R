# top_hashtags_all
#
#
#' @import tidyverse
#' @param data Twitter API User Timeline  data frame
#' @param num_hash how many hashtags to include (ex: top 10 hashtags)
#'
#' @return A data frame consisting of the user's most common hashtags
#' @export


top_hashtags_all <- function(data, num_hash){
  top_hashtags <- unlist(data$hashtags)
  top_hashtags <- data.frame(sort(table(top_hashtags[!is.na(top_hashtags)]), decreasing=T))
  return(head(top_hashtags, num_hash))
}


# top_hashtags_past_x_days
#
#
#' @import tidyverse
#' @import lubridate
#' @param data Twitter API User Timeline  data frame
#' @param num_hash how many hashtags to include (ex: top 10 hashtags)
#' @param n_days hashtags from the past n_days
#'
#' @return A data frame consisting of most used hashtag in past n days
#' @export

top_hashtags_past_x_days <- function(data, num_hash, n_days){
  time_frame <- interval(today() - n_days, today())
  data <- data %>%
    filter(created_at %within% time_frame)
  return(top_hashtags_all(data, num_hash))
}

# top_hashtags_interval
#
#
#' @import tidyverse#'
#' @import lubridate
#' @param data Twitter API User Timeline  data frame
#' @param num_hash how many hashtags to include (ex: top 10 hashtags)
#' @param n_days hashtags from the past n_days
#' @param time_start start of time interval investigating
#' @param time_end end of time interval investigating
#'
#' @return A data frame consisting of most used hashtag during time interval
#' @export

top_hashtags_interval <- function(data, num_hash, start_time, end_time){
  time_frame <- interval(time_start, time_end)
  data <- data %>%
    filter(created_at %within% time_frame)
  return(top_hashtags_all(data, num_hash))
}

