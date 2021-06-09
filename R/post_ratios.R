#  posting_ratios
#
#  inspired by https://towardsdatascience.com/a-guide-to-mining-and-analysing-tweets-with-r-2f56818fdd16
#
#' @param data Twitter API User Timeline  data frame
#'
#' @return A data frame consisting of retweet, tweet, and reply data
#' @export



posting_ratios <- function(data){
  user_og_tweets <- data[data$is_retweet==FALSE, ]

  user_og_tweets<- subset(user_og_tweets, is.na(user_og_tweets$reply_to_status_id))

  user_retweets <- data[data$is_retweet==TRUE,]

  user_replies <- subset(data, !is.na(data$reply_to_status_id))

  post_data <- data.frame(
    category=c("Original", "Retweets", "Replies"),
    count=c(nrow(user_og_tweets), nrow(user_retweets), nrow(user_replies)))

  post_data$fraction = data$count / sum(data$count)
  post_data$percentage = data$count / sum(data$count) * 100
  post_data$ymax = cumsum(data$fraction)
  post_data$ymin = c(0, head(data$ymax, n=-1))

  return(post_data)
}
