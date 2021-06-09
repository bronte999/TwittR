# get_sentiment_count
#
#
#' @import tidyverse
#' @import tidytext
#' @param data Twitter API User Timeline  data frame
#'
#' @return A data frame consisting of word count and associated sentiment
#' @export




get_sentiment_count <- function(data){
  keywords <- data %>%
    mutate(
      text = str_replace_all(text, "(\\n)", " "),
      text = str_trim(gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", text)),
      text = str_replace_all(text, "(@.* )|([[:punct:]])", "")
    ) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort=TRUE) %>%
    ungroup() %>%
    mutate(
      score = case_when(
        sentiment == "positive" ~ n * 1,
        sentiment == "negative" ~ (-1) * n)
    )
  return(keywords)
}
