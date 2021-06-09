# top_keywords_all
#
#
#' @import tidyverse
#' @import tidytext
#' @param data Twitter API User Timeline  data frame
#' @param num_words how many keywords to include (ex: top 10 words)
#'
#' @return A data frame consisting of most used words
#' @export


top_keywords_all <- function(data, num_words){
  keywords <- data %>%
    mutate(
      text = str_to_lower(text),
      text = str_replace_all(text, "(\\n)", " "),
      text = str_trim(gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", text)),
      text = str_replace_all(text, "(@.* )|([[:punct:]])", "")
    ) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    count(word, sort=TRUE)

  return(head(keywords,10))
}

