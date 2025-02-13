library(topicmodels)
library(textdata)

LDA_modelling <- function(word_counts, bigrams){
  word_counts_df <- bind_rows(word_counts, .id = "document")
  if (bigrams == TRUE) {
    dtm <- word_counts_df %>% cast_dtm(document = "document", term = "bigram", value = "n")
  }
  else {
    dtm <- word_counts_df %>% cast_dtm(document = "word", term = "word", value = "n")
  }
  
  k <- 5
  lda_model <- LDA(dtm, k = k, control = list(seed = 1234))  
  terms(lda_model, 10)
  
  topics <- tidy(lda_model, matrix = "beta")
  topics %>%
    group_by(topic) %>%
    slice_max(beta, n = 10) %>%
    ungroup() %>%
    ggplot(aes(beta, fct_reorder(term, beta), fill = as.factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    labs(x = "Beta", y = NULL)
}