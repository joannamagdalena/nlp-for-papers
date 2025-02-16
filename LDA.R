library(topicmodels)
library(textdata)

library(textmineR)
library(Matrix)
library(slam)

LDA_modeling <- function(word_counts, k, bigrams){
  word_counts_df <- bind_rows(word_counts, .id = "document")
  if (bigrams == TRUE) {
    dtm <- word_counts_df %>% cast_dtm(document = "document", term = "bigram", value = "n")
  }
  else {
    dtm <- word_counts_df %>% cast_dtm(document = "word", term = "word", value = "n")
  }
  
  seeds_list <- list(1234, 5678, 91011, 1213, 1415)
  lda_model <- LDA(dtm, k = k, method = "Gibbs", control = list(nstart = 5, seed = seeds_list))  
  
  dtm_sparse <- as(as(dtm, "matrix"), "dgCMatrix")
  beta_matrix <- exp(lda_model@beta)
  colnames(beta_matrix) <- colnames(dtm_sparse)
  coherence <- CalcProbCoherence(beta_matrix, dtm_sparse)
  
  result <- list(lda_model, coherence)
  return(result)
}

LDA_plot <- function(model) {
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