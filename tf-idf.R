tf_idf_visualization <- function(files){
  files <- tibble(files)
  files <- mutate(files, file = seq(from = 1, to = length(files), by = 1))
  
  files <- files %>% unnest_tokens(word, files) %>% count(file, word, sort = TRUE)
  words_count_total <- files %>% group_by(file) %>% summarize(total = sum(n))
  files <- left_join(files, words_count_total)
  files_tf_idf <- files %>%  bind_tf_idf(word, file, n)
  
  files_tf_idf %>% group_by(file) %>% slice_max(tf_idf, n = 10) %>% ungroup() %>%  
    ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = file)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~file, ncol = 2, scales = "free") +
    labs(x = "tf-idf", y = NULL)
}
