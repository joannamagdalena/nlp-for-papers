library(pdftools)
library(tm)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(tokenizers)
library(hash)
library(stringr)
library(dplyr)
library(textstem)
library(igraph)
library(ggraph)

# loading and pre-processing the pdf file
pdf_file <- pdf_text("C:/.../x.pdf")
pdf_file <- tolower(gsub("[\r\n]", " ", paste(pdf_file, collapse=" ")))
pdf_file <- removePunctuation(pdf_file)
pdf_file <- removeNumbers(pdf_file)

# lemmatization
pdf_file_unlist <- unlist(strsplit(pdf_file, " "))
pdf_file_lemmatized <- lemmatize_words(pdf_file_unlist)

# tokenization and removing the stop words, with bigrams
pdf_file_tokenized <- tibble(text = pdf_file_lemmatized) %>% unnest_tokens(word, text)
pdf_file_tokenized_bigrams <- tibble(text = pdf_file) %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)

stop_words <- stopwords("en") %>% tibble(word = .)
pdf_file_tokenized <- anti_join(pdf_file_tokenized, stop_words, by = "word")
pdf_file_tokenized_bigrams <- pdf_file_tokenized_bigrams %>% separate(bigram, c("word1", "word2"), sep = " ")
pdf_file_tokenized_bigrams <- pdf_file_tokenized_bigrams %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
pdf_file_tokenized_bigrams <- pdf_file_tokenized_bigrams %>% filter(!is.na(word1)) %>% filter(!is.na(word2))
pdf_file_tokenized_bigrams_united <- pdf_file_tokenized_bigrams %>% unite(bigram, word1, word2, sep = " ")

# frequency of words and bigrams
word_counts <- pdf_file_tokenized %>% count(word, sort = TRUE)
word_counts <- mutate(word_counts, freq = n/sum(n))
bigram_counts <- pdf_file_tokenized_bigrams_united %>% count(bigram, sort = TRUE)

words_filtered <- word_counts %>% filter(freq > 0.005 & freq <= 0.02)

# visualization (barplot)
ggplot(words_filtered, aes(x = reorder(word,-n), y = n)) + geom_bar(stat = "identity") + labs(title = "Filtered words", x = "Word", y = "Number")

# word clouds
wordcloud(words = words_filtered$word, freq = words_filtered$n, min.freq = 0.005, max.words=30, colors = brewer.pal(3, "Set2"))
wordcloud2(data = words_filtered, size = 0.8, color = "white", backgroundColor = "blue")
wordcloud2(data = bigram_counts, size = 0.8, color = "white", backgroundColor = "red")

# correlations between words
bigram_counts_sep <- pdf_file_tokenized_bigrams %>% count(word1, word2, sort = TRUE)
bigram_graph <- bigram_counts_sep %>% filter(n > 10) %>% graph_from_data_frame()
ggraph(bigram_graph, layout = "fr") + 
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = grid::arrow(type="closed", length=unit(2, "mm"))) + 
  geom_node_point() + 
  geom_node_text(aes(label=name), vjust=1, hjust=1) + 
  theme_void()