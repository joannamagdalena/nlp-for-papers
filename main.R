library(pdftools)
library(tm)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(textstem)

source("loading_files.R")
source("preprocessing.R")

# loading PDF files
folder_path = "C:/.../"
pdf_files <- loading_pdf_files(path = folder_path)

# pre-processing the PDF files
preprocessed_pdf_files <- preprocessing_of_pdf_files(pdf_files = pdf_files, bigrams = FALSE)
preprocessed_pdf_files_bigrams <- preprocessing_of_pdf_files(pdf_files = pdf_files, bigrams = TRUE)

# frequency of words
word_counts <- list()
for (i in 1:length(preprocessed_pdf_files)){
  word_counts[[i]] <- tibble(text = preprocessed_pdf_files[[i]]) %>% 
    unnest_tokens(word, text) %>% 
    count(word, sort = TRUE)
}

# frequency of bigrams
bigram_counts <- list()
for (i in 1:length(preprocessed_pdf_files_bigrams)){
  bigram_counts[[i]] <- tibble(text = preprocessed_pdf_files_bigrams[[i]]) %>% 
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
    count(bigram, sort = TRUE)
}

# LDA
source("LDA.R")
k <- 5
LDA_modeling(word_counts, k, FALSE)
LDA_modeling(bigram_counts, k, TRUE)


######## 
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(tokenizers)
library(hash)
library(stringr)

library(igraph)
library(ggraph)
library(tibble)

words_filtered <- mutate(word_counts[[1]], freq = n/sum(n))
words_filtered <- words_filtered %>% filter(freq > 0.005 & freq <= 0.02)

# visualization (barplot)
ggplot(words_filtered, aes(x = reorder(word,-n), y = n)) + geom_bar(stat = "identity") + labs(title = "Filtered words", x = "Word", y = "Number")

# word clouds
wordcloud(words = words_filtered$word, freq = words_filtered$n, min.freq = 0.005, max.words=30, colors = brewer.pal(3, "Set2"))
wordcloud2(data = words_filtered, size = 0.8, color = "white", backgroundColor = "blue")
wordcloud2(data = bigram_counts[[1]], size = 0.8, color = "white", backgroundColor = "red")

# correlations between words
pdf_file <- pdf_text("C:/.../x.pdf")
pdf_file <- tolower(gsub("[\r\n]", " ", paste(pdf_file, collapse=" ")))
pdf_file <- removePunctuation(pdf_file)
pdf_file <- removeNumbers(pdf_file)

pdf_file_tokenized_bigrams <- tibble(text = pdf_file) %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)

stop_words <- stopwords("en") %>% tibble(word = .)
pdf_file_tokenized_bigrams <- pdf_file_tokenized_bigrams %>% separate(bigram, c("word1", "word2"), sep = " ")
pdf_file_tokenized_bigrams <- pdf_file_tokenized_bigrams %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
pdf_file_tokenized_bigrams <- pdf_file_tokenized_bigrams %>% filter(!is.na(word1)) %>% filter(!is.na(word2))

bigram_counts_sep <- pdf_file_tokenized_bigrams %>% count(word1, word2, sort = TRUE)
bigram_graph <- bigram_counts_sep %>% filter(n > 10) %>% graph_from_data_frame()
ggraph(bigram_graph, layout = "fr") + 
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = grid::arrow(type="closed", length=unit(2, "mm"))) + 
  geom_node_point() + 
  geom_node_text(aes(label=name), vjust=1, hjust=1) + 
  theme_void()