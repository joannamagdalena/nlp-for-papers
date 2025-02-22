library(pdftools)
library(tm)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(textstem)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(widyr)
library(ggraph)
library(igraph)
library(tibble)

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

# examples of word clouds for unigrams (from the first paper in the dataset)
wordcloud(words = word_counts[[1]]$word, freq = word_counts[[1]]$n, min.freq = 0.005, max.words=40, colors = brewer.pal(3, "Set2"))
wordcloud2(data = word_counts[[1]], size = 0.8, color = "white", backgroundColor = "blue")


# frequency of bigrams
bigram_counts <- list()
for (i in 1:length(preprocessed_pdf_files_bigrams)){
  bigram_counts[[i]] <- tibble(text = preprocessed_pdf_files_bigrams[[i]]) %>% 
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
    count(bigram, sort = TRUE)
}

# example of a word cloud for bigrams (from the first paper in the dataset)
wordcloud2(data = bigram_counts[[1]], size = 0.8, color = "white", backgroundColor = "red")

# correlation - bigrams (for the first paper in the dataset)
bigram_cor <- list()
for (i in 1:length(preprocessed_pdf_files_bigrams)){
  preprocessed_pdf_files_bigrams_sep <- tibble(bigram = preprocessed_pdf_files_bigrams[[i]]) %>% 
                                        separate(bigram, c("word1", "word2"), sep = " ")
  bigram_cor[[i]] <- preprocessed_pdf_files_bigrams_sep %>% 
                    pairwise_cor(word1, word2)
}
bigram_graph_list <- list() 
for (i in 1:length(bigram_cor)){
  bigram_graph_list[[i]] <- bigram_cor[[i]] %>% 
                            filter(correlation > 0.8) %>%
                            graph_from_data_frame()
}
ggraph(bigram_graph_list[[1]], layout = "fr") + 
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE, arrow = grid::arrow(type="closed", length=unit(2, "mm"))) + 
  geom_node_point() + 
  geom_node_text(aes(label=name), vjust=1, hjust=1) + 
  theme_void()


### LDA + coherence score
source("LDA.R")
k <- 5

LDA_unigrams <- LDA_modeling(word_counts, k, FALSE)
LDA_plot(LDA_unigrams[1])
print(LDA_unigrams[2])

LDA_bigrams <- LDA_modeling(bigram_counts, k, TRUE)
LDA_plot(LDA_bigrams[1])
print(LDA_bigrams[2])

