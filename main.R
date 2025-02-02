library(pdftools)
library(tm)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(tokenizers)
library(hash)
library(stringr)
library(dplyr)

# loading and pre-processing the pdf file
pdf_file <- pdf_text("C:/.../x.pdf")
pdf_file <- tolower(gsub("[\r\n]", " ", paste(pdf_file, collapse=" ")))
pdf_file <- removePunctuation(pdf_file)
pdf_file <- removeNumbers(pdf_file)

# tokenization and removing the stop words
pdf_file_tokenized <- tibble(text = pdf_file) %>% unnest_tokens(word, text)

stop_words <- stopwords("en") %>% tibble(word = .)
pdf_file_tokenized <- anti_join(pdf_file_tokenized, stop_words, by = "word")

# frequency of words
word_counts <- pdf_file_tokenized %>% count(word, sort = TRUE)
word_counts <- mutate(word_counts, freq = n/sum(n))

words_filtered <- word_counts %>% filter(freq > 0.005 & freq <= 0.02)

# visualization (barplot)
ggplot(words_filtered, aes(x = reorder(word,-n), y = n)) + geom_bar(stat = "identity") + labs(title = "Filtered words", x = "Word", y = "Number")

