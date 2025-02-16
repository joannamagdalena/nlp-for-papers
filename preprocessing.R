# lemmatization
text_lemmatization <- function(the_text){
  the_text_unlist <- unlist(strsplit(the_text, " "))
  lemmatized_text <- lemmatize_words(the_text_unlist)
  return(lemmatized_text)
} 

# tokenization
text_tokenization <- function(the_text, bigrams){
  if (bigrams == TRUE){
    tokenized_text <- tibble(text = the_text) %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
  }
  else{
    tokenized_text <- tibble(text = the_text) %>% unnest_tokens(word, text)
  }
  return(tokenized_text)
} 

# removing the stop words
removing_stop_words <- function(the_text, bigrams){
  stop_words <- c(stopwords("en"), "et", "al", "x", "y", "i", "j", "k") %>% tibble(word = .)
  if (bigrams == TRUE){
    the_text_without_stopwords <- the_text %>% separate(bigram, c("word1", "word2"), sep = " ") %>% 
      filter(!word1 %in% stop_words$word) %>% 
      filter(!word2 %in% stop_words$word) %>% 
      filter(!is.na(word1)) %>% 
      filter(!is.na(word2)) %>% 
      unite(bigram, word1, word2, sep = " ")
  }
  else {
    the_text_without_stopwords <- anti_join(the_text, stop_words, by = "word")
  }
  return(the_text_without_stopwords)
} 

# pre-processing the pdf files
preprocessing_of_pdf_files <- function(pdf_files, bigrams){
  pdf_files_prepared <- list()
  for (pdf_file in pdf_files){
    pdf_file <- tolower(pdf_file)
    pdf_file <- removePunctuation(pdf_file)
    pdf_file <- removeNumbers(pdf_file)
    if (bigrams == FALSE){
      pdf_file <- text_lemmatization(pdf_file)
      pdf_file <- text_tokenization(pdf_file, FALSE)
      pdf_file <- removing_stop_words(pdf_file, FALSE)
    }
    else {
      pdf_file <- text_tokenization(pdf_file, TRUE)
      pdf_file <- removing_stop_words(pdf_file, TRUE)
    }
    pdf_files_prepared <- append(pdf_files_prepared, pdf_file)
  }
  return(pdf_files_prepared)
}
