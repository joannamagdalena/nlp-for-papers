# lemmatization
text_lemmatization <- function(the_text){
  the_text_unlist <- unlist(strsplit(the_text, " "))
  lemmatized_text <- lemmatize_words(the_text_unlist)
  return(lemmatized_text)
} 

# tokenization
text_tokenization <- function(the_text){
  tokenized_text <- tibble(text = the_text) %>% unnest_tokens(word, text)
  return(tokenized_text)
} 

# removing the stop words
removing_stop_words <- function(the_text){
  stop_words <- stopwords("en") %>% tibble(word = .)
  the_text_without_stopwords <- anti_join(the_text, stop_words, by = "word")
  return(the_text_without_stopwords)
} 

# pre-processing the pdf files
preprocessing_of_pdf_files <- function(pdf_files){
  pdf_files_prepared <- character()
  for (pdf_file in pdf_files){
    pdf_file <- tolower(pdf_file)
    pdf_file <- removePunctuation(pdf_file)
    pdf_file <- removeNumbers(pdf_file)
    pdf_file <- text_lemmatization(pdf_file)
    pdf_file <- text_tokenization(pdf_file)
    pdf_file <- removing_stop_words(pdf_file)
    pdf_files_prepared <- append(pdf_files_prepared, pdf_file)
  }
  return(pdf_files_prepared)
}
