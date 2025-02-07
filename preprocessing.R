# pre-processing the pdf files
preprocessing_of_pdf_files <- function(pdf_files){
  pdf_files_prepared <- character()
  for (pdf_file in pdf_files){
    pdf_file <- tolower(pdf_file)
    pdf_file <- removePunctuation(pdf_file)
    pdf_file <- removeNumbers(pdf_file)
    pdf_files_prepared <- append(pdf_files_prepared, pdf_file)
  }
  return(pdf_files_prepared)
}
