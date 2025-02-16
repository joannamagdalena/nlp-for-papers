# loading pdf files
loading_pdf_files <- function(path){
  filenames <- list.files(path, pattern = "*.pdf", full.names = TRUE)
  pdf_files <- character()
  for (filename in filenames){
    x <- pdf_text(filename)
    x <- gsub("[\r\n]", " ", paste(x, collapse=" "))
    pdf_files <- append(pdf_files, x) 
  }
  return(pdf_files)
}
