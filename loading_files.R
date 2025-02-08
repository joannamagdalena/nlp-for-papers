# loading pdf files
loading_pdf_files <- function(paths){
  pdf_files <- character()
  for (path in paths){
    x <- pdf_text(path)
    x <- gsub("[\r\n]", " ", paste(x, collapse=" "))
    pdf_files <- append(pdf_files, x) 
  }
  return(pdf_files)
}
