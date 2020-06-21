PdfToDatSmpl <- function(indata) {
  if(is.null(indata)) {return(NULL)} else {
    file <- indata
    lst <- pdftools::pdf_text(file[['datapath']])
    lst <- readr::read_lines(lst)
    s <- stringr::str_which(lst,"Group")
    e <- stringr::str_which(lst,"Reported")
    start <- s[[1]] +2
    end <- e[[4]] -1
    tbl <- data.frame()
    tbl <- readr::read_table2(lst[start:end], col_names = FALSE)
    tbl2 <- dplyr::select(tbl, 13, 14, 15)
    names(tbl2) <- c("Area", "Amount", "Concentration")
    return(tbl2)
  }
}