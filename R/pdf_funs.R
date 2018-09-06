# Constructors and Functions for Dealing with Full pdfs

#' Load PDF
#'
#' @param filename s
#' @param text s
#' @param ... s
#'
#' @return s
#' @export
#'
#' @examples
#' pdf_load(text = test_pdf_text())
pdf_load <- function(pdf_file, n_col = NULL,
                     pages = NULL,
                     drop_from_top = 0, drop_from_bottom = 0,
                     ...) {

  if (file.exists(pdf_file)) {
    pdf_file <- pdftools::pdf_text(filename, ...)
  } else {
    warning("No file found, interpreting pdf_file input as actual text.")
  }

  # split text and newline, drop extra lines and keep only needed pages
  p <- pdf(pdf_file, n_col)
  p <- keep_pages(p, pages)
  drop_lines(p, drop_from_top, drop_from_bottom)
}

set_pdf_text <- function(pdf, text) {
  pdf$text <- text
  pdf
}

set_n_col <- function(pdf, n_col) {
  pdf$n_col <- n_col
  pdf
}

get_pdf_text <- function(pdf) {
  lapply(pdf$pages, get_page_text)
}

get_n_col <- function(pdf) {
  pdf$n_col
}

#' PDF Object
#'
#' @param text pdf text
#' @param n_col number of columns in pdf
#'
#' @return
#'
#' pdf(test_pdf_text())
pdf <- function(text, n_col = NULL) {
  p <- list(pages = lapply(text, pdf_page),
            n_col = NULL)
  class(p) <- "pdf"
  p
}

#' Print a pdf object
#'
#' @param pdf pdf object
#'
#' @return pdf (invisibly)
#' pdf(test_pdf_text())
print.pdf <- function(x, pages = 1) {
  cat(sprintf("\n\nPDF of %s pages.\n\n", get_n_pages(x)))

  for (i in seq(get_n_pages(x))) {
    cat("\n---------------------------------------\n",
        sprintf("PAGE %s", i),
        "\n---------------------------------------\n")
    print(get_pages(x, i)[[1]])
  }

  invisible(pdf)
}



#' Get Pages from PDF Object
#'
#' @param x pdf object
#' @param i which pages to get
#'
#' @return
#'
#' x <- pdf(test_pdf_text())
#' get_pdf_pages(x)
#' get_pdf_pages(x, 1:2)
get_pdf_pages <- function(x, i = NULL) {
  p <- get_all_pages(x)

  if (is.null(i)) return(p)

  p[i]
}

get_all_pages <- function(x) {
  x$pages
}

get_n_pages <- function(x) {
  length(x$pages)
}


#' Sample PDF Text for Testing
#'
#' @return list of characters
#' @export
#'
#' @examples
#' test_pdf_text()
test_pdf_text <- function(which = "sentencing") {
  `[`(load_test_pdf(which),
      switch(which,
             sentencing = 7:8))
}

load_test_pdf <- function(which = "sentencing") {
  file <- here::here("inst", "extdata",
                     switch(which,
                            sentencing =
                              "USSC_Public_Release_Codebook_FY99_FY16.pdf"))
  pdftools::pdf_text(file)
}
