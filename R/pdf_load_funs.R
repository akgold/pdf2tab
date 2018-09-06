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
#' pdf_load(test_pdf_text())
pdf_load <- function(pdf_file, cols = NULL, rowss = NULL,
                     pages = NULL,
                     drop_lines = c(0, 0),
                     ...) {

  text <- load_file(pdf_file, ...)

  # split text and newline
  p <- pdf(pdf_file, cols, rowss)
  p <- keep_pages(p, pages)
  p <- drop_lines(p, drop_lines[1], "top")
  drop_lines(p, drop_lines[2], "bottom")
}



drop_lines.pdf <- function(pdf, lines, from) {
  stopifnot(from %in% c("top", "bottom"))

  text <- get_text(pdf)
  text <- lapply(text, drop_lines, lines = lines, from = from)
  set_text(pdf, text)
}

keep_pages <- function(pdf, pages) {
  if (!is.null(pages)) pdf <- select_pdf_pages(pdf, pages)

  pdf
}

load_file <- function(pdf_file, ...) {
  if (length(pdf_file) == 1 && file.exists(pdf_file)) {
    pdf_file <- pdftools::pdf_text(filename, ...)
  } else {
    warning("No file found, interpreting pdf_file input as actual text.")
  }

  pdf_file
}

####################################################
#   Constructors, Getters, Setters                 #
####################################################
pdf <- function(text,
                cols = NULL, rows = NULL) {
  cols <- check_arg_length(text, cols)
  rows <- check_arg_length(text, rows)

  p <- list(text = lapply(text, pdf_page),
            cols = cols,
            rows = rows)
  class(p) <- "pdf"
  p
}

check_arg_length <- function(text, arg) {
  if (is.null(arg)) return(arg)

  stopifnot(length(arg) %in% c(1, length(text)))

  if (length(arg) == 1) arg <- rep(arg, length(text))
  arg
}


get_text.pdf <- function(pdf) {
  get_attr(pdf, "text")
}

set_text.pdf <- function(pdf, text) {
  set_attr(pdf, "text", text)
}

get_cols.pdf <- function(pdf) {
  get_attr(pdf, "cols")
}

set_cols.pdf <- function(pdf, cols) {
  set_attr(pdf, "cols", cols)
}

get_rows.pdf <- function(pdf) {
  get_attr(pdf, "rows")
}

set_rows.pdf <- function(pdf, rows) {
  set_attr(pdf, "rows", rows)
}

get_n_pages <- function(x) {
  length(get_text(x))
}

get_attr.pdf <- function(x, which) {
  stopifnot(which %in% names(x))

  x[[which]]
}

set_attr.pdf <- function(x, which, val) {
  stopifnot(which %in% names(x))

  x[[which]] <- val
  x
}




#' Print a pdf object
#'
#' @param pdf pdf object
#'
#' @return pdf (invisibly)
#'
#' @export
#' pdf(test_pdf_text())
print.pdf <- function(x, pages = 1) {
  cat(sprintf("\n\nPDF of %s pages.\n\n", get_n_pages(x)))

  for (i in seq(get_n_pages(x))) {
    cat("\n---------------------------------------\n",
        sprintf("PAGE %s", i),
        "\n---------------------------------------\n")
    print(get_text(x))
  }

  invisible(pdf)
}

test_pdf_text <- function(which = "sentencing") {
  `[`(open_test_pdf(which),
      switch(which,
             sentencing = 7:8))
}

open_test_pdf <- function(which = "sentencing") {
  file <- here::here("inst", "extdata",
                     switch(which,
                            sentencing =
                              "USSC_Public_Release_Codebook_FY99_FY16.pdf"))
  pdftools::pdf_text(file)
}
