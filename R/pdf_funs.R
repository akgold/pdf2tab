# Constructors and Functions for Dealing with Full pdfs


#' Title
#'
#' @param filename
#' @param text
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' pdf_load(text = c("ABC\nDEF\nGHI", "abc\ndef\nghi"))
pdf_load <- function(filename = NULL, text = NULL,
                     pages = NULL,
                     drop_from_top = 0, drop_from_bottom = 0,
                     ...) {
  if (all(!is.null(filename), !is.null(text))) {
    stop("Only one of filename and text should be supplied.")
  }

  if (!is.null(filename)) text <- pdftools::pdf_text(filename, ...)

  if (is.null(pages)) {
    pages <- seq(length(text))
  }

  # split text and newline, drop extra lines and keep only needed pages
  p <- pdf(text) %>%
    split_text() %>%
    drop_lines(drop_from_top, drop_from_bottom) %>%
    keep_pages(pages)

  pdf(text)
}

#' Title
#'
#' @param pdf
#' @param by_char
#'
#' @return
#' @export
#'
#' @examples
#' pdf(c("ABC\nDEF\nGHI", "abc\ndef\nghi")) %>% split_text()
split_text <- function(pdf, by_char = "\n") {
  stopifnot(class(pdf) == "pdf")
  stringr::str_split(get_pdf_text(pdf) %>% unlist(), by_char) %>%
    lapply()
}

set_pdf_text <- function(pdf, text) {
  pdf$text <- text
  pdf
}

get_pdf_text <- function(pdf) {
  lapply(pdf$pages, get_page_text)
}

get_n_col <- function(pdf) {
  pdf$n_col
}

#' Title
#'
#' @param text
#' @param n_col
#'
#' @return
#' @export
#'
#' @examples
#' pdf(c("ABC\nDEF\nGHI", "abc\ndef\nghi"))
pdf <- function(text, n_col = NULL) {
  p <- list(pages = lapply(text, pdf_page),
            n_col = NULL)
  class(p) <- "pdf"
  p
}

#' Title
#'
#' @param pdf
#'
#' @return
#'
#' @examples
#' pdf_load(text = c("ABC\nDEF\nGHI", "abc\ndef\nghi"))
print.pdf <- function(x, pages = 1) {
  cat(sprintf("\n\nPDF of %s pages.\n\n", length(x$text)))

  for (i in pages) {
    cat(sprintf("\nPAGE %s\n\n", i))
    print(get_page(x, i))
  }

  invisible(x)
}

get_page <- function(x, i) {
  x$text[[i]]
}



#' Title
#'
#' @param pdf
#' @param drop_from_top
#' @param drop_from_bottom
#'
#' @return
#' @export
#'
#' @examples
drop_lines <- function(pdf, drop_from_top, drop_from_bottom) {
  purrr::map(pdf, function(page) page[-c(drop_from_top,
                                         seq(length(page) - drop_from_bottom,
                                             length(page)))])
}



