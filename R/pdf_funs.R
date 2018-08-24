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
    pages <- seq(length(l))
  }

  text <- lapply(text, function(x) strsplit(x, "\n")[[1]]) %>%
    drop_lines(drop_from_top, drop_from_bottom) %>%
    `[`(pages)

  pdf_tab(text)
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
pdf_tab <- function(text, n_col = NULL) {
  p <- list(text = lapply(text, pdf_page),
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



