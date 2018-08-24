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

pdf_tab <- function(text, n_col = NULL) {
  list(text = lapply(text, pdf_page),
       n_col = NULL,
       class = "pdf")
}

#' Title
#'
#' @param page
#' @param cols
#' @param rows
#'
#' @return
#' @export
#'
#' @examples
#' pdf_page(list("HEADER 1         HEADER 2         HEADER 3",
#'          " adsdsf           asd             asdfad"))
pdf_page <- function(page, cols = NULL, rows = NULL) {
  l <- list(page = page,
       cols = cols,
       rows = rows)
  class(l) <- "pdf_page"
  l
}

print.pdf_page <- function(x) {

  if (!is.null(x$cols)) {
    insert_cols(x)
  }

  if (!is.null(x$rows)) {
    insert_rows(x)
  }

  cat(paste0(x$page, collapse = "\n"))
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' pdf_page(list("123456789123456789",
#'          "123456789123456789"),
#'          cols = c(3, 6, 15)) %>%
#' insert_cols()
insert_cols <- function(x) {
  stopifnot(class(x) == "pdf_page")

  # Increment later cols to account for earlier separators inserted
  cols <- x$cols + which(x$cols == x$cols) - 1

  x$page <- c(lapply(x$page, add_chars, cols = cols),
    add_chars(paste0(rep(" ", max(cols + 1)), collapse = ""),
              cols,
              seq(length(cols))))
  x
}

#' Title
#'
#' @param line
#' @param cols
#' @param char
#'
#' @return
#' @export
#'
#' @examples
#' add_chars("ABcdEF", c(2, 5))
#' add_chars("ABcdEF", c(2, 5), 1:2)
add_chars <- function(line, cols, chars = "|") {
  if (length(cols) == 0) {
    return(line)
  }

  if (length(chars) < length(cols)) {
    chars <- rep(chars, length(cols))
  }

  # insert next col character
  line <- paste0(substr(line, 1, cols[1]),
                 chars[1],
                 substr(line, cols[1] + 1, nchar(line)))

  # recurse, removing first col to add
  add_chars(line, cols[-1], chars[-1])
}



drop_lines <- function(pdf, drop_from_top, drop_from_bottom) {
  purrr::map(pdf, function(page) page[-c(drop_from_top,
                                         seq(length(page) - drop_from_bottom,
                                             length(page)))])
}

#' Title
#'
#' @param pdf
#'
#' @return
#'
#' @examples
#' pdf_load(text = c("ABC\nDEF\nGHI", "abc\ndef\nghi"))
print.pdf <- function(pdf) {
  cat(sprintf("\n\nPDF of %s pages\n\nPage 1:\n\n",
              length(pdf)))
  cat(paste0(pdf[[1]], collapse = "\n"))
  invisible(pdf)
}
