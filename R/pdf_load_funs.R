# Constructors and Functions for Dealing with Full pdfs

#' Load PDF
#'
#' @param ... optional arguments passed to pdftools::pdf_text
#' @param pdf_file a filename of a pdf or a list of characters
#'
#' @return a pdf object
#' @export
#'
#' @examples
#' p <- pdf_load(test_pdf_text())
pdf_load <- function(pdf_file,
                     ...) {

  pdf_file <- load_file(pdf_file, ...)

  # split text and newline
 pdf(pdf_file)
}

#' Drop lines from pdf
#' @export
#' @rdname drop_lines
drop_lines.pdf <- function(x, lines, from) {
  stopifnot(from %in% c("top", "bottom"))

  text <- get_text(x)
  lines <- fmt_lines(text, lines)
  text <- purrr::map2(text, lines, function(p, l) drop_lines(p, l, from))
  set_text(x, text)
}

fmt_lines <- function(text, lines) {
  stopifnot(length(lines) %in% c(1, length(text)))

  if (length(lines) == 1) lines <- rep(lines, length(text))

  purrr::map2(text, lines,
              function(p, l) stopifnot(all(l %in% seq(length(get_text(p))))))

  lines
}

#' Keep only needed pages
#'
#' @param pdf pdf object
#' @param pages pages to keep
#'
#' @return pdf with only pages specified by pages
#' @export
#'
#' @examples
#' p <- pdf_load(test_pdf_text(pages = "all"))
#' p <- keep_pages(p, seq(7, 56))
keep_pages <- function(pdf, pages) {
  if (is.null(pages)) return(pdf)

  text <- get_text(pdf)
  stopifnot(all(pages %in% seq(length(text))))
  set_text(pdf, text[pages])
}

load_file <- function(pdf_file, ...) {
  if (length(pdf_file) == 1 && file.exists(pdf_file)) {
    pdf_file <- pdftools::pdf_text(pdf_file, ...)
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
  structure(list(text = lapply(text, pdf_page),
                 cols = check_arg(text, cols),
                 rows = check_arg(text, rows)),
            class = "pdf")
}

check_arg <- function(text, arg) {
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

print.pdf <- function(x, pages = 1) {
  cat(sprintf("\n\nPDF of %s pages.\n\n", get_n_pages(x)))

  for (i in seq(pages)) {
    cat("\n   ---------------------------------------------------\n",
        sprintf("       PAGE %s", i),
        "\n   ---------------------------------------------------\n")
    print(get_text(x)[[i]])
  }

  invisible(pdf)
}

#' Load the text of a pdf for testing
#'
#' @param which which pdf, currently just supports default
#' @param pages which pages, table starts on page 7
#'
#' @return list, each element 1 page
#' @export
#'
#' @examples
#' test_pdf_text()
test_pdf_text <- function(which = "sentencing", pages = NULL) {
  p <- open_test_pdf(which)

  if (is.null(pages)) pages <- switch(which,
                                      "sentencing" = 7)

  if (pages == "all") pages <- seq(length(p))

  p[pages]
}

open_test_pdf <- function(which = "sentencing") {
  file <- switch(which,
                 sentencing = "USSC_Public_Release_Codebook_FY99_FY16.pdf")
  file <- system.file("extdata", file, package = "pdf2tab", mustWork = TRUE)

  load_file(file)
}
