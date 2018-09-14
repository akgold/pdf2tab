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
#' p <- pdf_load()
pdf_load <- function(pdf_file) {
 pdf_construct(pdf_file)
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

####################################################
#   Constructors, Getters, Setters                 #
####################################################
pdf_construct <- function(text,
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


get_text.pdf <- function(x) {
  x$text
}

set_text.pdf <- function(x, text) {
  x$text <- text
  x
}

get_cols.pdf <- function(x) {
  get_attr(x, "cols")
}

set_cols.pdf <- function(x, cols) {
  set_attr(x, "cols", cols)
}

get_rows.pdf <- function(x) {
  get_attr(x, "rows")
}

set_rows.pdf <- function(x, rows) {
  set_attr(x, "rows", rows)
}

get_n_pages <- function(x) {
  length(get_text(x))
}

get_attr.pdf <- function(x, which) {
  stopifnot(which %in% names(x))

  lapply(get_text(x), get_attr, which = which)
}

set_attr.pdf <- function(x, which, val) {
  stopifnot(which %in% names(x))
  text <- get_text(x)
  stopifnot(length(val) == length(text))

  text <- purrr::map2(text, val, set_attr, which = which)
  set_text(x, text)
}

has_header.pdf <- function(x) {
  lapply(x, has_header)
}

set_header.pdf <- function(x, val) {
  lapply(x, set_header, val = val)
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

################################################
#         Data Framers                         #
################################################
as.data.frame.pdf <- function(x, row.names = NULL, optional = FALSE, ...){
  dplyr::bind_rows(lapply(get_text(x), as.data.frame, ...))
}