# Define Generics That Get Dispatched Across both PDFs and PDF Pages

# Getters and setters
get_text <- function(x) UseMethod("get_text")
set_text <- function(x, text) UseMethod("set_text")
get_cols <- function(x) UseMethod("get_cols")
set_cols <- function(x, cols) UseMethod("set_cols")
get_rows <- function(x) UseMethod("get_rows")
set_rows <- function(x, rows) UseMethod("set_rows")
get_attr <- function(x, which) UseMethod("get_attr")
set_attr <- function(x, which, val) UseMethod("set_attr")
has_header <- function(x) UseMethod("has_header")
set_header <- function(x, val) UseMethod("set_header")

# Guess Cols and Rows
guess_cols <- function(x, search, n_cols, ...) UseMethod("guess_cols")
guess_rows <- function(x, search, n_rows, ...) UseMethod("guess_rows")
guess <- function(x, search, n, margin, ...) UseMethod("guess")

#' Drop lines from top and bottom of pages
#'
#' @param x an object of class pdf
#' @param lines lines to drop, either a number, or a vector of same length as number of pages in pdf
#' @param from either "top" or "bottom"
#'
#' @return pdf with lines dropped from each page
#' @export
#'
#' @examples
#' p <- pdf_load(test_pdf_text())
#' p <- drop_lines(p, 1, "top")
#' p <- drop_lines(p, 2, "bottom")
drop_lines <- function(x, lines, from) UseMethod("drop_lines")