# PDF Page Constructors, Printers, and Accessors

############################################
# Pdf Page Constructor, Getters, Setters   #
############################################

#' Create a PDF Page
#'
#' @param text Text of page
#' @param cols numeric, characters preceding column breaks
#' @param rows numeric, rows preceding row breaks
#'
#' @return a pdf_page object
#'
#' @examples
#' str(pdf_page(list("HEADER 1         HEADER 2         HEADER 3",
#'          " adsdsf           asd             asdfad")))
pdf_page <- function(text, rows = NULL, cols = NULL) {
  l <- list(text = text,
            cols = cols,
            rows = rows)
  class(l) <- "pdf_page"
  l
}

#' Get Text from Page
#'
#' @inheritParams get_page_attr
#'
#' @return character, text of x
#'
#' @examples
#' p <- pdf_page("ABC", 1, 2)
#' get_page_text(p)
get_page_text <- function(x) {
  get_page_attr(x, "text")
}

#' Get PDF Page Columns
#'
#' @inheritParams get_page_attr
#'
#' @return numeric, columns of x
#'
#' @examples
#' p <- pdf_page("ABC", 1, 2)
#' get_page_cols(p)
get_page_cols <- function(x) {
  get_page_attr(x, "cols")
}

#' Get PDF Page Rows
#'
#' @inheritParams get_page_attr
#'
#' @return numeric, rows of x
#'
#' @examples
#' p <- pdf_page("ABC", 1, 2)
#' get_page_rows(p)
get_page_rows <- function(x) {
  get_page_attr(x, "rows")
}

#' Set Text for Page
#'
#' @inheritParams set_page_attr
#'
#' @return pdf_page
#'
#' @examples
#' p <- pdf_page("ABC", 1, 2)
#' set_page_text(p, "DEF")
set_page_cols <- function(x, val) {
  set_page_attr(x, "cols", val)
}

#' Set Rows for Page
#'
#' @inheritParams set_page_attr
#'
#' @return pdf_page
#'
#' @examples
#' p <- pdf_page("ABC", 1, 2)
#' set_page_rows(p, 3)
set_page_rows <- function(x, val) {
  set_page_attr(x, "rows", val)
}

#' Set Text for Page
#'
#' @inheritParams set_page_attr
#'
#' @return pdf_page
#'
#' @examples
#' p <- pdf_page("ABC", 1, 2)
#' set_page_text(p, "DEF")
set_page_text <- function(x, val) {
  set_page_attr(x, "text", val)
}

#' Get Attribute from Page
#'
#' @param x pdf_age
#' @param which attribute
#'
#' @return attribute of x
#'
#' @examples
#' p <- pdf_page("ABC", 1, 2)
#' get_page_attr(p, "text")
get_page_attr <- function(x, which) {
  stopifnot(which %in% names(x))
  x[[which]]
}

#' Set Attribute for Page
#'
#' @param x pdf_age
#' @param which attribute
#' @param val value
#'
#' @return attribute of x
#'
#' @examples
#' p <- pdf_page("ABC", 1, 2)
#' set_page_attr(p, "text", "DEF")
set_page_attr <- function(x, which, val) {
  stopifnot(which %in% names(x))
  x[[which]] <- val
  x
}

###################################################
#      Printing Funs                              #
###################################################

#' Print a PDF page
#'
#' @param x pdf_page
#' @param ... nothing right now
#'
#' @return x (invisibly)
#'
#' @examples
#' pdf_page(list("a   b   c", "1   2   3"))
#' pdf_page(list("ABCDEF", "123456"), cols = c(2, 4))
#' pdf_page(list("ABCDEF", "ABCDEF", "ABCDEF", "123456"), rows = 3)
#' pdf_page(list("ABCDEF", "ABCDEF", "ABCDEF", "123456"),
#' cols = c(2, 4), rows = c(3, 4))
print.pdf_page <- function(x, ...) {

  t <- get_page_text(x)
  t <- add_print_cols(t, get_page_cols(x))
  t <- add_print_rows(t, get_page_rows(x), get_page_cols(x))

  cat(paste0(t, collapse = "\n"))

  invisible(x)
}

#' Add column breaks
#'
#' @param x list of characters
#' @param cols column breaks
#'
#' @return x with column break characters added
#'
#' @examples
#' t <- pdf_page(list("123456789123456789",
#'          "123456789123456789"),
#'          cols = c(3, 6, 15))
#' add_print_cols(get_page_text(t), get_page_cols(t))
#' add_print_cols(get_page_text(t), NULL)
add_print_cols <- function(x, cols) {
  if (is.null(cols)) return(x)

  c(purrr::map_chr(x, add_chars, cols = cols),
    add_chars(paste0(rep(" ", max(cols + 1)), collapse = ""),
              cols = cols,
              chars = seq(length(cols))))
}

#' Add Rows to Text
#'
#' @param x list of characters
#' @param rows where to add row breaks
#' @param cols column breaks
#'
#' @return x with row break character strings added
#'
#' @examples
#' add_rows(list("AB|CD|EF", "AB|CD|EF", "AB|CD|EF", "12|34|56"),
#' rows = c(3, 4))
add_print_rows <- function(x, rows, cols) {
  if (is.null(rows)) return(x)

  add_rows_recur(x, rows,
                 make_row(cols, row_len = max(purrr::map_int(x, nchar))))
}

#' Make filler row
#'
#' @param cols location of col breaks
#' @param row_len total length of row
#'
#' @return character of length 1, filler row
#'
#' @examples
#' make_row(c(2, 4), 6)
make_row <- function(cols, row_len) {
  r <- rep("-", row_len)

  if (!is.null(cols)) {
    cols <- cols + which(cols == cols)
    r[cols] <- "+"
  }
  paste0(r, collapse = "")
}

#' Recursively add Rows
#'
#' @param x list of characters
#' @param rows where to add
#' @param new_row character, value for new row
#'
#' @return x with new row added after rows
#'
#' @examples
#' add_rows_recur(list("ABC", "ABC"), NULL, "")
#' add_rows_recur(list("ABC", "ABC"), 1, "---")
add_rows_recur <- function(x, rows, new_row) {
  if (length(rows) == 0) return(x)

  add_rows_recur(append(x, new_row, rows[1]), rows[-1] + 1, new_row)
}

#' Add Characters After Specific Number
#'
#' @param line character of length 1
#' @param chars characters to add, length 1 or same as cols
#' @param cols characters preceding those to add
#'
#' @return character, col breaks added after cols
#'
#' @examples
#' add_chars("ABcdEF", c(2, 4))
#' add_chars("ABcdEF", c(2, 4), 1:2)
add_chars <- function(line, cols, chars = "|") {
  stopifnot(typeof(line) == "character" & length(line) == 1)
  stopifnot(length(chars %in% c(1, length(cols))))

  # split line and create col line to merge with
  line <- strsplit(line, "")[[1]]
  i <- rep("", length(line))
  i[cols] <- chars

  # zip together
  paste0(purrr::map2_chr(line, i, paste0), collapse = "")
}
