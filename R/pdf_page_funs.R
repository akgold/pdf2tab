# PDF Page Constructors, Printers, and Accessors

############################################
# Pdf Page Constructor, Getters, Setters   #
############################################

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
#'          " adsdsf           asd             asdfad")) %>%
#'          str()
pdf_page <- function(text, rows = NULL, cols = NULL) {
  l <- list(text = text,
            cols = cols,
            rows = rows)
  class(l) <- "pdf_page"
  l
}

get_page_text <- function(x) {
  get_page_attr(x, "text")
}

get_page_cols <- function(x) {
  get_page_attr(x, "cols")
}

get_page_rows <- function(x) {
  get_page_attr(x, "rows")
}

set_page_cols <- function(x, val) {
  set_page_attr(x, "cols", val)
}

set_page_rows <- function(x, val) {
  set_page_attr(x, "rows", val)
}

set_page_text <- function(x, val) {
  set_page_attr(x, "text", val)
}

get_page_attr <- function(x, which) {
  stopifnot(which %in% names(x))
  x[[which]]
}

set_page_attr <- function(x, which, val) {
  stopifnot(which %in% names(x))
  x[[which]] <- val
  x
}

###################################################
#      Printing Funs                              #
###################################################

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' pdf_page(list("a   b   c", "1   2   3"))
#' pdf_page(list("ABCDEF", "123456"), cols = c(2, 4))
#' pdf_page(list("ABCDEF", "ABCDEF", "ABCDEF", "123456"), rows = 3)
#' pdf_page(list("ABCDEF", "ABCDEF", "ABCDEF", "123456"),
#' cols = c(2, 4), rows = c(3, 4))
print.pdf_page <- function(x) {

  t <- get_page_text(x) %>%
    add_print_cols(get_page_cols(x)) %>%
    add_print_rows(get_page_rows(x), get_page_cols(x))

  cat(paste0(t, collapse = "\n"))
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
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

#' Title
#'
#' @param x
#' @param rows
#'
#' @return
#' @export
#'
#' @examples
#' add_rows(list("AB|CD|EF", "AB|CD|EF", "AB|CD|EF", "12|34|56"),
#' rows = c(3, 4))
add_rows <- function(x, rows, cols) {
  if (is.null(rows)) return(x)

  add_rows_recur(x, rows,
                 make_row(cols, row_len = purrr::map_int(x, nchar) %>% max()))
}

#' Title
#'
#' @param cols
#' @param row_len
#'
#' @return
#' @export
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

#' Title
#'
#' @param x
#' @param rows
#' @param new_row
#'
#' @return
#' @export
#'
#' @examples
#' add_rows_recur(list("ABC", "ABC"), NULL, "")
#' add_rows_recur(list("ABC", "ABC"), 1, "---")
add_rows_recur <- function(x, rows, new_row) {
  if (length(rows) == 0) return(x)

  add_rows_recur(append(x, new_row, rows[1]), rows[-1] + 1, new_row)
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
#' add_chars("ABcdEF", c(2, 4))
#' add_chars("ABcdEF", c(2, 4), 1:2)
add_chars <- function(line, cols, chars = "|") {
  line <- strsplit(line, "")[[1]]
  i <- rep("", length(line))
  i[cols] <- chars

  purrr::map2_chr(line, i, paste0) %>% paste0(collapse = "")
}