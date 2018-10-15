# PDF Page Constructors, Printers, and Accessors

############################################
# Pdf Page Constructor, Getters, Setters   #
############################################
pdf_page <- function(text,
                     rows = NULL, cols = NULL,
                     header = TRUE) {
  structure(list(text = rectangularize(stringr::str_split(text, "\n")[[1]]),
                 cols = cols,
                 rows = rows,
                 header = header),
            class = "pdf_page")
}

get_text.pdf_page <- function(x) {
  get_attr(x, "text")
}

get_cols.pdf_page <- function(x) {
  get_attr(x, "cols")
}

get_rows.pdf_page <- function(x) {
  r <- get_attr(x, "rows")
  r <- r - 1
  if (has_header(x)) r <- c(1, r)
  r
}

#' Set Cols of PDF Page
#'
#' @param x page of pdf
#' @param val cols, numeric
#'
#' @return x with cols set
#' @export
#'
#' @examples
set_cols.pdf_page <- function(x, val) {
  stopifnot(is.numeric(val))
  set_attr(x, "cols", val)
}

#' Set Rows of PDF Page
#'
#' @param x page of pdf
#' @param val rows, numeric
#'
#' @return x with rows set
#' @export
#'
#' @examples
set_rows.pdf_page <- function(x, val) {
  stopifnot(is.numeric(val))
  set_attr(x, "rows", val)
}

set_text.pdf_page <- function(x, val) {
  set_attr(x, "text", val)
}

get_attr.pdf_page <- function(x, which) {
  stopifnot(which %in% names(x))
  x[[which]]
}

set_attr.pdf_page <- function(x, which, val) {
  stopifnot(which %in% names(x))
  x[[which]] <- val
  x
}

rectangularize <- function(x) {
  stringr::str_pad(x, max(nchar(x)), side = "right")
}

has_header.pdf_page <- function(x) {
  get_attr(x, "header")
}

set_header.pdf_page <- function(x, val) {
  stopifnot(is.logical(val))

  set_attr(x, "header", val)
}

###################################################
#      Printing Funs                              #
###################################################
print.pdf_page <- function(x, ...) {
  x <- add_print_cols(x)
  x <- add_print_rows(x)

  print(get_text(x))

  invisible(x)
}

add_print_cols <- function(x) {
  if (is.null(get_cols(x))) return(x)

  split_text <- split_cols(get_text(x), get_cols(x))
  # Add row with numbers at end
  blank_row <- stringr::str_replace_all(split_text[[1]], "\\w", " ")
  blank_row <- paste0(blank_row, sep = c(seq(length(blank_row) - 1), ""))

  set_text(x, c(purrr::map_chr(split_text, paste0, collapse = "|"),
                paste0(blank_row, collapse = "")))
}


add_print_rows <- function(x) {
  if (is.null(get_rows(x))) return(x)

  p <- rep("", length(get_text(x)))
  p[get_rows(x) - 1] <- make_row_chars(x)
  t <- purrr::map2(get_text(x), p, function(x, y) c(x, y))
  t <- purrr::discard(unlist(t), function(x) x == "")
  set_text(x, t)
}

make_row_chars <- function(x) {
  r <- rep("-", nchar(get_text(x)[1]))
  r[get_cols(x) + seq(length(get_cols(x))) - 1] <- "+"
  paste0(r, collapse = "")
}

#' @export
#' @rdname drop_lines
drop_lines.pdf_page <- function(x, lines, from) {
  if (is.null(lines)) return(x)

  text <- get_text(x)
  if (from == "bottom") lines <- seq(length(text) - lines + 1, length(text))
  set_text(x, text[-lines])
}

as.data.frame.pdf_page <- function(x, row.names = NULL, optional = FALSE, ...) {
  text <- split_cols(get_text(x), get_cols(x))
  text <- join_rows(text, get_rows(x))

  m <- matrix(unlist(text), ncol = length(text[[1]]), byrow = TRUE)

  df <- dplyr::as_tibble(m)

  if(has_header(x)) {
   names(df) <- df[1,]
   df <- df[-1,]
  }

  df
}

split_cols <- function(page, cols) {
  purrr::map(page,
             ~ stringr::str_sub(.,
                                c(1, cols),
                                c(cols - 1, nchar(.))))
}

join_rows <- function(page, rows) {
  purrr::map2(c(1, rows + 1), c(rows, length(page)),
              function(start, end) join_row(page[seq(start, end)]))
}

join_row <- function(clump) {
  x <- matrix(unlist(clump), nrow = length(clump), byrow = TRUE)
  x <- apply(x, 2, function(y) paste0(y, collapse = ""))
  stringr::str_squish(x)
}