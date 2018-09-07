# PDF Page Constructors, Printers, and Accessors

############################################
# Pdf Page Constructor, Getters, Setters   #
############################################
pdf_page <- function(text,
                     rows = NULL, cols = NULL, split_char = "\n") {
  structure(list(text = stringr::str_split(text, split_char)[[1]],
                 cols = cols,
                 rows = rows),
            class = "pdf_page")
}

get_text.pdf_page <- function(x) {
  get_attr(x, "text")
}

get_cols.pdf_page <- function(x) {
  get_attr(x, "cols")
}

get_rows.pdf_page <- function(x) {
  get_attr(x, "rows")
}

set_cols.pdf_page <- function(x, val) {
  set_attr(x, "cols", val)
}

set_rows.pdf_page <- function(x, val) {
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

###################################################
#      Printing Funs                              #
###################################################
print.pdf_page <- function(x, ...) {
  t <- get_text(x)
  t <- add_print_cols(t, get_cols(x))
  t <- add_print_rows(t, get_rows(x), get_cols(x))

  row_ns <- get_row_ns(t)

  cat(paste(row_ns, t, sep = "| ", collapse = "\n"))

  invisible(x)
}

get_row_ns <- function(t) {
  ns <- seq(length(t))
  stringr::str_pad(ns, max(nchar(ns)), "left")
}

add_print_cols <- function(x, cols) {
  if (is.null(cols)) return(x)

  as.list(c(purrr::map_chr(x, add_chars, cols = cols),
            add_chars(paste0(rep(" ", max(cols + 1)), collapse = ""),
                      cols = cols,
                      chars = seq(length(cols)))))
}


add_print_rows <- function(x, rows, cols) {
  if (is.null(rows)) return(x)

  add_rows_recur(x, rows,
                 make_row(cols, row_len = max(purrr::map_int(x, nchar))))
}

make_row <- function(cols, row_len) {
  r <- rep("-", row_len)

  if (!is.null(cols)) {
    cols <- cols + which(cols == cols)
    r[cols] <- "+"
  }
  paste0(r, collapse = "")
}

add_rows_recur <- function(x, rows, new_row) {
  if (length(rows) == 0) return(x)

  add_rows_recur(append(x, new_row, rows[1]), rows[-1] + 1, new_row)
}

add_chars <- function(line, cols, chars = "|") {
  stopifnot(typeof(line) == "character" & length(line) == 1)
  stopifnot(length(chars) %in% c(1, length(cols)))

  # split line and create col line to merge with
  line <- strsplit(line, "")[[1]]
  i <- rep("", length(line))
  i[cols] <- chars

  # zip together
  paste0(purrr::map2_chr(line, i, paste0), collapse = "")
}

drop_lines.pdf_page <- function(page, lines, from) {
  if (is.null(lines)) return(page)

  text <- get_text(page)
  if (from == "bottom") lines <- seq(length(text) - lines + 1, length(text))
  set_text(page, text[-lines])
}
