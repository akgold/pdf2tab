pdf_load <- function(filename, ...) {
  text <- pdftools::pdf_text(filename, ...)

  text <- lapply(text, sep_lines)

  p <- list(text = text)
  class(p) <- "pdf"
  p
}

tab_page <- function(text,
                     drop_from_top = 1,
                     drop_from_bottom = 1,
                     has_names = TRUE) {

  text <- text[-c(seq(1, drop_from_top),
                  seq(length(text) - drop_from_bottom,
                      length(text)))]

  l <- list(text = text,
            cols = NULL,
            rows = NULL,
            drop_from_top = drop_from_top,
            drop_from_bottom = drop_from_bottom,
            has_names = has_names,
            nchar = max(vapply(text, nchar, numeric(1))))
  l$text <- rectangularize(l)

  class(l) <- "tab_page"
  l
}



sep_lines <- function(page) {
  strsplit(page, "\n")[[1]]
}

pdf_n_pages <- function(pdf) {
  stopifnot("pdf" %in% class(pdf))
  length(pdf$text)
}

extract_tab_pages <- function(pdf, page_nos = seq(pdf_n_pages(pdf))) {
  pdf$text <- pdf$text[page_nos]
  pdf
}

split_cols <- function(tab_page) {
  starts <- c(1, tab_page$cols)
  ends <- c(tab_page$cols, tab_page$nchar)

  p <- purrr::map(tab_page$text, function(x) substring(x, starts, ends))
  matrix(unlist(p), nrow = length(p), byrow = T)
}

#'
#' @param pdf
#' @param drop_first
#' @param drop_last
#'
#' @examples
#' drop_lines(tab$text)
drop_lines <- function(pdf, drop_first = 1, drop_last = 1) {
  l <- length(pdf)
  purrr::map(pdf, function(x) {
    l <- length(x)
    x[-c(seq(drop_first), seq(l - drop_last, l))]
  })
}


#' Title
#'
#' @param line
#' @param blank_chars
#'
#' @return
#' @export
#'
#' @examples
#' line <- "NAME   ORIGIN  FORM        CODES                       DESCRIPTION"
#' get_blanks(line)
get_blanks <- function(line, blank_chars = " ") {
  which(strsplit(line, "")[[1]] %in% blank_chars)
}


#' Title
#'
#' @param pdf_page
#' @param blank_chars
#'
#' @return
#' @export
#'
#' @examples
#' blanks <- get_breaks(tab$text[[1]])
suggest_cols <- function(tab, blank_chars = " ") {
  text <- tab$text
  chars <- tab$nchar

  tab$cols <- purrr::map(
    tab$text,
    function(x) get_blanks(x, blank_chars = blank_chars)) %>%
    purrr::reduce(intersect)

  tab
}

get_new_rows <- function(df) {
  has_dat <- as.data.frame(purrr::map(df, get_full_row))
  r <- rowSums(len)
  which(r == max(r))
}

get_full_row <- function(col) {
  col <- stringr::str_replace_all(col, " ", "")
  col != ""
}

suggest_rows <- function(tab) {
  text <- split_cols(tab)
  nrow <- nrow(text)
  text <- stringr::str_trim(text) %>%
    matrix(nrow = nrow)

  r <- rowSums(text != "")
  tab$rows <- which(r == max(r))
  tab$rows <- tab$rows[tab$rows != 1]
  tab
}

get_chars <- function(tab) {
  vapply(tab, nchar, numeric(1))
}

rectangularize <- function(tab) {
  purrr::map(tab$text, function(x) stringr::str_pad(x, tab$nchar, "right")) %>%
    unlist()
}

print.tab_page <- function(tab) {
  stopifnot("tab_page" %in% class(tab))
  cols <- tab$cols
  proc <- tab$text
  rows <- tab$rows

  # Add column breaks
  if (!is.null(cols)) {
    proc <- split_cols(tab)
    proc <- apply(proc, 1, function(x) paste0(x, collapse = "|"))
  }

  if (!is.null(rows)) {
    proc <- split_rows(proc, rows, tab$nchar + length(cols) * 2)

  }

  # Number column and row breaks
  proc <- fix_corners(proc)
  proc <- add_col_breaks(proc, tab$nchar + length(cols) * 2)
  proc <- add_row_breaks(proc, rows)

  print(proc)
}

fix_corners <- function(tab) {
  rows <- which(stringr::str_replace_all(tab[-length(tab)], "-", "") == "")
  cols <- which(strsplit(tab[1], "")[[1]] == "|")

  for (col in cols) {
    substr(tab[rows], col, col + 1) <- "+"
  }

  tab
}

split_rows <- function(tab, rows, row_length) {
  purrr::map2(c(1, rows), c(rows -1, length(tab)),
              function(x, y) tab[seq(x, y)] %>%
                c(paste0(rep("-", row_length), collapse = ""))) %>%
    unlist()
}

add_col_breaks <- function(tab, len) {
  col_breaks <- number_col_breaks(tab[1])
  c(tab, "COL NUMBERS:", col_breaks)
}



#' Title
#'
#' @param row
#'
#' @return
#' @export
#'
#' @examples
#' number_col_breaks("     NAME      | ORIGIN  | FORM |                  CODES                 |              DESCRIPTION                   ")
number_col_breaks <- function(row) {
  row <- stringr::str_replace_all(row, "[^\\|]", " ")
  for (i in seq(stringr::str_count(row, "\\|"))) {
    row <- stringr::str_replace(row, "\\|", as.character(i))
  }
  row
}

add_row_breaks <- function(tab, rows) {
  tab <- paste0(rep(" |", length(tab)), tab)
  for (i in seq(length(rows))) {
    row <- rows[i]
    substr(tab[row + i - 1], 1, 1) <- as.character(i)
  }
  tab
}

#' Title
#'
#' @param breaks
#'
#' @return
#' @export
#'
#' @examples
#' break_no_row(c(2, 4, 6))
break_no_row <- function(breaks) {
  cols <- get_col_lengths(breaks) + seq(length(breaks))
  lapply(cols, function(x) rep(" ", x) %>% paste0(collapse = "")) %>%
    paste0(seq(length(breaks))) %>%
    paste0(collapse = "")
}


#' Title
#'
#' @param breaks
#'
#' @return
#' @export
#'
#' @examples
#' get_col_lengths(c(2, 4, 6))
get_col_lengths <- function(breaks) {
  breaks <- c(0, breaks)
  purrr::map2(breaks[-length(breaks)], breaks[-1], function(x, y) y - x - 1) %>%
    unlist()
}