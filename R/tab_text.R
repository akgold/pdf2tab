# Dealing with single tables




#' Title
#'
#' @param tab_text
#'
#' @return
#' @export
#'
#' @examples
split_cols <- function(tab_text) {
  starts <- c(1, tab_text$cols)
  ends <- c(tab_text$cols, tab_text$nchar)

  p <- purrr::map(tab_text$text, function(x) substring(x, starts, ends))
  matrix(unlist(p), nrow = length(p), byrow = T)
}

#' Title
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
#' @param tab
#'
#' @return
#' @export
#'
#' @examples
print.tab_text <- function(tab) {
  stopifnot("tab_text" %in% class(tab))
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
  if (!is.null(rows) & !is.null(cols)) proc <- fix_corners(proc)
  if (!is.null(cols)) proc <- add_col_breaks(proc, tab$nchar + length(cols) * 2)
  if (!is.null(rows)) proc <- add_row_breaks(proc, rows)

  cat(paste0(c(" ", proc), sep = "\n"))
}

#' Title
#'
#' @param tab
#'
#' @return
#' @export
#'
#' @examples
fix_corners <- function(tab) {
  rows <- which(stringr::str_replace_all(tab[-length(tab)], "-", "") == "")
  cols <- which(strsplit(tab[1], "")[[1]] == "|")

  for (col in cols) {
    substr(tab[rows], col, col + 1) <- "+"
  }

  tab
}

#' Title
#'
#' @param tab
#' @param rows
#' @param row_length
#'
#' @return
#' @export
#'
#' @examples
split_rows <- function(tab, rows, row_length) {
  purrr::map2(c(1, rows), c(rows -1, length(tab)),
              function(x, y) tab[seq(x, y)] %>%
                c(paste0(rep("-", row_length), collapse = ""))) %>%
    unlist()
}

#' Title
#'
#' @param tab
#' @param len
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param tab
#' @param rows
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param tab
#'
#' @return
#' @export
#'
#' @examples
get_chars <- function(tab) {
  vapply(tab, nchar, numeric(1))
}