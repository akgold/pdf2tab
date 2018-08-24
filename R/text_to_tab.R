# Functions for turning pdf text into tables

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
#' get_blanks(line, "\\s")
get_blanks <- function(line, blank_chars = "\\s") {
  stringr::str_locate_all(line, blank_chars)[[1]][,1]
}


find_max_blanks <- function(page, header, n_col) {
  blanks <- purrr::map(page, ~ get_blanks(.) %>% get_outer())


  tibble::tibble(char_no = unlist(blanks)) %>%
    dplyr::count(char_no) %>%
    dplyr::top_n(n_col, n) %>%
    pull(char_no)
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
suggest_cols <- function(tab, n_col, header = TRUE) {
  # make all lines same length
  tab <- rectangularize(tab)

  # get blank chars in cols and add across rows
  col_edges <- get_col_edges(tab, header, n_col)

  # if consecutive, choose rightmost column to separate cols




 c <- purrr::map(
    tab$text,
    function(x) get_blanks(x, blank_chars = blank_chars))

  tab$cols <- data.frame(cols = unlist(c)) %>%
    count(cols) %>%
    filter(n/length(text) > wgt) %>%
    pull(cols)

  tab
}
#' Title
#'
#' @param tab
#'
#' @return
#' @export
#'
#' @examples
#' rectangularize(list(c("j12j 123k123k123 k123", "q23asd", "adadasff")))
rectangularize <- function(page) {
  page <- stringr::str_trim(page, "right")
  chars <- max(nchar(page))

  purrr::map(page, ~ stringr::str_pad(., chars, "right")) %>%
    unlist()
}


get_outer <- function(x) {
  x[!(dplyr::lag(x) == x - 1 & dplyr::lead(x) == x + 1)] %>%
    purrr::keep(!is.na(.))
}

##############################################3
# OLD CODE BELOW HERE                         #
#################################################
#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
get_new_rows <- function(df) {
  has_dat <- as.data.frame(purrr::map(df, get_full_row))
  r <- rowSums(len)
  which(r == max(r))
}

#' Title
#'
#' @param col
#'
#' @return
#' @export
#'
#' @examples
get_full_row <- function(col) {
  col <- stringr::str_replace_all(col, " ", "")
  col != ""
}

#' Title
#'
#' @param tab
#'
#' @return
#' @export
#'
#' @examples
suggest_rows <- function(tab, cols = NULL, min_blank = 1) {
  text <- split_cols(tab)
  nrow <- nrow(text)
  text <- stringr::str_trim(text) %>%
    matrix(nrow = nrow)

  if (is.null(cols)) {
    cols <- seq(ncol(text))
  }

  text <- text[seq(nrow(text)), cols]

  r <- as.tibble(text) %>%
    mutate_all(nr_detect) %>%
    rowSums()
  tab$rows <- which(r >= min_blank)
  tab
}

nr_detect <- function(x) {
  x != ""
}

#' Title
#'
#' @param tab
#' @param rem_breaks
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
#' t <- tab_text(c("", "A B C D", "1 2 3 4", "5 6 7 8", ""))
#' t <- suggest_cols(t)
#' t <- suggest_rows(t)
#' remove_breaks(t, list(1, 3))
remove_breaks <- function(tab,
                          rem_breaks = NULL, verbose = TRUE) {
  stopifnot("tab_text" %in% class(tab))
  stopifnot(length(rem_breaks) == 2)

  dims <- c(rows = 1, cols = 2)

  for (n in names(dims)) {
    tab[[n]] <- rem_break(tab, n, rem_breaks[[dims[n]]], verbose)
  }

  if (verbose) {
    print(tab)
  }

  tab
}

remove_cols <- function(tab, cols, ...) {
  remove_breaks(tab, list(NULL, cols), ...)
}

remove_rows <- function(tab, rows, ...) {
  remove_breaks(tab, list(rows, NULL), ...)
}


#' Title
#'
#' @param tab
#' @param name
#' @param breaks
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
#' t <- tab_text(c("", "A B C D", "1 2 3 4", ""))
#' t <- suggest_cols(t)
#' rem_break(t, "cols", 3, TRUE)
rem_break <- function(tab, name, breaks, verbose = T) {
  t <- tab[[name]]
  if (!is.null(breaks)) {
    stopifnot(all(breaks <= length(t)))
    t <- t[-breaks]
    if (verbose) {
      cat(sprintf("\nRemoving %s break: %s\n", name,
                  paste(breaks, sep = ", ")))
    }
  }
  t
}

#' Title
#'
#' @param tab
#'
#' @return
#' @export
#'
#' @examples
tab_text_to_df <- function(tab) {
  t <- split_cols(tab)
  row_splits <- purrr::map2(c(1, tab$rows), c(tab$rows - 1, length(tab$text)),
                            function(x, y) seq(x, y))
  df <- lapply(seq(ncol(t)),
         function(i) purrr::map_chr(row_splits,
                                    function(y) paste0(`[`(t[, i], y),
                                                       collapse = "") %>%
                                      stringr::str_squish() %>%
                                      stringr::str_trim())) %>%
    data.frame(stringsAsFactors = FALSE)

  names(df) <- tolower(df[1, ])
  df <- df[seq(2, nrow(df)), ]
}
