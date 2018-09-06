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

drop_lines <- function(x, lines, from) UseMethod("drop_lines")