context("Check pdf paging works right.")
library(testthat)

p <- pdf_page(c("ABC", "123"), 1, 2)

test_that("pdf_page constructors, getters, setters work right", {
  expect_equal(get_page_text(p), c("ABC", "123"))
  expect_equal(get_page_cols(p), 2)
  expect_equal(get_page_rows(p), 1)
  expect_equal(get_page_attr(p, "rows"), get_page_rows(p))
  expect_equal(set_page_cols(p, 3) %>% get_page_cols(), 3)
  expect_equal(set_page_attr(p, "rows", 3) %>% get_page_rows(), 3)
})

test_that("Page printers work right", {
  expect_equal(add_chars("ABcdEF", c(2, 4)), "AB|cd|EF")
  expect_equal(add_chars("ABcdEF", c(2, 4), 1:2), "AB1cd2EF")
  expect_equal(make_row(c(2, 4), 8), "--+--+--")
  expect_equal(add_rows("ABC", NULL, NULL), "ABC")
  expect_equal(add_rows(list("AB|CD|EF", "AB|CD|EF", "AB|CD|EF"), 1:2, c(2, 4)),
               list("AB|CD|EF", "--+--+--", "AB|CD|EF", "--+--+--", "AB|CD|EF"))
  expect_equal(add_rows_recur(list("ABC", "ABC"), 1, "---"),
               list("ABC", "---", "ABC"))
  expect_equal(add_rows_recur(list("ABC", "ABC"), NULL, ""),
               list("ABC", "ABC"))
  expect_equal(add_print_cols(get_page_text(p), NULL),
               c("ABC", "123"))
  expect_equal(add_print_cols(get_page_text(p), 2), c("AB|C", "12|3", "  1 "))
})