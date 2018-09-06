library(testthat)
library(pdf2tab)
context("Check pdf paging works right.")

p <- pdf2tab:::pdf_page(c("ABC", "123"), 1, 2)

test_that("pdf_page constructors, getters, setters work right", {
  expect_equal(pdf2tab:::get_page_text(p), list("ABC", "123"))
  expect_equal(pdf2tab:::get_page_cols(p), 2)
  expect_equal(pdf2tab:::get_page_rows(p), 1)
  expect_equal(pdf2tab:::get_page_attr(p, "rows"), pdf2tab:::get_page_rows(p))
  expect_equal(pdf2tab:::set_page_cols(p, 3) %>% pdf2tab:::get_page_cols(), 3)
  expect_equal(pdf2tab:::set_page_attr(p, "rows", 3) %>%
                 pdf2tab:::get_page_rows(), 3)
})

test_that("Page printers work right", {
  expect_equal(pdf2tab:::add_chars("ABcdEF", c(2, 4)), "AB|cd|EF")
  expect_equal(pdf2tab:::add_chars("ABcdEF", c(2, 4), 1:2), "AB1cd2EF")
  expect_equal(pdf2tab:::make_row(c(2, 4), 8), "--+--+--")
  expect_equal(pdf2tab:::add_print_rows("ABC", NULL, NULL), "ABC")
  expect_equal(pdf2tab:::add_print_rows(list("AB|CD|EF", "AB|CD|EF", "AB|CD|EF"),
                              1:2, c(2, 4)),
               list("AB|CD|EF", "--+--+--", "AB|CD|EF", "--+--+--", "AB|CD|EF"))
  expect_equal(pdf2tab:::add_rows_recur(list("ABC", "ABC"), 1, "---"),
               list("ABC", "---", "ABC"))
  expect_equal(pdf2tab:::add_rows_recur(list("ABC", "ABC"), NULL, ""),
               list("ABC", "ABC"))
  expect_equal(pdf2tab:::add_print_cols(pdf2tab:::get_page_text(p), NULL),
               list("ABC", "123"))
  # expect_equal(pdf2tab:::add_print_cols(pdf2tab:::get_page_text(p), 2),
  #              list("AB|C", "12|3", "  1 "))
})
