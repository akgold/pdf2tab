library(testthat)
library(pdf2tab)
context("Check pdf loading works right.")

p <- pdf2tab:::pdf_page(c("ABC", "123"), 1, 2)

test_that("pdf constructors, getters, setters work right", {
  expect_equal(pdf2tab:::get_page_text(p), list("ABC", "123"))
  expect_equal(pdf2tab:::get_page_cols(p), 2)
  expect_equal(pdf2tab:::get_page_rows(p), 1)
  expect_equal(pdf2tab:::get_page_attr(p, "rows"), pdf2tab:::get_page_rows(p))
  expect_equal(pdf2tab:::set_page_cols(p, 3) %>% pdf2tab:::get_page_cols(), 3)
  expect_equal(pdf2tab:::set_page_attr(p, "rows", 3) %>%
                 pdf2tab:::get_page_rows(), 3)
})
