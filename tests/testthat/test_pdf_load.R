library(testthat)
library(pdf2tab)
context("Check pdf loading works right.")


test_that("pdf loading and editing works right", {
  expect_warning(p <- pdf_load(test_pdf_text(pages = "all")))
  expect_equal(class(p), "pdf")
  expect_equal(length(p$text), 86)

  p <- keep_pages(p, 7:45)
  expect_equal(length(p$text), 39)

  p <- drop_lines(p, 1, "top")
  p <- drop_lines(p, 2, "bottom")
  expect_equal(length(p$text[[1]]$text), 50)
})
