testthat::test_that(
  "Everything looks nice",
  {
    rmarkdown::render("test_knitting.Rmd")
    system2("open","test_knitting.pdf")
    testthat::expect_true(TRUE)
  }
)
