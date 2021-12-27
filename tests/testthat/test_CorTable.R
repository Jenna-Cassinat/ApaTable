testthat::test_that(
  "CorTable makes a table that looks nice",
  {
    rmarkdown::render("test_CorTable.Rmd")
    system2("open","test_CorTable.pdf")
    testthat::expect_true(TRUE)
  }
)
