test_that("Substitute curly quotes", {

  x <- c("Straight quote'", "Single straight '‘and right curly quotes ", "Double “ and ” single ‘curly ’ ''quotes")
  y <- c("Straight quote'", "Single straight ''and right curly quotes ", "Double ' and ' single 'curly ' ''quotes")
  z <- c("Straight quote'", "Single straight ''and right curly quotes ", "Double '' and '' single 'curly ' ''quotes")

  expect_identical(curly_quotes(x), y)
  expect_identical(curly_quotes(x, F), z)

})
