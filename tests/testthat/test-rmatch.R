test_that("Pattern/Value matching", {

  x <- c("NA", "asdf", "aaa", "ppNA")
  y <- c("ASDF", "asdf", "ghhjjh", "aaa", "sdgfdgfhj", "NA", "aaa", "Pasdf")

  expect_equal(rmatch(x, y), list(6, 2, c(4, 7), integer()))
  expect_equal(rmatch(x, y, T), list(6, c(2, 8), c(4, 7), integer()))
  expect_equal(rmatch(y, x), list(numeric(), 2, numeric(), 3, numeric(), 1, 3, numeric()))
  expect_equal(rmatch(y, x, T), list(numeric(), 2, numeric(), 3, numeric(), c(1, 4), 3, numeric()))

  # Add true NAs.
  x <- c(x, NA)
  y <- c(y, NA)

  expect_equal(rmatch(x, y), list(6, 2, c(4, 7), integer(), integer()))
  expect_error(rmatch(x, y, T))
  expect_equal(rmatch(y, x), list(numeric(), 2, numeric(), 3, numeric(), 1, 3, numeric(), numeric()))
  expect_error(rmatch(y, x, T))

})
