test_that("Change type of columns", {
  df <- list()
  expect_error(set_colmode(df))

  df <- numeric(4)
  expect_error(set_colmode(df))

  df <- data.frame(a=1:4, b=letters[1:4])
  df <- set_colmode(df, c("double", "character"))
  expect_identical(unname(sapply(df, typeof)), c("double", "character"))
  df$a <- as.character(df$a)
  expect_identical(unname(sapply(df, typeof)), c("character", "character"))

})
