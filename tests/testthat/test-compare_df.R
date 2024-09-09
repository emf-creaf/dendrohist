test_that("multiplication works", {


  # Fake databases
  df1 <- as.data.frame(matrix(100, 20, 5))
  colnames(df1) <- letters[1:5]

  df2 <- as.data.frame(matrix(80, 20, 4))
  colnames(df2) <- letters[2:5]

  df3 <- as.data.frame(matrix(100, 20, 5))
  colnames(df3) <- letters[2:6]

  df4 <- df1
  df4[, 1] <- as.character(df1[, 1])

  # All correct.
  expect_true(compare_df(df1, df1, df1))

  # Number of columns is incorrect.
  expect_false(compare_df(df1, df1, df2))

  # Name of columns is incorrect, but number is ok.
  expect_false(compare_df(df1, df1, df3))

  # Column format is incorrect.
  expect_false(compare_df(df1, df1, df4))

  # All at the same time.
  expect_false(compare_df(df1, df2, df3, df4))

  # One is not a data.frame.
  expect_false(compare_df(df1, df2, df3, df4, runif(100)))


})
