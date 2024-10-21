test_that("multiplication works", {


  # Fake databases
  df1 <- as.data.frame(matrix(runif(100), 20, 5))
  colnames(df1) <- letters[1:5]

  df2 <- as.data.frame(matrix(runif(80), 20, 4))
  colnames(df2) <- letters[2:5]

  df3 <- as.data.frame(matrix(runif(100), 20, 5))
  colnames(df3) <- letters[2:6]

  df4 <- df1
  df4[, 1] <- as.character(df1[, 1])

  # There are duplicated rows.
  expect_false(compare_df(df1, df1, df1))

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

  # Returning indices of duplicated rows, if there is any.
  df5 <-   as.data.frame(matrix(runif(100), 20, 5))
  colnames(df5) <- letters[1:5]
  expect_true(compare_df(df1, df5))
  df5 <- rbind(df5, df1[1, ])
  expect_false(compare_df(df1, df5))
  expect_equal(compare_df(df1, df5, index_duplicated = T), 41)


})
