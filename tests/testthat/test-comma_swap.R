test_that("Swap string with a comma", {


  x <- c(",", " ,", ", ", " , ", "Rosario, El", "Madrid capital", "Madrid, Spain, Europe", NA)
  xT <- c(",", ",", ",", "", "El Rosario", "Madrid capital", "Spain, Europe Madrid", NA)
  xF <- c(",", " ,", ", ", "   ", " El Rosario", "Madrid capital", " Spain, Europe Madrid", NA)

  expect_identical(comma_swap(x, T), xT)
  expect_identical(comma_swap(x, F), xF)

  expect_error(comma_swap(x, T, F))
  expect_error(comma_swap(x, F, F))


})
