test_that("Swap string with a comma", {


  x <- c(",", " ,", ", ", " , ", "Rosario, El", "Madrid capital", "Madrid, Spain, Europe")
  xT <- c(",", ",", ",", "", "El Rosario", "Madrid capital", "Spain, Europe Madrid")
  xF <- c(",", " ,", ", ", "   ", " El Rosario", "Madrid capital", " Spain, Europe Madrid")

  expect_identical(comma_swap(x, T), xT)
  expect_identical(comma_swap(x, F), xF)

})
