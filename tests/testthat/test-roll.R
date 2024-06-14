test_that("Shift a vector", {

  expect_identical(roll(letters[1:5], 2), c("d", "e", "a", "b", "c"))
  expect_identical(roll(letters[1:5], -2), c("c", "d", "e", "a", "b"))
  expect_identical(roll(letters[1:5], 20), letters[1:5])
  expect_identical(roll(letters[1:5], -20), letters[1:5])
  expect_error(roll(letters[1:5], 1.3))
  expect_error(roll(letters[1:5], NA))

})
