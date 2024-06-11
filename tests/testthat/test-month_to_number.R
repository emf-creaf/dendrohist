test_that("Month numbers from English names", {

  # Character vector.
  x <- c("Asd","October", "January", "Dec", "June_long", "march")

  # Tests.
  expect_equal(sum(is.na(month_to_number(x))), 2)
  expect_error(month_to_number(1:4))
  expect_equal(month_to_number(x), c(NA, 10, 1, 12, NA, 3))

  expect_equal(sum(is.na(month_to_number(x, to_lower = F))), 3)
  expect_equal(sum(is.na(month_to_number(x, to_lower = F, abbreviated = F))), 4)
  expect_equal(sum(is.na(month_to_number(x, to_lower = T, abbreviated = F))), 3)

})
