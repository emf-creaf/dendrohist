test_that("multiplication works", {


  # source(".\\R\\curly_quotes.R")
  # source(".\\R\\replace_accent.R")
  # source(".\\R\\string_split.R")
  # source(".\\R\\comma_swap.R")

  data(munic_CNIG)
  data(munic_INE)
  expect_message(check_CNIG_INE(munic_CNIG, munic_INE))

  # One name is duplicated.


})
