test_that("multiplication works", {


  x <- "https://ceh.cedex.es/anuarioaforos/temp/resultado8187.txt"
  expect_error(read_cedex(x, type = "monthly", station_label = "9020"))

  x <- "https://ceh.cedex.es/anuarioaforos/temp/resultado4370.txt"
  expect_error(read_cedex(x, type = "daily", station_label = "9256"))

})
