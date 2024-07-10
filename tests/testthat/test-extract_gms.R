test_that("Decimal degrees", {

  expect_error(extract_gms(3))
  expect_error(extract_gms(c(4, 6)))
  expect_error(extract_gms(c("3", "6")))

  expect_warning(extract_gms("33333333"))

  expect_equal(extract_gms("3"), 0.0008333333)
  expect_equal(extract_gms("-3"), -0.0008333333)

  expect_equal(extract_gms("34"), 0.009444444)
  expect_equal(extract_gms("-34"), -0.009444444)

  expect_equal(extract_gms("342"), 0.06166667)
  expect_equal(extract_gms("-342"), -0.06166667)

  expect_equal(extract_gms("3421"), 0.5725)
  expect_equal(extract_gms("-3421"), -0.5725)

  expect_equal(extract_gms("34215"), 3.704167, tolerance = 1e-06)
  expect_equal(extract_gms("-34215"), -3.704167, tolerance = 1e-06)

  expect_equal(extract_gms("342156"), 34.36556, tolerance = 1e-06)
  expect_equal(extract_gms("-342156"), -34.36556, tolerance = 1e-06)

  expect_equal(expect_warning(extract_gms("4342156")), 434.3656, tolerance = 1e-04)
  expect_equal(expect_warning(extract_gms("-4342156")), -434.3656, tolerance = 1e-04)


})
