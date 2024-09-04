test_that("Decimal degrees", {


  # Needed below.
  f <- function(x) ifelse(x < 10, paste0("0", x), as.character(x))


  # We test all possible combinations "à la force brute".
  i_seq <- c(0, seq(1, 59, 2))
  j_seq <- c(0, seq(1, 59, 2))
  k_seq <- c(0, seq(1, 359, 2))

  y <- numeric(length(i_seq) * length(j_seq) * length(k_seq))
  z <- character(length(i_seq) * length(j_seq) * length(k_seq))
  icount <- 1
  for (i in i_seq) {
    for (j in j_seq) {
      for (k in k_seq) {
        y[icount] <- k + j/60 + i/3600
        z[icount] <- paste0(k, f(j), f(i))
        icount <- icount + 1
      }
    }
  }

  zz <- extract_ddmmss(z)

  # Compare.
  expect_equal(y, zz)


  # Same with a minus sign.
  y <- -y
  z <- paste0("-", z)
  zz <- extract_ddmmss(z)
  expect_equal(y, zz)


  expect_error(extract_ddmmss(c(4, 6)))
  expect_equal(extract_ddmmss(c("3", "6")), c(3, 6)/3600)

  expect_error(extract_ddmmss("-33333333"))

  expect_equal(extract_ddmmss("00003"), 0.0008333333)
  expect_equal(extract_ddmmss("000003"), 0.0008333333)
  expect_equal(extract_ddmmss("-00003"), -0.0008333333)
  expect_equal(extract_ddmmss("-000003"), -0.0008333333)

  expect_equal(extract_ddmmss("00034"), 0.009444444)
  expect_equal(extract_ddmmss("-00034"), -0.009444444)

  expect_equal(extract_ddmmss("000342"), 0.06166667)
  expect_equal(extract_ddmmss("-000342"), -0.06166667)

  expect_equal(extract_ddmmss("03421"), 0.5725)
  expect_equal(extract_ddmmss("-03421"), -0.5725)

  expect_equal(extract_ddmmss("34215"), 3.704167, tolerance = 1e-06)
  expect_equal(extract_ddmmss("-34215"), -3.704167, tolerance = 1e-06)

  expect_equal(extract_ddmmss("342156"), 34.36556, tolerance = 1e-06)
  expect_equal(extract_ddmmss("-342156"), -34.36556, tolerance = 1e-06)

})
