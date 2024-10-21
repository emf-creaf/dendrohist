test_that("Convert ddmmss to coordinates and back", {

  # Needed below.
  f <- function(x) ifelse(x < 10, paste0("0", x), as.character(x))

  # We test all possible combinations "à la force brute".
  i_seq <- c(0, seq(1, 59, 2))
  j_seq <- c(0, seq(1, 59, 2))
  k_seq <- c(0, seq(1, 359, 2))

  x <- numeric(length(i_seq) * length(j_seq) * length(k_seq))
  icount <- 1
  for (i in i_seq) {
    for (j in j_seq) {
      for (k in k_seq) {
        x[icount] <- paste0(k, f(j), f(i))
        icount <- icount + 1
      }
    }
  }

  y <- extract_ddmmss(x)
  z <- build_ddmmss(y)
  yy <- extract_ddmmss(z)
  zz <- build_ddmmss(yy)
  expect_equal(y, yy)


  # Same for negatives.
  x <- numeric(length(i_seq) * length(j_seq) * length(k_seq))
  icount <- 1
  for (i in i_seq) {
    for (j in j_seq) {
      for (k in k_seq) {
        x[icount] <- paste0("-", k, f(j), f(i))
        icount <- icount + 1
      }
    }
  }

  y <- extract_ddmmss(x)
  z <- build_ddmmss(y)
  yy <- extract_ddmmss(z)
  zz <- build_ddmmss(yy)
  expect_equal(y, yy)


})
