test_that("Matching strings", {

  x <- c("Buho", "azúcar", "Prague", "Momentum", "Mom")

  y <- c("Búho búsqueda de piñatas exóticas con él",
         "Stop all the clocks",
         "Azucarillo",
         "Tráigaselo rápido al marqués con muchísimo azúcar",
         "le pont d'avignon n'est pas terminé",
         "Mom")

  # Search.
  z <- match_string(x, y, ignore.case = F, remove.accent = F, reverse = F)
  testthat::expect_equal(sapply(z, nrow), c(0, 1, 0, 0, 1))

  z <- match_string(x, y, ignore.case = F, remove.accent = T, reverse = F)
  testthat::expect_equal(sapply(z, nrow), c(1, 1, 0, 0, 1))

  z <- match_string(x, y, ignore.case = F, remove.accent = T, reverse = T)
  testthat::expect_equal(sapply(z, nrow), c(1, 1, 0, 1, 1))

  z <- match_string(x, y, ignore.case = T, remove.accent = F, reverse = F)
  testthat::expect_equal(sapply(z, nrow), c(0, 1, 0, 0, 1))

  z <- match_string(x, y, ignore.case = T, remove.accent = T, reverse = F)
  testthat::expect_equal(sapply(z, nrow), c(1, 2, 0, 0, 1))

  z <- match_string(x, y, ignore.case = T, remove.accent = T, reverse = T)
  testthat::expect_equal(sapply(z, nrow), c(1, 2, 0, 1, 1))



})
