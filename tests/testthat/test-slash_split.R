test_that("Split strings at slash position", {


  x <- c(",", " ,", ", ", " , ", "Rosario, El", "Madrid capital", "Madrid, Spain, Europe")
  xT <- c(",", ",", ",", "", "El Rosario", "Madrid capital", "Spain, Europe Madrid")
  xF <- c(",", " ,", ", ", "   ", " El Rosario", "Madrid capital", " Spain, Europe Madrid")

  # Tests with trim = T.
  expect_identical(slash_split("/", T), data.frame(left = "", right = ""))
  expect_identical(slash_split("/ Slash at the start", T), data.frame(left = "", right = "Slash at the start"))
  expect_identical(slash_split("Slash at the end /", T), data.frame(left = "Slash at the end", right = ""))
  expect_identical(slash_split("No slash in sight", T), data.frame(left = "No slash in sight", right = NA))

  # Tests with trim = F.
  expect_identical(slash_split(" / ", F), data.frame(left = " ", right = " "))
  expect_identical(slash_split("/ Slash at the start", F), data.frame(left = "", right = " Slash at the start"))
  expect_identical(slash_split("Slash at the end /", F), data.frame(left = "Slash at the end ", right = ""))
  expect_identical(slash_split(" No slash in sight ", F), data.frame(left = " No slash in sight ", right = NA))

})
