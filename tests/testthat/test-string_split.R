test_that("Split strings at slash or dash position", {

  x <- c("This is left/ and right", "/ Slash at start", "Slash at the end /", "No slash at all")

  # Tests with trim = F and T.
  expect_identical(string_split(x, "/", T),
                   data.frame(left = c("This is left", "", "Slash at the end", "No slash at all"),
                              right = c("and right", "Slash at start", "", NA)))
  expect_identical(string_split(x, "/", F),
                   data.frame(left = c("This is left", "", "Slash at the end ", "No slash at all"),
                              right = c(" and right", " Slash at start", "", NA)))


  # Tests with dash.
  x <- gsub("/", "-", x)
  expect_identical(string_split(x, "-", T),
                   data.frame(left = c("This is left", "", "Slash at the end", "No slash at all"),
                              right = c("and right", "Slash at start", "", NA)))
  expect_identical(string_split(x, "-", F),
                   data.frame(left = c("This is left", "", "Slash at the end ", "No slash at all"),
                              right = c(" and right", " Slash at start", "", NA)))

})
