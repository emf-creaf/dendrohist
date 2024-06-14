test_that("Replacing diacritics and tildes", {

  expect_identical(replace_accent(c("Áñò", "Búho", "Añoro")), c("Ano", "Buho", "Anoro"))


})
