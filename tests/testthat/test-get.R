context("get functions")

test_that("get functions work as expected", {
  afscs <- c("9T000", "92T0K", "A9T0X0K", "", NA)
  expect_identical(get_career_group(afscs), c("9", "9", NA, NA, NA))
  expect_identical(get_invalid_afscs(afscs), c("A9T0X0K", "", NA))
  expect_identical(get_prefix(afscs), c("none", "none", "A", "none", "none"))
  expect_identical(get_suffix(afscs), c("none", "K", "K", "none", "none"))
  expect_identical(get_valid_afscs(afscs), c("9T000", "92T0K"))
})
