test_that("integer validation works", {
  expect_equal(pokeapi_validate_int(5, min = 1, max = 10, arg = "limit"), 5L)
  expect_error(pokeapi_validate_int(-1, min = 0, arg = "limit"), "between")
  expect_error(pokeapi_validate_int("a", arg = "limit"), "single numeric")
})

test_that("name/id validation works", {
  expect_equal(pokeapi_validate_name_or_id("Pikachu"), "pikachu")
  expect_equal(pokeapi_validate_name_or_id(25), 25L)
  expect_error(pokeapi_validate_name_or_id(c("a", "b")), "non-empty")
})
