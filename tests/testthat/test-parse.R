test_that("pokemon parsing returns structured fields", {
  raw <- jsonlite::fromJSON(testthat::test_path("data/pikachu.json"), simplifyVector = FALSE)
  parsed <- pokeapi_parse_pokemon(raw)

  expect_equal(parsed$name, "pikachu")
  expect_equal(parsed$id, 25)
  expect_equal(parsed$types, "electric")
  expect_true(nrow(parsed$stats) >= 6)
  expect_true("speed" %in% parsed$stats$stat)
  expect_length(parsed$moves, 3)
  expect_match(parsed$sprite, "official-artwork")
})

test_that("move parsing extracts english effect", {
  raw <- jsonlite::fromJSON(testthat::test_path("data/thunderbolt.json"), simplifyVector = FALSE)
  res <- pokeapi_pick_effect(raw$effect_entries)
  expect_match(res, "paralyze")
})
