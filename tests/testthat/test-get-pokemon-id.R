test_that("get_pokemon_id returns expected info (case-insensitive)", {
  tmp <- tempfile()
  dir.create(tmp)
  old <- setwd(tmp)
  on.exit(setwd(old), add = TRUE)

  sample <- list(
    list(name = "bulbasaur", type = list("grass", "poison")),
    list(name = "pikachu",   type = list("electric"))
  )
  jsonlite::write_json(sample, "pokemon_stats.json", auto_unbox = TRUE, pretty = TRUE)

  res <- get_pokemon_id("Pikachu")
  expect_equal(res$pokemon_id, 2L)
  expect_equal(res$pokemon_name, "pikachu")
  expect_equal(res$type, "electric")
})

test_that("get_pokemon_id errors on unknown names", {
  tmp <- tempfile()
  dir.create(tmp)
  old <- setwd(tmp)
  on.exit(setwd(old), add = TRUE)

  sample <- list(list(name = "bulbasaur", type = list("grass", "poison")))
  jsonlite::write_json(sample, "pokemon_stats.json", auto_unbox = TRUE, pretty = TRUE)

  expect_error(get_pokemon_id("missingmon"), "No Pokemon found with that name.")
})
