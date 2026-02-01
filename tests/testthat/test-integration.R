test_that("live call pulls pikachu when online", {
  skip_if_offline(host = "pokeapi.co")
  res <- pokeapi_get_pokemon("pikachu")
  expect_equal(res$name, "pikachu")
  expect_true("speed" %in% res$stats$stat)
})

test_that("listing uses pagination", {
  skip_if_offline(host = "pokeapi.co")
  res <- pokeapi_list_pokemon(limit = 5, offset = 0)
  expect_equal(nrow(res), 5)
  expect_true(all(c("name", "id", "url") %in% names(res)))
})
