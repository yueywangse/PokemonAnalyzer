test_that("pokeapi_get_pokemon can run without HTTP", {
  fake_request <- function(path, query = list(), ...) {
    expect_match(path, "^pokemon/")
    jsonlite::fromJSON(testthat::test_path("data/pikachu.json"), simplifyVector = FALSE)
  }

  with_mocked_in_namespace(
    "pokeapiclient",
    pokeapi_request = fake_request,
    {
      res <- pokeapi_get_pokemon("pikachu")
      expect_equal(res$name, "pikachu")
      expect_true("speed" %in% res$stats$stat)
    }
  )
})

test_that("pokeapi_list_pokemon handles pagination without HTTP", {
  fake_request <- function(path, query = list(), ...) {
    expect_equal(path, "pokemon")
    expect_equal(query$limit, 5)
    expect_equal(query$offset, 0)
    list(
      results = lapply(1:5, function(i) list(
        name = paste0("poke", i),
        url = sprintf("https://pokeapi.co/api/v2/pokemon/%s/", i)
      ))
    )
  }

  with_mocked_in_namespace(
    "pokeapiclient",
    pokeapi_request = fake_request,
    {
      res <- pokeapi_list_pokemon(limit = 5, offset = 0)
      expect_equal(nrow(res), 5)
      expect_true(all(c("name", "id", "url") %in% names(res)))
      expect_equal(res$id, 1:5)
    }
  )
})
