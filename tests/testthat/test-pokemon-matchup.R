test_that("pokemon_matchup works with pokeapi-style objects", {
  p1 <- mock_pokemon("mew", c("psychic"), attack = 100, defense = 100)
  p2 <- mock_pokemon("mewtwo", c("psychic"), attack = 120, defense = 90)

  res <- pokemon_matchup(p1, p2)

  expect_true(is.data.frame(res))
  expect_true(all(c(
    "your_pokemon", "opponent_pokemon",
    "percent_win_chance", "favored_pokemon"
  ) %in% names(res)))

  expect_gte(res$percent_win_chance, 0)
  expect_lte(res$percent_win_chance, 100)
})

test_that("pokemon_matchup works with names using injected fetch_fun", {
  p_a <- mock_pokemon("charizard", c("fire", "flying"), attack = 120, defense = 85)
  p_b <- mock_pokemon("venusaur", c("grass", "poison"), attack = 100, defense = 100)

  fake_fetch <- function(name) {
    nm <- tolower(trimws(name))
    if (nm == "charizard") return(p_a)
    if (nm == "venusaur") return(p_b)
    stop("unexpected name: ", nm)
  }

  res <- pokemon_matchup("charizard", "venusaur", fetch_fun = fake_fetch)

  expect_equal(res$your_pokemon, "charizard")
  expect_equal(res$opponent_pokemon, "venusaur")
  expect_gte(res$percent_win_chance, 0)
  expect_lte(res$percent_win_chance, 100)
})