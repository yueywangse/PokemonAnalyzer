test_that("analyze_team returns expected matrices and types", {
  team <- list(
    charizard = mock_pokemon("charizard", c("fire", "flying")),
    gyarados  = mock_pokemon("gyarados", c("water", "flying")),
    gengar    = mock_pokemon("gengar", c("ghost", "poison"))
  )

  fake_fetch <- function(name) {
    key <- tolower(trimws(name))
    if (!key %in% names(team)) stop("not found in fake_fetch: ", key)
    team[[key]]
  }

  out <- analyze_team(c("charizard", "gyarados", "gengar"), fetch_fun = fake_fetch)

  expect_true(is.matrix(out$defensive_multiplier_matrix))
  expect_true(is.numeric(out$defensive_multiplier_matrix))
  expect_equal(ncol(out$defensive_multiplier_matrix), 3)

  expect_true(is.matrix(out$offensive_multiplier_matrix))
  expect_true(is.numeric(out$offensive_multiplier_matrix))
  expect_equal(ncol(out$offensive_multiplier_matrix), 3)

  expect_true(!is.null(rownames(out$defensive_multiplier_matrix)))
  expect_true(!is.null(rownames(out$offensive_multiplier_matrix)))

  expect_true(is.character(out$types_with_no_team_resistance))
})

test_that("analyze_team validates team size", {
  expect_error(analyze_team(character(0)))
  # must be 7 DISTINCT names because your analyze_team() probably does unique()
  expect_error(analyze_team(paste0("poke", 1:7)))
})