test_that("run_team_console runs one cycle and quits", {
  inputs <- c("charizard, gyarados, gengar", "q")
  i <- 0

  fake_readline <- function(prompt = "") {
    i <<- i + 1
    inputs[[i]]
  }

  fake_analyze <- function(pokemon_names, ...) {
    # minimal fake output object shaped like analyze_team() output
    types <- names(type_chart)
    mat <- matrix(1, nrow = length(types), ncol = length(pokemon_names))
    rownames(mat) <- types
    colnames(mat) <- pokemon_names

    list(
      defensive_multiplier_matrix = mat,
      offensive_multiplier_matrix = mat,
      types_with_no_team_resistance = character(0)
    )
  }

  out <- capture.output({
    expect_no_error(
      run_team_console(
        input_fun = fake_readline,
        analyze_fun = fake_analyze,
        output_fun = function(...) cat(...)
      )
    )
  })

  expect_true(length(out) > 0)
})