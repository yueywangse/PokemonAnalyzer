#' Run an interactive console interface for team coverage
#'
#' Prompts the user for 1-6 Pokemon names, then prints:
#' - Defensive multiplier matrix (what hits you)
#' - Types with no team resistance
#' - Offensive multiplier matrix (what you hit)
#'
#' @export
run_team_console <- function(input_fun = readline,
                             analyze_fun = analyze_team,
                             output_fun = cat) {
  output_fun("\n=== PokemonAnalyzer: Team Coverage Console ===\n")
  output_fun("Enter 1-6 Pokemon names separated by commas.\n")
  output_fun("Example: Spiritomb, togekiss, Lucario, Porygon-Z, milotic,  Garchomp\n")
  output_fun("Type 'q' to quit.\n\n")

  repeat {
    inp <- input_fun("Team> ")
    inp <- trimws(inp)

    if (tolower(inp) %in% c("q", "quit", "exit")) {
      output_fun("Bye!\n")
      return(invisible(NULL))
    }

    team_names <- trimws(strsplit(inp, ",", fixed = TRUE)[[1]])
    team_names <- team_names[nzchar(team_names)]
    team_names <- unique(tolower(team_names))

    if (length(team_names) == 0) {
      output_fun("Please enter at least 1 Pokemon name.\n\n")
      next
    }
    if (length(team_names) > 6) {
      output_fun("Please enter at most 6 Pokemon names.\n\n")
      next
    }

    res <- tryCatch(analyze_fun(team_names), error = identity)
    if (inherits(res, "error")) {
      output_fun("Error:", res$message, "\n\n")
      next
    }

    output_fun("\n--- Team ---\n")
    output_fun(paste0("- ", team_names, collapse = "\n"), "\n\n")

    output_fun("--- Types with NO team resistance/immune (no one takes <= 0.5x) ---\n")
    if (length(res$types_with_no_team_resistance) == 0) {
      output_fun("None\n\n")
    } else {
      output_fun(paste(res$types_with_no_team_resistance, collapse = ", "), "\n\n")
    }

    output_fun("--- Defensive multiplier matrix (rows=attacking types; cols=your mons) ---\n")
    print(round(res$defensive_multiplier_matrix, 2))
    output_fun("\n")

    output_fun("--- Offensive multiplier matrix (rows=defending types; cols=your mons) ---\n")
    print(round(res$offensive_multiplier_matrix, 2))
    output_fun("\n")

    output_fun("Tip: type another team, or 'q' to quit.\n\n")
  }
}
