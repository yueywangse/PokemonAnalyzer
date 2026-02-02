#' Run an interactive console interface for team coverage
#'
#' Prompts the user for 1–6 Pokemon names, then prints:
#' - Defensive multiplier matrix (what hits you)
#' - Types with no team resistance
#' - Offensive multiplier matrix (what you hit)
#'
#' @export
run_team_console <- function() {
  cat("\n=== PokemonAnalyzer: Team Coverage Console ===\n")
  cat("Enter 1–6 Pokémon names separated by commas.\n")
  cat("Example: Spiritomb, togekiss, Togekiss, Porygon-Z, milotic,  Garchomp\n")
  cat("Type 'q' to quit.\n\n")

  repeat {
    inp <- readline("Team> ")
    inp <- trimws(inp)

    if (tolower(inp) %in% c("q", "quit", "exit")) {
      cat("Bye!\n")
      return(invisible(NULL))
    }

    team_names <- trimws(strsplit(inp, ",", fixed = TRUE)[[1]])
    team_names <- team_names[nzchar(team_names)]
    team_names <- unique(tolower(team_names))

    if (length(team_names) == 0) {
      cat("Please enter at least 1 Pokémon name.\n\n")
      next
    }
    if (length(team_names) > 6) {
      cat("Please enter at most 6 Pokémon names.\n\n")
      next
    }

    res <- tryCatch(analyze_team(team_names), error = identity)
    if (inherits(res, "error")) {
      cat("Error:", res$message, "\n\n")
      next
    }

    cat("\n--- Team ---\n")
    cat(paste0("- ", team_names), sep = "\n")
    cat("\n")

    cat("--- Types with NO team resistance/immune (no one takes <= 0.5x) ---\n")
    if (length(res$types_with_no_team_resistance) == 0) {
      cat("None 🎉\n\n")
    } else {
      cat(paste(res$types_with_no_team_resistance, collapse = ", "), "\n\n")
    }

    cat("--- Defensive multiplier matrix (rows=attacking types; cols=your mons) ---\n")
    print(round(res$defensive_multiplier_matrix, 2))
    cat("\n")

    cat("--- Offensive multiplier matrix (rows=defending types; cols=your mons) ---\n")
    print(round(res$offensive_multiplier_matrix, 2))
    cat("\n")

    cat("Tip: type another team, or 'q' to quit.\n\n")
  }
}