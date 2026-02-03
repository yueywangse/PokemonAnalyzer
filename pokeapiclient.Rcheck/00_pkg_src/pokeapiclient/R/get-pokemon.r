#' Download full Pokemon roster and assemble cached stats
#'
#' Helper utilities that call the public PokeAPI to build `pokemon_stats.json`,
#' a cached data file consumed elsewhere in the package. These helpers are
#' intended for data generation, not as part of the public API.
#' @keywords internal

#' Fetch details for a single move
#'
#' @param move_row Row from the moves data.frame returned by the Pokemon call
#'   containing `name` and `url` columns.
#' @return List with `name`, `accuracy`, `power`, `damage_class`, and `type`.
#' @keywords internal
get_move_details <- function(move_row) {
  mres <- httr::GET(move_row$url)
  httr::stop_for_status(mres)

  m <- jsonlite::fromJSON(httr::content(mres, "text", encoding = "UTF-8"))

  to_zero <- function(x) {
    if (is.null(x) || (is.atomic(x) && length(x) == 0) || is.na(x)) 0 else x
  }

  list(
    name         = m$name,
    accuracy     = to_zero(m$accuracy),
    power        = to_zero(m$power),
    damage_class = m$damage_class$name,
    type         = m$type$name
  )
}

#' Fetch stats and limited move info for one Pokemon
#'
#' @param url Fully-qualified PokeAPI URL for a Pokemon resource.
#' @param max_moves Optional integer limiting how many moves to fetch (default 5).
#' @return List containing core stats, types, first `max_moves` move details,
#'   and the official artwork URL.
#' @keywords internal
get_stats <- function(url, max_moves = 5) {
  res <- httr::GET(url)
  httr::stop_for_status(res)

  p <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))

  stats <- stats::setNames(
    p$stats$base_stat,
    p$stats$stat$name
  )

  moves <- p$moves$move
  move_details <- list()
  if (!is.null(moves) && length(moves) > 0) {
    # ensure data.frame for consistent indexing
    if (!is.data.frame(moves)) {
      moves <- as.data.frame(moves, stringsAsFactors = FALSE)
    }
    if (!is.null(max_moves)) {
      moves <- utils::head(moves, max_moves)
    }
    move_count <- nrow(moves)
    if (!is.null(move_count) && move_count > 0) {
      move_details <- lapply(seq_len(move_count), function(i) get_move_details(moves[i, ]))
    }
  }

  list(
    name     = p$name,
    hp       = stats["hp"],
    attack   = stats["attack"],
    defense  = stats["defense"],
    sattack  = stats["special-attack"],
    sdefense = stats["special-defense"],
    type     = unname(p$types$type$name),
    moves    = move_details,
    picture  = p$sprites$other$`official-artwork`$front_default
  )
}

#' Build a local cached stats file from the PokeAPI
#'
#' This function is intentionally separate from package load to avoid
#' network calls in automated environments (e.g., CI).
#'
#' @param path Output path for the JSON cache.
#' @param max_moves Max moves per Pokemon to include (default 5).
#' @param limit Maximum number of Pokemon to fetch (default 100000).
#' @param offset Offset for pagination (default 0).
#' @return Invisibly returns the output path.
#' @keywords internal
build_pokemon_stats_cache <- function(path = "pokemon_stats.json",
                                      max_moves = 5,
                                      limit = 100000,
                                      offset = 0) {
  res <- httr::GET(
    "https://pokeapi.co/api/v2/pokemon",
    query = list(limit = limit, offset = offset)
  )
  httr::stop_for_status(res)

  pokemon_list <- jsonlite::fromJSON(
    httr::content(res, "text", encoding = "UTF-8")
  )$results

  pokemon_stats <- lapply(pokemon_list$url, get_stats, max_moves = max_moves)

  json_out <- jsonlite::prettify(
    jsonlite::toJSON(pokemon_stats, auto_unbox = TRUE, pretty = TRUE)
  )
  writeLines(json_out, path)
  invisible(path)
}
