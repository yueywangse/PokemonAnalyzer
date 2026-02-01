library(httr)
library(jsonlite)

res <- GET("https://pokeapi.co/api/v2/pokemon?limit=100000&offset=0")
stop_for_status(res)

pokemon_list <- fromJSON(content(res, "text", encoding = "UTF-8"))$results

get_move_details <- function(move_row) {
  mres <- GET(move_row$url)
  stop_for_status(mres)

  m <- fromJSON(content(mres, "text", encoding = "UTF-8"))

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

get_stats <- function(url, max_moves = 5) {
  res <- GET(url)
  stop_for_status(res)

  p <- fromJSON(content(res, "text", encoding = "UTF-8"))

  stats <- setNames(
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
      moves <- head(moves, max_moves)
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

pokemon_stats <- lapply(pokemon_list$url, get_stats)

json_out <- prettify(toJSON(pokemon_stats, auto_unbox = TRUE, pretty = TRUE))
writeLines(json_out, "pokemon_stats.json")
