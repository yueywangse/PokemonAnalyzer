library(httr)
library(jsonlite)

res <- GET("https://pokeapi.co/api/v2/pokemon?limit=100000&offset=0")
stop_for_status(res)

pokemon_list <- fromJSON(content(res, "text", encoding = "UTF-8"))$results

get_stats <- function(url) {
  res <- GET(url)
  stop_for_status(res)

  p <- fromJSON(content(res, "text", encoding = "UTF-8"))

  stats <- setNames(
    p$stats$base_stat,
    p$stats$stat$name
  )

  list(
    name    = p$name,
    hp      = stats["hp"],
    attack  = stats["attack"],
    defense = stats["defense"],
    type = p$types$type$name,
    moves = p$moves,
    picture = p$sprites$other$`official-artwork`$front_default
  )
}

pokemon_stats <- lapply(pokemon_list$url, get_stats)

json_out <- prettify(toJSON(pokemon_stats, auto_unbox = TRUE, pretty = TRUE))
writeLines(json_out, "pokemon_stats.json")