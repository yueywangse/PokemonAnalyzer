library(jsonlite)

get_pokemon_id <- function(name) {
  data <- fromJSON("pokemon_stats.json")
  
  name <- tolower(name)
  idx <- match(name, tolower(data$name))
  
  if (is.na(idx)) {
    stop("No Pokémon found with that name.", call. = FALSE)
  }
  
  data.frame(
    pokemon_name = data$name[idx],
    pokemon_id = idx,
    type = paste(data$type[[idx]], collapse = "/"),
    stringsAsFactors = FALSE
  )
}

# get_pokemon_id("magikarp")