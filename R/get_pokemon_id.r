#' Look up a Pokemon's id and type from the cached stats file
#'
#' @param name Character scalar: Pokemon name (case-insensitive).
#' @param path Path to the `pokemon_stats.json` file.
#' @return A data frame with columns `pokemon_name`, `pokemon_id`, and `type`.
#' @examples
#' get_pokemon_id("pikachu")
#' get_pokemon_id("Pidgey")
#' @export
get_pokemon_id <- function(name, path = "pokemon_stats.json") {
  data <- jsonlite::fromJSON(path)

  name <- tolower(name)
  idx <- match(name, tolower(data$name))

  if (is.na(idx)) {
    stop("No Pokemon found with that name.", call. = FALSE)
  }

  data.frame(
    pokemon_name = data$name[idx],
    pokemon_id = idx,
    type = paste(data$type[[idx]], collapse = "/"),
    stringsAsFactors = FALSE
  )
}

# get_pokemon_id("magikarp")
