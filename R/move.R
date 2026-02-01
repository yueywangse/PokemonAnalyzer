#' Retrieve move details from PokeAPI
#'
#' @param id_or_name Character or integer move identifier (e.g., "thunderbolt" or 85).
#' @return A list with id, name, accuracy, power, pp, type, damage_class, and short effect text.
#' @examples
#' # pokeapi_get_move("thunderbolt")
#' @export
pokeapi_get_move <- function(id_or_name) {
  id_or_name <- pokeapi_validate_name_or_id(id_or_name)
  raw <- pokeapi_request(sprintf("move/%s", id_or_name))
  parsed <- list(
    id = raw$id,
    name = raw$name,
    accuracy = raw$accuracy,
    power = raw$power,
    pp = raw$pp,
    type = raw$type$name,
    damage_class = raw$damage_class$name,
    effect = pokeapi_pick_effect(raw$effect_entries)
  )
  class(parsed) <- c("pokeapi_move", class(parsed))
  parsed
}

pokeapi_pick_effect <- function(effect_entries) {
  if (is.null(effect_entries) || length(effect_entries) == 0) return(NA_character_)
  eng <- Filter(function(e) identical(e$language$name, "en"), effect_entries)
  if (length(eng) > 0) {
    eng[[1]]$short_effect %||% eng[[1]]$effect
  } else {
    effect_entries[[1]]$short_effect %||% effect_entries[[1]]$effect
  }
}
