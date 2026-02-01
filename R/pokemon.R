#' List Pokemon available in the PokeAPI
#'
#' @param limit Number of rows to return (1-1000).
#' @param offset Result offset for pagination (>= 0).
#' @return A data frame with columns `name`, `id`, and `url`.
#' @examples
#' pokeapi_list_pokemon(limit = 5)
#' @export
pokeapi_list_pokemon <- function(limit = 50, offset = 0) {
  limit <- pokeapi_validate_int(limit, min = 1, max = 1000, arg = "limit")
  offset <- pokeapi_validate_int(offset, min = 0, arg = "offset")

  res <- pokeapi_request("pokemon", query = list(limit = limit, offset = offset))
  out <- data.frame(
    name = vapply(res$results, `[[`, character(1), "name"),
    url = vapply(res$results, `[[`, character(1), "url"),
    stringsAsFactors = FALSE
  )
  out$id <- vapply(out$url, pokeapi_extract_id, integer(1))
  out[, c("name", "id", "url")]
}

#' Retrieve a single Pokemon with structured fields
#'
#' @param id_or_name Character or integer Pokemon identifier (e.g., "pikachu" or 25).
#' @return A list with basic attributes, vectors for `types` and `abilities`,
#'   a data frame `stats`, a character vector `moves`, and `sprite` URL.
#' @examples
#' # get Pikachu (id 25)
#' # pokeapi_get_pokemon("pikachu")
#' @export
pokeapi_get_pokemon <- function(id_or_name) {
  id_or_name <- pokeapi_validate_name_or_id(id_or_name)
  raw <- pokeapi_request(sprintf("pokemon/%s", id_or_name))
  parsed <- pokeapi_parse_pokemon(raw)
  class(parsed) <- c("pokeapi_pokemon", class(parsed))
  parsed
}

pokeapi_parse_pokemon <- function(x) {
  stats <- do.call(rbind, lapply(x$stats, function(s) {
    data.frame(
      stat = s$stat$name,
      base_stat = s$base_stat,
      effort = s$effort,
      stringsAsFactors = FALSE
    )
  }))
  if (is.null(stats)) {
    stats <- data.frame(stat = character(), base_stat = numeric(), effort = numeric())
  }

  moves <- vapply(x$moves, function(m) m$move$name, character(1), USE.NAMES = FALSE)
  abilities <- vapply(x$abilities, function(a) a$ability$name, character(1), USE.NAMES = FALSE)
  types <- vapply(x$types, function(t) t$type$name, character(1), USE.NAMES = FALSE)

  sprite <- x$sprites$other$`official-artwork`$front_default %||%
    x$sprites$front_default %||% NA_character_

  list(
    id = x$id,
    name = x$name,
    height = x$height,
    weight = x$weight,
    base_experience = x$base_experience,
    types = types,
    abilities = abilities,
    stats = stats,
    moves = moves,
    sprite = sprite
  )
}

# helpers ---------------------------------------------------------------

pokeapi_extract_id <- function(url) {
  # URLs end with "/<id>/"; grab penultimate segment
  parts <- strsplit(sub("/$", "", url), "/", fixed = TRUE)[[1]]
  as.integer(utils::tail(parts, 1))
}

pokeapi_validate_name_or_id <- function(x) {
  if (is.numeric(x)) {
    return(pokeapi_validate_int(as.integer(x), min = 1, arg = "id_or_name"))
  }
  if (!is.character(x) || length(x) != 1 || !nzchar(x)) {
    stop("`id_or_name` must be a non-empty character or positive integer", call. = FALSE)
  }
  tolower(x)
}

pokeapi_validate_int <- function(x, min = -Inf, max = Inf, arg = "value") {
  if (length(x) != 1 || is.na(x) || !is.numeric(x)) {
    stop(sprintf("`%s` must be a single numeric value", arg), call. = FALSE)
  }
  x <- as.integer(x)
  if (x < min || x > max) {
    stop(sprintf("`%s` must be between %s and %s", arg, min, max), call. = FALSE)
  }
  x
}
