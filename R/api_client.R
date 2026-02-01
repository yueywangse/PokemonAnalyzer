#' Set the base URL for PokeAPI requests
#'
#' Allows pointing the client at alternative PokeAPI mirrors for testing.
#'
#' @param base_url Character scalar with the root URL (default keeps current value).
#' @return Invisibly returns the active base URL.
#' @export
pokeapi_set_base_url <- function(base_url = "https://pokeapi.co/api/v2") {
  stopifnot(is.character(base_url), length(base_url) == 1, nzchar(base_url))
  base_url <- sub("/+$", "", base_url)
  options(pokeapiclient.base_url = base_url)
  invisible(base_url)
}

pokeapi_base_url <- function() {
  getOption("pokeapiclient.base_url", "https://pokeapi.co/api/v2")
}

pokeapi_build_url <- function(path, base_url = pokeapi_base_url()) {
  stopifnot(is.character(path), length(path) == 1)
  path <- gsub("^/+", "", path)
  base <- sub("/+$", "", base_url)
  paste(base, path, sep = "/")
}

pokeapi_request <- function(path, query = list(), timeout_sec = 15, retry = TRUE) {
  url <- pokeapi_build_url(path)
  resp <- httr::GET(
    url,
    query = query,
    httr::user_agent("pokeapiclient/0.1.0 (https://github.com/richardhua/PokemonAnalyzer)"),
    httr::timeout(timeout_sec)
  )

  if (httr::status_code(resp) == 429 && retry) {
    retry_after <- as.numeric(httr::headers(resp)[["retry-after"]])
    if (!is.na(retry_after) && retry_after <= 5) {
      Sys.sleep(retry_after)
    } else {
      Sys.sleep(1)
    }
    return(pokeapi_request(path, query = query, timeout_sec = timeout_sec, retry = FALSE))
  }

  if (httr::http_error(resp)) {
    msg <- tryCatch(
      httr::content(resp, as = "text", encoding = "UTF-8"),
      error = function(...) "(no body)"
    )
    stop(sprintf("PokeAPI request failed [%s]: %s", httr::status_code(resp), msg), call. = FALSE)
  }

  text <- httr::content(resp, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}

`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 0)) y else x
}
