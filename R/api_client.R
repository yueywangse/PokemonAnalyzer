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

#' Call the Google Gemini API to generate text
#'
#' Sends a prompt to the Gemini model and returns the first candidate's text
#' response. Uses the `generateContent` endpoint of the Google Generative
#' Language API.
#'
#' @param prompt Character string with the text prompt to send.
#' @param apikey Character string containing your Gemini API key.
#' @param model Model name to call (default: "gemini-2.5-flash").
#'
#' @return Character string with the model's first text candidate.
#' @export
call_gemini <- function(prompt, apikey, model = "gemini-2.5-flash") {
  stopifnot(is.character(prompt), length(prompt) == 1)
  stopifnot(is.character(apikey), length(apikey) == 1)

  apikey <- trimws(apikey)
  if (!nzchar(apikey)) {
    stop("`apikey` must be a non-empty string", call. = FALSE)
  }

  url <- sprintf(
    "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent",
    model
  )

  body <- list(
    contents = list(
      list(
        parts = list(
          list(text = prompt)
        )
      )
    )
  )

  resp <- httr::POST(
    url,
    query = list(key = apikey),
    body = body,
    encode = "json",
    httr::user_agent("pokeapiclient/0.1.0 (https://github.com/yueywangse/PokemonAnalyzer)")
  )

  if (httr::http_error(resp)) {
    msg <- tryCatch(
      httr::content(resp, as = "text", encoding = "UTF-8"),
      error = function(...) "(no body)"
    )
    stop(sprintf("Gemini request failed [%s]: %s", httr::status_code(resp), msg), call. = FALSE)
  }

  parsed <- httr::content(resp, as = "parsed", type = "application/json", encoding = "UTF-8")
  candidates <- parsed$candidates
  if (is.null(candidates) || length(candidates) == 0) {
    stop("Gemini response missing candidates", call. = FALSE)
  }

  parts <- candidates[[1]]$content$parts
  if (is.null(parts) || length(parts) == 0) {
    stop("Gemini response missing content parts", call. = FALSE)
  }

  parts[[1]]$text %||% ""
}

`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 0)) y else x
}
