% Generated manually
\name{get_pokemon_id}
\alias{get_pokemon_id}
\title{Look up a Pokemon's ID and type from the cached stats file}
\description{
Look up a Pokemon's id and type from the cached \code{pokemon_stats.json} file.
}
\usage{
get_pokemon_id(name, path = "pokemon_stats.json")
}
\arguments{
  \item{name}{Character scalar Pokemon name (case-insensitive).}
  \item{path}{Path to the \code{pokemon_stats.json} file.}
}
\value{
A data frame with columns \code{pokemon_name}, \code{pokemon_id}, and \code{type}.
}
\examples{
tmp <- tempfile()
dir.create(tmp)
path <- file.path(tmp, "pokemon_stats.json")
jsonlite::write_json(
  list(
    list(name = "pikachu", type = list("electric")),
    list(name = "pidgey", type = list("normal", "flying"))
  ),
  path,
  auto_unbox = TRUE,
  pretty = TRUE
)

get_pokemon_id("pikachu", path = path)
get_pokemon_id("Pidgey", path = path)
}
