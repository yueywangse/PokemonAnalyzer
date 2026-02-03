\name{pokeapi_get_pokemon}
\alias{pokeapi_get_pokemon}
\title{Retrieve a single Pokemon}
\usage{pokeapi_get_pokemon(id_or_name)}
\arguments{
  \item{id_or_name}{Character or integer identifier (e.g., "pikachu" or 25).}
}
\value{A structured list with id, name, height, weight, base_experience, types, abilities, stats, moves, and sprite URL.}
\description{Fetch one Pokemon record and return parsed, analysis-ready fields.}
\examples{
# Fetch Pikachu
# pokeapi_get_pokemon("pikachu")
}
