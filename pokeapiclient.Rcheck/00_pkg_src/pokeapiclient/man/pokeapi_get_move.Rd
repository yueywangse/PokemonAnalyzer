\name{pokeapi_get_move}
\alias{pokeapi_get_move}
\title{Retrieve move details}
\usage{pokeapi_get_move(id_or_name)}
\arguments{
  \item{id_or_name}{Character or integer move identifier (e.g., "thunderbolt" or 85).}
}
\value{A list with id, name, accuracy, power, pp, type, damage_class, and effect text.}
\description{Fetch a single move from PokeAPI and return key metadata.}
\examples{
# pokeapi_get_move("thunderbolt")
}
