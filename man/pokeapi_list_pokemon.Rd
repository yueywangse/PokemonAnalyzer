\name{pokeapi_list_pokemon}
\alias{pokeapi_list_pokemon}
\title{List Pokemon}
\usage{pokeapi_list_pokemon(limit = 50, offset = 0)}
\arguments{
  \item{limit}{Number of rows to return (1-1000).}
  \item{offset}{Zero-based offset for pagination.}
}
\value{A data frame with columns \code{name}, \code{id}, and \code{url}.}
\description{Retrieve paginated Pokemon metadata from PokeAPI.}
\examples{
pokeapi_list_pokemon(limit = 5)
}
