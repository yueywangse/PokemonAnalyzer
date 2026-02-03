\name{pokeapi_set_base_url}
\alias{pokeapi_set_base_url}
\title{Set the base URL for PokeAPI requests}
\usage{pokeapi_set_base_url(base_url = "https://pokeapi.co/api/v2")}
\arguments{
  \item{base_url}{Character scalar giving the root URL. Useful for testing against mirrors.}
}
\value{Invisibly returns the active base URL.}
\description{Updates the default base URL used by the client.}
\examples{
# Point at a mirror
pokeapi_set_base_url("https://pokeapi.co/api/v2")
}
