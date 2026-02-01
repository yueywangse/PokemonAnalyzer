# pokeapiclient

![R-CMD-check](https://github.com/yueywangse/PokemonAnalyzer/actions/workflows/R-CMD-check.yaml/badge.svg)

Lightweight, cross-platform R client for the public [PokeAPI](https://pokeapi.co/). It wraps the REST endpoints into a few consistent, tested functions that return tidy data frames and lists ready for analysis.

## Installation

```r
# install.packages("devtools")
# devtools::install_github("yueywangse/PokemonAnalyzer")
```

Imports are CRAN-only (`httr`, `jsonlite`), so Windows/Mac/Linux are supported out of the box. PokeAPI does not require an API key.

## Usage

```r
library(pokeapiclient)

# List first 10 Pokemon
pokeapi_list_pokemon(limit = 10)

# Fetch a single Pokemon with structured fields
pika <- pokeapi_get_pokemon("pikachu")
pika$types        # character vector
head(pika$stats)  # data frame of base stats

# Retrieve move metadata
move <- pokeapi_get_move("thunderbolt")
move$power
move$effect
```

## Package structure
- `R/` - HTTP helper (`pokeapi_request`), list/detail/move functions, parsing utilities
- `tests/` - unit tests for validation/parsing + online integration tests guarded by `skip_if_offline()`
- `vignettes/` - walkthrough from install to plotting a stat distribution
- `.github/workflows/` - GitHub Actions runner for `R CMD check` on Windows, macOS, and Linux
- `LICENSE`, `CODE_OF_CONDUCT.md`, `CONTRIBUTING.md` - required hygiene files

## Running tests locally
```r
# Unit tests
> testthat::test_dir("tests/testthat")

# Full check (runs online tests when internet is available)
> devtools::check()
```

## Notes on best practices
- Centralized HTTP helper for consistent headers, timeouts, and error handling (retries once on 429).
- Clear return types: data frames for lists, structured lists for individual resources.
- Friendly errors for bad inputs or HTTP failures.
- Pagination exposed via `limit`/`offset`; input validation keeps values sensible.
