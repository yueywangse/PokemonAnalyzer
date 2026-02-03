pkgname <- "pokeapiclient"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "pokeapiclient-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('pokeapiclient')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("call_gemini")
### * call_gemini

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: call_gemini
### Title: Call the Gemini API
### Aliases: call_gemini

### ** Examples

## Not run: 
##D call_gemini("Explain type advantages in Pokemon battles.", Sys.getenv("GEMINI_API_KEY"))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("call_gemini", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_pokemon_id")
### * get_pokemon_id

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_pokemon_id
### Title: Look up a Pokemon's ID and type from the cached stats file
### Aliases: get_pokemon_id

### ** Examples

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



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_pokemon_id", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pokeapi_get_move")
### * pokeapi_get_move

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pokeapi_get_move
### Title: Retrieve move details
### Aliases: pokeapi_get_move

### ** Examples

# pokeapi_get_move("thunderbolt")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pokeapi_get_move", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pokeapi_get_pokemon")
### * pokeapi_get_pokemon

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pokeapi_get_pokemon
### Title: Retrieve a single Pokemon
### Aliases: pokeapi_get_pokemon

### ** Examples

# Fetch Pikachu
# pokeapi_get_pokemon("pikachu")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pokeapi_get_pokemon", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pokeapi_list_pokemon")
### * pokeapi_list_pokemon

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pokeapi_list_pokemon
### Title: List Pokemon
### Aliases: pokeapi_list_pokemon

### ** Examples

## No test: 
if (curl::has_internet()) {
  pokeapi_list_pokemon(limit = 5)
}
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pokeapi_list_pokemon", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pokeapi_set_base_url")
### * pokeapi_set_base_url

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pokeapi_set_base_url
### Title: Set the base URL for PokeAPI requests
### Aliases: pokeapi_set_base_url

### ** Examples

# Point at a mirror
pokeapi_set_base_url("https://pokeapi.co/api/v2")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pokeapi_set_base_url", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
