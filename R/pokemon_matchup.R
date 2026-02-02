# Shared Pokemon type chart (Gen 6+)
# Used by pokemon_matchup() and team coverage utilities.

type_chart <- list(
  normal   = list(double=c(), half=c("rock","steel"), zero=c("ghost")),
  fire     = list(double=c("grass","ice","bug","steel"), half=c("fire","water","rock","dragon"), zero=c()),
  water    = list(double=c("fire","ground","rock"), half=c("water","grass","dragon"), zero=c()),
  electric = list(double=c("water","flying"), half=c("electric","grass","dragon"), zero=c("ground")),
  grass    = list(double=c("water","ground","rock"), half=c("fire","grass","poison","flying","bug","dragon","steel"), zero=c()),
  ice      = list(double=c("grass","ground","flying","dragon"), half=c("fire","water","ice","steel"), zero=c()),
  fighting = list(double=c("normal","ice","rock","dark","steel"), half=c("poison","flying","psychic","bug","fairy"), zero=c("ghost")),
  poison   = list(double=c("grass","fairy"), half=c("poison","ground","rock","ghost"), zero=c("steel")),
  ground   = list(double=c("fire","electric","poison","rock","steel"), half=c("grass","bug"), zero=c("flying")),
  flying   = list(double=c("grass","fighting","bug"), half=c("electric","rock","steel"), zero=c()),
  psychic  = list(double=c("fighting","poison"), half=c("psychic","steel"), zero=c("dark")),
  bug      = list(double=c("grass","psychic","dark"), half=c("fire","fighting","poison","flying","ghost","steel","fairy"), zero=c()),
  rock     = list(double=c("fire","ice","flying","bug"), half=c("fighting","ground","steel"), zero=c()),
  ghost    = list(double=c("psychic","ghost"), half=c("dark"), zero=c("normal")),
  dragon   = list(double=c("dragon"), half=c("steel"), zero=c("fairy")),
  dark     = list(double=c("psychic","ghost"), half=c("fighting","dark","fairy"), zero=c()),
  steel    = list(double=c("ice","rock","fairy"), half=c("fire","water","electric","steel"), zero=c()),
  fairy    = list(double=c("fighting","dragon","dark"), half=c("fire","poison","steel"), zero=c())
)

# Remove any NULL placeholders (safety)
type_chart <- type_chart[!vapply(type_chart, is.null, logical(1))]

# Compute type effectiveness multiplier for an attacking type vs one or more defending types.
# defending_types can be a character vector like c("fire","flying").
# Returns a numeric multiplier in {0, 0.25, 0.5, 1, 2, 4}.

type_multiplier <- function(attacking_type, defending_types) {
  attacking_type <- tolower(as.character(attacking_type))
  defending_types <- tolower(as.character(defending_types))

  info <- type_chart[[attacking_type]]
  if (is.null(info)) stop("Unknown attacking type: ", attacking_type, call. = FALSE)

  mult <- 1
  for (dt in defending_types) {
    if (dt %in% info$zero)        mult <- mult * 0
    else if (dt %in% info$double) mult <- mult * 2
    else if (dt %in% info$half)   mult <- mult * 0.5
  }
  mult
}

# Simple null-coalescing helper used in this file
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# --- Type chart + type_multiplier can stay exactly as you have them ---

# Robustly extract type names from the PokéAPI object
get_types_api <- function(p) {
  # your pokeapi_get_pokemon() seems to return p$types as a character vector
  # but we make it robust in case it's a list/data.frame
  ty <- p$types

  if (is.null(ty)) return(character(0))

  if (is.character(ty)) return(tolower(ty))

  if (is.data.frame(ty)) {
    if ("type" %in% names(ty)) return(tolower(as.character(ty$type)))
    if ("name" %in% names(ty)) return(tolower(as.character(ty$name)))
  }

  if (is.list(ty)) {
    out <- vapply(ty, function(e) {
      if (is.character(e) && length(e) == 1) return(e)
      if (is.list(e) && !is.null(e$name)) return(e$name)
      if (is.list(e) && !is.null(e$type) && is.list(e$type) && !is.null(e$type$name)) return(e$type$name)
      NA_character_
    }, character(1))
    out <- out[!is.na(out)]
    return(tolower(out))
  }

  tolower(as.character(ty))
}

# Robustly extract base stats from the PokéAPI object
# expects p$stats to be a data.frame with columns stat + base_stat
get_stats_api <- function(p) {
  if (is.null(p$stats) || !is.data.frame(p$stats)) {
    stop("PokéAPI object missing $stats data.frame.", call. = FALSE)
  }
  if (!all(c("stat", "base_stat") %in% names(p$stats))) {
    stop("PokéAPI object $stats must have columns 'stat' and 'base_stat'.", call. = FALSE)
  }

  lookup <- setNames(p$stats$base_stat, tolower(p$stats$stat))

  # normalize names that commonly appear from PokéAPI
  hp      <- unname(lookup[["hp"]])
  attack  <- unname(lookup[["attack"]])
  defense <- unname(lookup[["defense"]])
  sattack <- unname(lookup[["special-attack"]])
  sdefense<- unname(lookup[["special-defense"]])

  if (any(is.na(c(hp, attack, defense, sattack, sdefense)))) {
    stop("Missing one or more required stats (hp/attack/defense/special-attack/special-defense).", call. = FALSE)
  }

  list(hp = hp, attack = attack, defense = defense, sattack = sattack, sdefense = sdefense)
}

# Score using typing + base stats only
battle_score_api <- function(p, opp) {
  my_types  <- get_types_api(p)
  opp_types <- get_types_api(opp)

  # best type multiplier attacker can get using any of its types
  best_mult <- if (length(my_types) == 0) 1 else {
    max(vapply(my_types, type_multiplier, numeric(1), defending_types = opp_types))
  }

  ps <- get_stats_api(p)
  os <- get_stats_api(opp)

  phys <- ps$attack / max(1, os$defense)
  spec <- ps$sattack / max(1, os$sdefense)
  offense <- max(phys, spec) * best_mult

  bulk <- ps$hp * ((ps$defense + ps$sdefense) / 2)

  offense^1.2 * (bulk^0.6)
}

# --- NEW pokemon_matchup: takes PokéAPI objects ---
pokemon_matchup <- function(yours, opponent) {
  # Optional: allow passing names by fetching
  if (is.character(yours))   yours <- pokeapi_get_pokemon(yours)
  if (is.character(opponent)) opponent <- pokeapi_get_pokemon(opponent)

  s1 <- battle_score_api(yours, opponent)
  s2 <- battle_score_api(opponent, yours)

  k <- 1.2
  p_win <- 100 / (1 + exp(-k * (log(s1) - log(s2))))

  your_name <- yours$name %||% "your_pokemon"
  opp_name  <- opponent$name %||% "opponent_pokemon"

  favored <- if (p_win > 50) your_name else if (p_win < 50) opp_name else "tie"

  data.frame(
    your_pokemon = your_name,
    opponent_pokemon = opp_name,
    percent_win_chance = p_win,
    favored_pokemon = favored,
    your_types = paste(get_types_api(yours), collapse = "/"),
    opponent_types = paste(get_types_api(opponent), collapse = "/"),
    stringsAsFactors = FALSE
  )
}