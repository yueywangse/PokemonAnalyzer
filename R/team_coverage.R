# team_coverage.R
# Analyze a team of 1-6 Pokemon for type weaknesses/resistances.

# ---- Load your package code during development ----
# If you're running this inside the package repo:
# install.packages("devtools")
# devtools::load_all()

# If installed:
# library(pokeapiclient)

# ---- Type chart (same structure as your matchup code) ----
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

type_multiplier <- function(attacking_type, defending_types) {
  attacking_type <- tolower(attacking_type)
  defending_types <- tolower(defending_types)

  info <- type_chart[[attacking_type]]
  if (is.null(info)) stop("Unknown attacking type: ", attacking_type, call. = FALSE)

  mult <- 1
  for (dt in defending_types) {
    if (dt %in% info$zero)      mult <- mult * 0
    else if (dt %in% info$double) mult <- mult * 2
    else if (dt %in% info$half)   mult <- mult * 0.5
  }
  mult
}

# ---- Fetch & normalize types from your pokeapi object ----
get_types_from_api <- function(p) {
  # In your package, pokeapi_get_pokemon() returns p$types as character vector already
  tolower(as.character(p$types))
}

analyze_team <- function(pokemon_names, fetch_fun = pokeapi_get_pokemon) {
  pokemon_names <- tolower(trimws(pokemon_names))
  pokemon_names <- pokemon_names[nzchar(pokemon_names)]
  pokemon_names <- unique(pokemon_names)

  if (length(pokemon_names) < 1 || length(pokemon_names) > 6) {
    stop("Provide between 1 and 6 Pokemon names.", call. = FALSE)
  }

  # Fetch pokemon objects
  team <- lapply(pokemon_names, function(nm) {
    p <- pokeapi_get_pokemon(nm)
    list(name = p$name, types = get_types_from_api(p))
  })

  all_types <- names(type_chart)

  # 1) Defensive multiplier matrix: rows = attacking type, cols = team member
  mult_mat <- sapply(team, function(mem) {
    vapply(all_types, type_multiplier, numeric(1), defending_types = mem$types)
  })
  colnames(mult_mat) <- vapply(team, `[[`, character(1), "name")
  rownames(mult_mat) <- all_types

  # 2) Types with no team resistance/immunity: nobody takes <= 0.5x
  types_with_no_team_resistance <- rownames(mult_mat)[
    apply(mult_mat, 1, function(row_mult) !any(row_mult <= 0.5))
  ]

  # 3) Offensive multipliers (table): rows = DEFENDING type, cols = team member
  # Values are the best multiplier each Pokemon can achieve using only its own typing
  # (ignores STAB bonus; this is just type effectiveness).
  offensive_multiplier_matrix <- sapply(team, function(mem) {
    vapply(all_types, function(def_type) {
      if (length(mem$types) == 0) return(1)
      max(vapply(mem$types, type_multiplier, numeric(1), defending_types = def_type))
    }, numeric(1))
  })
  colnames(offensive_multiplier_matrix) <- vapply(team, `[[`, character(1), "name")
  rownames(offensive_multiplier_matrix) <- all_types

  # Team-level best vs each single defending type (max across team)
  offensive_multiplier <- apply(offensive_multiplier_matrix, 1, max)

  list(
    defensive_multiplier_matrix = mult_mat,
    types_with_no_team_resistance = types_with_no_team_resistance,
    offensive_multiplier_matrix = offensive_multiplier_matrix,
    offensive_multiplier = offensive_multiplier
  )
}

# ---- Example usage (interactive) ----
# res <- analyze_team(c("charizard", "gengar", "gyarados", "excadrill"))
# res$summary
# res$types_with_no_team_resistance
# res$multiplier_matrix