library(jsonlite)

load_pokemon_data <- function() {
  fromJSON("pokemon_stats.json")
}

get_pokemon_row <- function(name, data = load_pokemon_data()) {
  idx <- match(tolower(name), tolower(data$name))
  if (is.na(idx)) stop("Pokemon not found: ", name, call. = FALSE)
  data[idx, ]
}

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
    if (dt %in% info$zero) mult <- mult * 0
    else if (dt %in% info$double) mult <- mult * 2
    else if (dt %in% info$half) mult <- mult * 0.5
  }
  mult
}

battle_score <- function(p, opp) {
  my_types <- unlist(p$type)
  opp_types <- unlist(opp$type)
  
  # Best type multiplier you can get using any of your types
  best_mult <- max(vapply(my_types, type_multiplier, numeric(1), defending_types = opp_types))
  
  # Choose physical or special based on what hits harder relative to opponent bulk
  phys <- p$attack / opp$defense
  spec <- p$sattack / opp$sdefense
  offense <- max(phys, spec) * best_mult
  
  # Simple bulk proxy
  bulk <- p$hp * ((p$defense + p$sdefense) / 2)
  
  # Combine (weights are tweakable)
  offense^1.2 * (bulk^0.6)
}

pokemon_matchup <- function(yours, opponent) {
  data <- load_pokemon_data()
  p1 <- get_pokemon_row(yours, data)
  p2 <- get_pokemon_row(opponent, data)
  
  s1 <- battle_score(p1, p2)
  s2 <- battle_score(p2, p1)
  
  # Convert scores to a "win probability" using a logistic transform
  # (bigger k => more decisive outcomes)
  k <- 1.2
  p_win <- 100 / (1 + exp(-k * (log(s1) - log(s2))))
  favored_pokemon <- if (p_win > 50) {
  p1$name
} else if (p_win < 50) {
  p2$name
} else {
  "tie"
}
  data.frame(
    your_pokemon = p1$name,
    opponent_pokemon = p2$name,
    your_score = s1,
    opponent_score = s2,
    percent_win_chance = p_win,
    favored_pokemon = favored_pokemon,
    stringsAsFactors = FALSE
  )
}

# pokemon_matchup("magikarp", "Regieleki")