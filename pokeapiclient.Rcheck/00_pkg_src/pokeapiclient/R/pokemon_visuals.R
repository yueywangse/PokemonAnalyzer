# library(PokemonAnalyzer)
# Simple visualization helpers for PokemonAnalyzer

plot_attack_vs_speed <- function(pokemon_names) {
  # Fetch pokemon data
  pokemon_data <- lapply(pokemon_names, pokeapi_get_pokemon)
  # Build tidy stat dataframe
  stat_df <- do.call(rbind, lapply(pokemon_data, function(p) {
    data.frame(
      name  = p$name,
      type  = paste(p$types, collapse = "/"),
      stat  = p$stats$stat,
      value = p$stats$base_stat,
      stringsAsFactors = FALSE
    )
  }))
  # Keep only attack + speed
  atk_spd <- subset(stat_df, stat %in% c("attack", "speed"))
  # Wide format
  atk_spd_wide <- stats::reshape(
    atk_spd,
    timevar = "stat",
    idvar = c("name", "type"),
    direction = "wide"
  )
  # Plot
  ggplot2::ggplot(
    atk_spd_wide,
    ggplot2::aes(
      x = value.attack,
      y = value.speed,
      color = type,
      label = name
    )
  ) +
    ggplot2::geom_point(size = 3) +
    ggrepel::geom_text_repel(show.legend = FALSE) +
    ggplot2::labs(
      x = "Attack",
      y = "Speed",
      title = "Pokemon: Attack vs Speed"
    ) +
    ggplot2::theme_minimal()
}

plot_bst_allocation <- function(pokemon_names) {
  pokemon_data <- lapply(pokemon_names, pokeapi_get_pokemon)

  stat_df <- do.call(rbind, lapply(pokemon_data, function(p) {
    data.frame(
      name  = p$name,
      type  = paste(p$types, collapse = "/"),
      stat  = tolower(p$stats$stat),
      value = as.numeric(p$stats$base_stat),
      stringsAsFactors = FALSE
    )
  }))

  stat_map <- c(
    "hp" = "HP",
    "attack" = "Atk",
    "defense" = "Def",
    "special-attack" = "SpA",
    "special-defense" = "SpD",
    "speed" = "Spe"
  )

  stat_df$stat <- stat_map[stat_df$stat]
  stat_df <- stat_df[!is.na(stat_df$stat), ]

  stat_df$stat <- factor(
    stat_df$stat,
    levels = c("HP", "Atk", "Def", "SpA", "SpD", "Spe")
  )

  # Order Pokemon by total BST
  totals <- stats::aggregate(value ~ name, data = stat_df, sum)
  totals <- totals[order(totals$value), ]
  stat_df$name <- factor(stat_df$name, levels = totals$name)

  ggplot2::ggplot(stat_df, ggplot2::aes(x = name, y = value, fill = stat)) +
    ggplot2::geom_col() +
    ggplot2::labs(
      title = "Base Stat Allocation (BST Breakdown)",
      x = "Pokemon",
      y = "Base Stat Points",
      fill = "Stat"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}
