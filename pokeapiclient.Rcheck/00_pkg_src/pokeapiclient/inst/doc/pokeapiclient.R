## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE
)

has_net <- function() {
  requireNamespace("curl", quietly = TRUE) && curl::has_internet()
}

## ----eval=FALSE---------------------------------------------------------------
# # install.packages("devtools")
# # devtools::install_github("yueywangse/PokemonAnalyzer")

## ----load, eval=FALSE---------------------------------------------------------
# library(PokemonAnalyzer)

## ----load_all, eval=FALSE-----------------------------------------------------
# devtools::load_all()

## ----list_pokemon, eval=FALSE-------------------------------------------------
# if (has_net()) {
#   head(pokeapi_list_pokemon(limit = 10))
# }

## ----fetch_pokemon, eval=FALSE------------------------------------------------
# if (has_net()) {
#   pika <- pokeapi_get_pokemon("pikachu")
#   pika$name
#   pika$types
#   head(pika$stats)
# }

## ----fetch_moves, eval=FALSE--------------------------------------------------
# if (has_net()) {
#   mv <- pokeapi_get_move("thunderbolt")
#   mv$power
#   mv$accuracy
#   mv$type
#   mv$damage_class
# }

## ----gui_source, eval=FALSE---------------------------------------------------
# run_app()

## ----eval=FALSE---------------------------------------------------------------
# # run_team_console()

## ----bst_plot, eval=FALSE-----------------------------------------------------
# if (has_net()) {
#   plot_bst_allocation(c("stakataka", "regieleki", "cleffa", "eevee"))
# }

## ----atk_speed_plot, eval=FALSE-----------------------------------------------
#  if (has_net()) {
#   plot_attack_vs_speed(c("stakataka", "regieleki", "cleffa", "eevee"))
# }

