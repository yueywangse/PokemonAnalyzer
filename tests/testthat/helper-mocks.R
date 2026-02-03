# helper-mocks.R
# Utilities for mocking package functions without extra dependencies.

with_mocked_in_namespace <- function(pkg, ..., code) {
  ns <- asNamespace(pkg)
  bindings <- list(...)

  old <- list()
  was_locked <- logical(length(bindings))
  names(was_locked) <- names(bindings)

  for (nm in names(bindings)) {
    has_binding <- exists(nm, envir = ns, inherits = FALSE)
    old[[nm]] <- if (has_binding) get(nm, envir = ns, inherits = FALSE) else NULL

    was_locked[[nm]] <- has_binding && bindingIsLocked(nm, ns)
    if (was_locked[[nm]]) unlockBinding(nm, ns)

    assign(nm, bindings[[nm]], envir = ns)
  }

  on.exit({
    for (nm in names(bindings)) {
      if (is.null(old[[nm]])) {
        rm(list = nm, envir = ns)
      } else {
        assign(nm, old[[nm]], envir = ns)
      }
      if (was_locked[[nm]]) lockBinding(nm, ns)
    }
  }, add = TRUE)

  force(code)
}

mock_pokemon <- function(
  name,
  types,
  hp = 100, attack = 100, defense = 100,
  sattack = 100, sdefense = 100,
  sprite = NA_character_
) {
  list(
    name = tolower(name),
    types = tolower(types),
    stats = data.frame(
      stat = c("hp", "attack", "defense", "special-attack", "special-defense"),
      base_stat = c(hp, attack, defense, sattack, sdefense),
      stringsAsFactors = FALSE
    ),
    sprite = sprite
  )
}
