# Contributing

Thanks for helping improve `pokeapiclient`!

## Workflow
1. Fork or create a feature branch from `main`.
2. Make small, focused commits with clear messages.
3. Add/adjust tests for any behavior change.
4. Run `R CMD check` (or `devtools::check()`) locally if possible.
5. Open a Pull Request; link issues, describe changes, and note testing performed.
6. Address review comments; avoid force-push unless asked.

## Coding standards
- Follow tidyverse-style naming: snake_case for functions, noun-based for data, verb-based for actions.
- Keep public return types consistent (lists or data frames, not mixed).
- Centralize HTTP logic; prefer helpers over copy/paste.
- Handle errors gracefully with informative messages.

## Tests
- Unit tests live in `tests/testthat/`.
- Integration tests that hit the real API should call `skip_if_offline("pokeapi.co")` to remain CI-friendly.

## Documentation
- Use roxygen2-style comments for exports.
- Update README and vignettes when user-facing behavior changes.

## Conduct
- Be respectful. See `CODE_OF_CONDUCT.md`.
