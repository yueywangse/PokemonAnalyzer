test_that("get_stats returns structured data without real HTTP calls", {
  pokemon_json <- jsonlite::toJSON(list(
    name = "pikachu",
    stats = list(
      list(base_stat = 35, stat = list(name = "hp")),
      list(base_stat = 55, stat = list(name = "attack")),
      list(base_stat = 40, stat = list(name = "defense")),
      list(base_stat = 50, stat = list(name = "special-attack")),
      list(base_stat = 50, stat = list(name = "special-defense"))
    ),
    moves = list(list(move = list(name = "thunder-shock", url = "http://example.com/move/1"))),
    types = list(list(type = list(name = "electric"))),
    sprites = list(other = list(`official-artwork` = list(front_default = "http://img/pika.png")))
  ), auto_unbox = TRUE)

  move_json <- jsonlite::toJSON(list(
    name = "thunder-shock",
    accuracy = 100,
    power = 40,
    damage_class = list(name = "special"),
    type = list(name = "electric")
  ), auto_unbox = TRUE)

  mock_GET <- function(url, ...) {
    list(url = url, json = if (grepl("move", url)) move_json else pokemon_json)
  }
  mock_content <- function(res, ..., encoding = "UTF-8") res$json
  mock_stop <- function(res) invisible(res)

  with_mocked_in_namespace(
    "pokeapiclient",
    GET = mock_GET,
    content = mock_content,
    stop_for_status = mock_stop,
    fromJSON = jsonlite::fromJSON,
    code = {
      res <- pokeapiclient:::get_stats("http://example.com/pokemon/25", max_moves = 1)
      expect_equal(res$name, "pikachu")
      expect_equal(res$type, "electric")
      expect_equal(res$hp, 35)
      expect_equal(res$moves[[1]]$name, "thunder-shock")
      expect_equal(res$moves[[1]]$damage_class, "special")
    }
  )
})
