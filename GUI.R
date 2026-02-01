library(shiny)
library(shinyjs)

library(httr2)
library(jsonlite)

# local battle logic + dataset loader
source("pokemon_matchup.r")
source("get_pokemon_id.r")

call_gemini <- function(prompt, apikey) {
  req <- request(
    "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent"
  ) |>
    req_url_query(key = apikey) |>
    req_method("POST") |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(list(
      contents = list(
        list(
          parts = list(
            list(text = prompt)
          )
        )
      )
    ))

  resp <- req_perform(req)
  parsed <- resp_body_json(resp)
  parsed$candidates[[1]]$content$parts[[1]]$text
}

get_picture_url <- function(name) {
  if (!file.exists("pokemon_stats.json")) return(NA_character_)
  data <- load_pokemon_data()
  idx <- match(tolower(name), tolower(data$name))
  if (is.na(idx)) return(NA_character_)
  data$picture[[idx]]
}


ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
    .wrap-text {
      white-space: pre-wrap;
      word-wrap: break-word;
      overflow-wrap: break-word;
      height: auto !important;
      min-height: 50px;
      overflow-y: visible;
    }
  "))
  ),
  fluidRow(
    column(
      12,
      align = "center",
      h2("PokemonAnalyzer Battle Calculator")
    )
  ),
  br(),
  fluidRow(
    column(
      12,
      align = "center",
      textInput("input_main", "Gemini API Key:")
    ),
  ),
  fluidRow(
    column(6, h3("Your Pokemon:")),
    column(6, h3("Enemy Pokemon:"))
  ),
  fluidRow(
    column(6, uiOutput("img_a")),
    column(6, uiOutput("img_b"))
  ),
  fluidRow(
    column(6, textInput("input_a", "Input:")),
    column(6, textInput("input_b", "Input:"))
  ),
  br(),
  fluidRow(
    column(
      12,
      align = "center",
      actionButton("start", "Start Calculating")
    )
  ),
  br(),
  fluidRow(
    column(
      6,
      offset = 3,
      tags$label("Our Suggestion:"),
      uiOutput("output_c")
    )
  ),
  br(),
  fluidRow(
    column(
      6,
      offset = 3,
      tags$label("AI Suggestion:"),
      textOutput("output_d")
    )
  ),
  br(), br()
)


server <- function(input, output, session) {
  placeholder_src <- "https://via.placeholder.com/300x300?text=Pokemon"

  render_img <- function(name) {
    if (is.null(name) || !nzchar(name)) {
      return(img(src = placeholder_src, width = "80%"))
    }
    src <- get_picture_url(name)
    if (is.na(src) || src == "") {
      img(src = placeholder_src, width = "80%")
    } else {
      img(src = src, width = "80%")
    }
  }

  output$img_a <- renderUI({ render_img(input$input_a) })
  output$img_b <- renderUI({ render_img(input$input_b) })

  observeEvent(input$start, {
    shinyjs::disable("start")
    on.exit(shinyjs::enable("start"), add = TRUE)

    # our locally computed matchup suggestion
    output$output_c <- renderUI({
      req(input$input_a, input$input_b)

      if (!file.exists("pokemon_stats.json")) {
        return(tags$div(
          class = "form-control wrap-text",
          "pokemon_stats.json not found. Run get-pokemon.r first to build the dataset."
        ))
      }

      res <- tryCatch(
        pokemon_matchup(input$input_a, input$input_b),
        error = identity
      )

      if (inherits(res, "error")) {
        return(tags$div(
          class = "form-control wrap-text",
          paste("Matchup error:", res$message)
        ))
      }

      tags$div(
        class = "form-control wrap-text",
        paste0(
          "Favored: ", res$favored_pokemon,
          " (", sprintf("%.1f", res$percent_win_chance), "% win chance for ", res$your_pokemon, ")"
        )
      )
    })

    apikey <- trimws(input$input_main)

    if (apikey != "") {
      prompt <- paste0(
        "My pokemon is ", input$input_a,
        ". The enemy pokemon is ", input$input_b,
        ". What is your suggestion in less than 100 words?"
      )

      # enforce spacing between calls
      Sys.sleep(6)

      AIAnswer <- tryCatch(
        call_gemini(prompt, apikey),
        error = function(e) paste("Gemini error:", e$message)
      )

      output$output_d <- renderText(AIAnswer)
    } else {
      output$output_d <- renderText("Enter a Gemini API key to see AI suggestion.")
    }
  })
}

shinyApp(ui, server)
