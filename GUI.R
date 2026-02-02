library(shiny)
library(shinyjs)

library(httr2)

# use wrapper functions from the package (sourced locally to avoid install step)
source("R/api_client.R")
source("R/pokemon.R")
source("pokemon_matchup.r")

#' Call the Google Gemini API to Generate Text
#'
#' Sends a text prompt to the Google Gemini (gemini-2.5-flash) model and
#' returns the generated text response.
#'
#' This function uses the \pkg{httr2} package to make a POST request to the
#' Google Generative Language API. The API key is passed as a query parameter
#' and the prompt is sent as JSON in the request body.
#'
#' @param prompt A character string containing the text prompt to send to
#' the Gemini model.
#' @param apikey A character string containing your Google Gemini API key.
#'
#' @return A character string containing the generated text from the model.
#' If the API call is successful, this is the text of the first candidate
#' returned by the model.
#'
#' @details
#' The function calls the \code{gemini-2.5-flash} model via the
#' \code{generateContent} endpoint. Only the first candidate and first text
#' part of the response is returned.
#'
#' @seealso
#' \link[httr2]{request}, \link[httr2]{req_perform},
#' \link[httr2]{resp_body_json}
#'
#' @examples
#' \dontrun{
#' key <- Sys.getenv("GEMINI_API_KEY")
#' call_gemini("Explain unit testing in R", key)
#' }
#'
#' @export
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

  # below parses the response for the string we're looking for
  resp <- req_perform(req)
  parsed <- resp_body_json(resp)
  parsed$candidates[[1]]$content$parts[[1]]$text
}

# code below sets up the ui elements for the GUI, not intended for independent use
ui <- fluidPage(
  useShinyjs(), # initializes shinyjs for ui
  tags$head( # sets the css parameters for the ui
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
  fluidRow( # the title of the ui
    column(
      12,
      align = "center",
      h2("PokemonAnalyzer Battle Calculator")
    )
  ),
  br(),
  fluidRow( # where to input the gemini api key
    column(
      12,
      align = "center",
      textInput("input_main", "Gemini API Key:")
    ),
  ),
  fluidRow( # Titles of the pokemon entered
    column(6, h3("Your Pokemon:")),
    column(6, h3("Enemy Pokemon:"))
  ),
  fluidRow( # Where the pokemon image asset will be loaded when entered
    column(6, uiOutput("img_a")),
    column(6, uiOutput("img_b"))
  ),
  fluidRow( # Fields to input pokemon names
    column(6, textInput("input_a", "Input:")),
    column(6, textInput("input_b", "Input:"))
  ),
  br(),
  fluidRow( # Button to trigger calculation and ai prompt
    column(
      12,
      align = "center",
      actionButton("start", "Start Calculating")
    )
  ),
  br(),
  fluidRow( # Our mathematical analysis of the matchup
    column(
      6,
      offset = 3,
      tags$label("Our Suggestion:"),
      uiOutput("output_c")
    )
  ),
  br(),
  fluidRow( # AI suggestion of the matchup
    column(
      6,
      offset = 3,
      tags$label("AI Suggestion:"),
      textOutput("output_d")
    )
  ),
  br(), br()
)

# backend code to use with ui, not intended for independent use
server <- function(input, output, session) {
  placeholder_src <- "https://via.placeholder.com/300x300?text=Pokemon" #base url for pokemon images

  render_img <- function(pokemon_obj = NULL) { # function to generate correct url depending on pokmeon name
    if (is.null(pokemon_obj) || inherits(pokemon_obj, "error")) {
      return(img(src = placeholder_src, width = "80%"))
    }

    src <- pokemon_obj$sprite %||% NA_character_
    if (is.null(src) || is.na(src) || identical(src, "")) src <- placeholder_src
    img(src = src, width = "80%")
  }

  output$img_a <- renderUI({ render_img() })
  output$img_b <- renderUI({ render_img() })

  observeEvent(input$start, { # button event to trigger start of calculations
    shinyjs::disable("start")
    on.exit(shinyjs::enable("start"), add = TRUE)

    req(input$input_a, input$input_b)

    pokemon_a <- tryCatch(pokeapi_get_pokemon(input$input_a), error = identity)
    pokemon_b <- tryCatch(pokeapi_get_pokemon(input$input_b), error = identity)

    output$img_a <- renderUI({ render_img(pokemon_a) })
    output$img_b <- renderUI({ render_img(pokemon_b) })

    # update of ui on button press
    output$output_c <- renderUI({
      if (inherits(pokemon_a, "error")) {
        return(tags$div(class = "form-control wrap-text", paste("Lookup error:", pokemon_a$message)))
      }
      if (inherits(pokemon_b, "error")) {
        return(tags$div(class = "form-control wrap-text", paste("Lookup error:", pokemon_b$message)))
      }

      # locally calculated odds of matchup
      res <- tryCatch(pokemon_matchup(pokemon_a, pokemon_b), error = identity)

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

    if (apikey != "") { # option to allow a mode with no api key use
      prompt <- paste0( # generates gemini prompt
        "My pokemon is ", input$input_a,
        ". The enemy pokemon is ", input$input_b,
        ". What is your suggestion in less than 100 words?"
      )

      # enforce spacing between calls
      Sys.sleep(5)

      # calls gemini to get prompt
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

# connects ui to server code for backend
shinyApp(ui, server)
