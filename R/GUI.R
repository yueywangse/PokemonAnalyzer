# use wrapper functions from the package (sourced locally to avoid install step)
source("R/api_client.R")
source("R/pokemon.R")
source("R/pokemon_matchup.R")

# code below sets up the ui elements for the GUI, not intended for independent use
ui <- shiny::fluidPage(
  shinyjs::useShinyjs(), # initializes shinyjs for ui
  shiny::tags$head( # sets the css parameters for the ui
    shiny::tags$style(shiny::HTML("
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
  shiny::fluidRow( # the title of the ui
    shiny::column(
      12,
      align = "center",
      shiny::h2("PokemonAnalyzer Battle Calculator")
    )
  ),
  shiny::br(),
  shiny::fluidRow( # where to input the gemini api key
    shiny::column(
      12,
      align = "center",
      shiny::textInput("input_main", "Gemini API Key:")
    ),
  ),
  shiny::fluidRow( # Titles of the pokemon entered
    shiny::column(6, shiny::h3("Your Pokemon:")),
    shiny::column(6, shiny::h3("Enemy Pokemon:"))
  ),
  shiny::fluidRow( # Where the pokemon image asset will be loaded when entered
    shiny::column(6, shiny::uiOutput("img_a")),
    shiny::column(6, shiny::uiOutput("img_b"))
  ),
  shiny::fluidRow( # Fields to input pokemon names
    shiny::column(6, shiny::textInput("input_a", "Input:")),
    shiny::column(6, shiny::textInput("input_b", "Input:"))
  ),
  shiny::br(),
  shiny::fluidRow( # Button to trigger calculation and ai prompt
    shiny::column(
      12,
      align = "center",
      shiny::actionButton("start", "Start Calculating")
    )
  ),
  shiny::br(),
  shiny::fluidRow( # Our mathematical analysis of the matchup
    shiny::column(
      6,
      offset = 3,
      shiny::tags$label("Our Suggestion:"),
      shiny::uiOutput("output_c")
    )
  ),
  shiny::br(),
  shiny::fluidRow( # AI suggestion of the matchup
    shiny::column(
      6,
      offset = 3,
      shiny::tags$label("AI Suggestion:"),
      shiny::textOutput("output_d")
    )
  ),
  shiny::br(), shiny::br()
)

# backend code to use with ui, not intended for independent use
server <- function(input, output, session) {
  placeholder_src <- "https://via.placeholder.com/300x300?text=Pokemon" #base url for pokemon images

  render_img <- function(pokemon_obj = NULL) { # function to generate correct url depending on pokmeon name
    if (is.null(pokemon_obj) || inherits(pokemon_obj, "error")) {
      return(shiny::img(src = placeholder_src, width = "80%"))
    }

    src <- pokemon_obj$sprite %||% NA_character_
    if (is.null(src) || is.na(src) || identical(src, "")) src <- placeholder_src
    shiny::img(src = src, width = "80%")
  }

  output$img_a <- shiny::renderUI({ render_img() })
  output$img_b <- shiny::renderUI({ render_img() })

  shiny::observeEvent(input$start, { # button event to trigger start of calculations
    shinyjs::disable("start")
    on.exit(shinyjs::enable("start"), add = TRUE)

    shiny::req(input$input_a, input$input_b)

    pokemon_a <- tryCatch(pokeapi_get_pokemon(input$input_a), error = identity)
    pokemon_b <- tryCatch(pokeapi_get_pokemon(input$input_b), error = identity)

    output$img_a <- shiny::renderUI({ render_img(pokemon_a) })
    output$img_b <- shiny::renderUI({ render_img(pokemon_b) })

    # update of ui on button press
    output$output_c <- shiny::renderUI({
      if (inherits(pokemon_a, "error")) {
        return(shiny::tags$div(class = "form-control wrap-text", paste("Lookup error:", pokemon_a$message)))
      }
      if (inherits(pokemon_b, "error")) {
        return(shiny::tags$div(class = "form-control wrap-text", paste("Lookup error:", pokemon_b$message)))
      }

      # locally calculated odds of matchup
      res <- tryCatch(pokemon_matchup(pokemon_a, pokemon_b), error = identity)

      if (inherits(res, "error")) {
        return(shiny::tags$div(
          class = "form-control wrap-text",
          paste("Matchup error:", res$message)
        ))
      }

      shiny::tags$div(
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

      output$output_d <- shiny::renderText(AIAnswer)
    } else {
      output$output_d <- shiny::renderText("Enter a Gemini API key to see AI suggestion.")
    }
  })
}

# connects ui to server code for backend
run_app <- function() {
  if (!interactive()) {
    stop("run_app() must be called in an interactive R session", call. = FALSE)
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
