library(shiny)
library(shinyjs)

library(httr2)
library(jsonlite)

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
      textInput("input_main", "Gemini API Key:")),
  ),
  
  fluidRow(
    column(6, h3("Your Pokemon:")),
    column(6, h3("Enemy Pokemon:"))
  ),
  
  fluidRow(
    column(6, img(src = "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/other/official-artwork/1.png", width = "80%")),
    column(6, img(src = "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/other/official-artwork/2.png", width = "80%"))
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
      uiOutput("output_d")
    )
  ),
  
  br(), br()
)



server <- function(input, output, session) {
  
  observeEvent(input$start, {
    
    shinyjs::disable("start")
    on.exit(shinyjs::enable("start"), add = TRUE)
    
    # show inputs
    output$output_c <- renderUI({
      tags$div(
        class = "form-control wrap-text",
        paste(
          "| A:", input$input_a,
          "| B:", input$input_b
        )
      )
    })
    
    apikey <- trimws(input$input_main)
    
    if(apikey != "") {
    
      prompt <- paste0(
        "My pokemon is ", input$input_a,
        ". The enemy pokemon is ", input$input_b,
        ". What is your suggestion in less than 100 words?"
      )
      
      # 👇 enforce spacing between calls
      Sys.sleep(6)
      
      AIAnswer <- tryCatch(
        call_gemini(prompt, input$input_main),
        error = function(e) paste("Gemini error:", e$message)
      )
      
      output$output_d <- renderUI({
        tags$div(
          class = "form-control wrap-text",
          AIAnswer
        )
      })
    
    }
    
  })
  
}

shinyApp(ui, server)