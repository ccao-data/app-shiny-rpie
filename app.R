# Load necessary packages
library(ccao)
library(DBI)
library(dplyr)
library(DT)
library(emojifont)
library(glue)
library(noctua)
library(odbc)
library(openxlsx)
library(purrr)
library(readr)
library(shiny)
library(shinycssloaders)
library(stringr)


# Getting MSSQL credentials from the shinyproxy server
noctua_options(clear_cache = TRUE)
noctua_options(cache_size = 10)

AWS_ATHENA_CONN_NOCTUA <- dbConnect(
  noctua::athena(),
  aws_access_key_id = Sys.getenv("AWS_ACCESS_KEY_ID"),
  aws_secret_access_key = Sys.getenv("AWS_SECRET_ACCESS_KEY")
)

# gather necessary functions and data
source("code.r/retrieve_codes.R", local = TRUE)


# Grab version number and commit from environment
vcs_version <- Sys.getenv("VCS_VER")
vcs_commit_sha <- Sys.getenv("VCS_REF_SHORT")

# Define the UI object for the layout of the app
ui <- fluidPage(
  headerPanel(""),
  sidebarLayout(
    sidebarPanel(
      titlePanel("RPIE Code Retrieval"),
      glue("Version: {vcs_version} {vcs_commit_sha}"),
      hr(),
      textInput(
        "PIN",
        label = "Enter a 10 or 14-digit IC PIN:",
        placeholder = "15-01-203-008-0000"
      ),
      numericInput(
        "YEAR",
        label = "Enter Filing Year:",
        value = 2023
      ),
      hr(),
      HTML(
        glue("<b>Upload Criteria:</b> <br/>
             <span>&#8226;</span>
              Column containing PINs must be named 'PIN' <br/>
             <span>&#8226;</span>
              PIN column does <i>not</i> need to be clean <br/>
             <span>&#8226;</span> PINs can be 10 or 14 digits <br/>
             <span>&#8226;</span> One PIN per row <br/> <br/>")
      ),
      fileInput(
        "FILE",
        label = "Upload a List of PINs:",
        accept = c(".csv", ".xlsx"),
        placeholder = ".csv or .xlsx"
      )
    ),
    mainPanel(
      verticalLayout(
        shinycssloaders::withSpinner(
          htmlOutput("pin_output"),
          color = "#566ca9"
        ),
        htmlOutput("divider"),
        shinycssloaders::withSpinner(
          DT::dataTableOutput("table"),
          color = "#566ca9"
        )
      )
    )
  )
)

# Define the server function to handle backend behavior
server <- function(input, output) {
  # Main panel output, essentially just a formatted message
  output$pin_output <- eventReactive(
    {
      input$PIN
      input$YEAR
    },
    message(pinfo(AWS_ATHENA_CONN_NOCTUA, input$PIN, input$YEAR), input$YEAR),
    ignoreInit = TRUE
  )

  # Ingest the .csv or .xlsx file a user uploads based on the file extensions
  upload <- reactive({
    req(input$FILE)

    # Validate file is the right type
    validate(
      need(
        tools::file_ext(input$FILE$name) %in% c("csv", "xlsx"),
        "Invalid file; Please upload a .csv or .xlsx file"
      )
    )

    # Validate file is formatted properly for joining on RPIE codes
    validate(
      need(
        "PIN" %in% names(
          switch(tools::file_ext(input$FILE$name),
            xlsx = read.xlsx(input$FILE$datapath, sheet = 1),
            csv = read.csv(input$FILE$datapath)
          )
        ),
        "No 'PIN' column found in uploaded file"
      )
    )

    switch(tools::file_ext(input$FILE$name),
      xlsx = read.xlsx(input$FILE$datapath, sheet = 1),
      csv = read.csv(input$FILE$datapath)
    )
  })

  # Output cleaned data with RPIE codes to a datatable
  # where it can be viewed and downloaded
  output$table <- DT::renderDataTable(
    server = FALSE,
    {
      formatStyle(
        DT::datatable(
          clean_list(upload(), input$YEAR),
          rownames = FALSE,
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            # https://datatables.net/reference/option/dom Blfrtip
            dom = "<B<rt>ip>",
            buttons = list(
              list(
                extend = "csv",
                filename = paste0(gsub(".csv|.xlsx", "", input$FILE$name)),
                title = ""
              ),
              list(
                extend = "excel",
                filename = paste0(gsub(".csv|.xlsx", "", input$FILE$name)),
                title = ""
              ),
              list(
                extend = "copy",
                title = ""
              )
            )
          )
        ),
        columns = 2,
        fontFamily = "Consolas"
      )
    }
  )

  # Place a border between single and multiple PIN output if both are shown
  observeEvent(
    req(input$PIN, input$FILE),
    {
      output$divider <- renderText('<hr style="border-color: black;">')
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
