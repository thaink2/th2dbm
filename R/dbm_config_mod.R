#' UI for database configuration
#'
#' This function creates the user interface for the database configuration module.
#'
#' @param id The module ID
#' @return A bs4Dash box containing database configuration inputs
#' @export
#' @importFrom bs4Dash box
#' @importFrom shiny NS textInput numericInput passwordInput actionButton
dbConfigUI <- function(id) {
  ns <- shiny::NS(id)
  bs4Dash::box(
    title = "Database Configuration",
    width = 12,
    status = "primary",
    solidHeader = TRUE,
    background = "white",
    fluidPage(
      fluidRow(
        column(width = 3, textInput(ns("db_name"), "Database Name", value = Sys.getenv('TH_DATABASE'))),
        column(width = 3, passwordInput(ns("db_host"), "Host", value = Sys.getenv('TH_HOST'))),
        column(width = 2, numericInput(ns("db_port"), "Port", value = as.numeric(Sys.getenv('TH_PORT')))),
        column(width = 3, passwordInput(ns("db_user"), "Username", value = Sys.getenv('TH_DB_USERNAME'))),
        column(width = 3, passwordInput(ns("db_password"), "Password", value = Sys.getenv('TH_DB_PASSWORD'))),
        column(width = 2, textInput(ns("db_schema"), "Schema", value = "public"))
      ),
      fluidRow(
        column(width = 3, uiOutput(ns("connect_db_btn"))),
        column(width = 3, uiOutput(ns("disconnect_db_btn")))
      )
    )
  )
}

#' Server function for database configuration
#'
#' This function contains the server logic for the database configuration module.
#'
#' @param id The module ID
#' @return A list containing the database connection and schema
#' @export
#' @importFrom shiny moduleServer reactive observeEvent showNotification
#' @importFrom bs4Dash updateTabItems
#' @importFrom th2dbm connect_to_database
dbConfigServer <- function(id, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    con <- reactiveVal(NULL)
    ns <- session$ns
    output$connect_db_btn <- renderUI({
      req(input$db_name, input$db_host, input$db_user, input$db_password, input$db_port, input$db_schema)
      actionButton(ns("connect_db"), "Connect to Database", style = add_button_theme(), icon = icon("link"))
    })

    output$disconnect_db_btn <- renderUI({
      req(con())
      actionButton(ns("disconnect_db"), "Disconnect from Database", style = add_button_theme(), icon = icon("unlink"))
    })

    shiny::observeEvent(input$connect_db, {
      db_params <- list(
        host = input$db_host,
        database = input$db_name,
        username = input$db_user,
        password = input$db_password,
        port = input$db_port
      )

      tryCatch({

        new_con <- th2dbm::connect_to_database(db_params, db_type = "postgresql")
        con(new_con)
        Sys.setenv(TH_DATABASE = input$db_name)
        Sys.setenv(TH_HOST = input$db_host)
        Sys.setenv(TH_PORT = as.character(input$db_port))
        Sys.setenv(TH_DB_USERNAME = input$db_user)
        Sys.setenv(TH_DB_PASSWORD = input$db_password)
        shiny::showNotification("Connected to database successfully", type = "message")
        bs4Dash::updateTabItems(parent_session, "sidebar", selected = "tables")
      }, error = function(e) {
        shiny::showNotification(paste("Error connecting to database:", e$message), type = "error")
        print(paste("Error connecting to database:", e$message))
      })
    })

    shiny::observeEvent(input$disconnect_db, {
      if (!is.null(con())) {
        DatabaseConnector::disconnect(con())
        con(NULL)
        shiny::showNotification("Disconnected from database", type = "message")
      }
    })

    return(list(
      con = con,
      schema = shiny::reactive(input$db_schema)
    ))
  })
}
