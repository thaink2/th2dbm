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
    shiny::textInput(ns("db_name"), "Database Name", value = Sys.getenv('TH_DATABASE')),
    shiny::passwordInput(ns("db_host"), "Host", value = Sys.getenv('TH_HOST')),
    shiny::numericInput(ns("db_port"), "Port", value = Sys.getenv('TH_PORT')),
    shiny::passwordInput(ns("db_user"), "Username", value = Sys.getenv('TH_DB_USERNAME')),
    shiny::passwordInput(ns("db_password"), "Password", value = Sys.getenv('TH_DB_PASSWORD') ),
    shiny::textInput(ns("db_schema"), "Schema", value = "public"),
    actionButton(ns("connect_db"), "Connect to Database", style = add_button_theme(), icon = icon("link")),
    actionButton(ns("disconnect_db"), "Disconnect from Database", style = add_button_theme(), icon = icon("unlink"))
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
  shiny::moduleServer(id, function(input, output, session) {
    con <- shiny::reactiveVal(NULL)

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
