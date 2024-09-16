#' UI for creating a new table
#'
#' This function creates the user interface for the create table module.
#'
#' @param id The module ID
#' @return A bs4Dash box containing inputs for creating a new table
#' @export
#' @importFrom bs4Dash box
#' @importFrom shiny NS textInput numericInput uiOutput actionButton
createTableUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    bs4Dash::box(
      title = "Create New Table",
      textInput(ns("table_name"), "Table Name"),
      numericInput(ns("num_columns"), "Number of Columns", value = 1, min = 1),
      uiOutput(ns("column_inputs")),
      actionButton(ns("create_table"), "Create Table", style = add_button_theme(), icon = icon("table"))
    )
  )
}

#' Server function for creating a new table
#'
#' This function contains the server logic for the create table module.
#'
#' @param id The module ID
#' @param con Reactive expression containing the database connection
#' @param schema Reactive expression containing the database schema
#' @return None
#' @export
#' @importFrom shiny moduleServer reactive observeEvent req showNotification
#' @importFrom DatabaseConnector dbExecute
createTableServer <- function(id, con, schema, data_changed, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    column_types <- c(
      "integer", "bigint", "numeric", "real", "double precision",
      "char", "varchar", "text", "date", "time", "timestamp",
      "boolean", "uuid", "json", "Choice"
    )

    output$column_inputs <- renderUI({
      num_columns <- input$num_columns
      lapply(1:num_columns, function(i) {
        fluidRow(
          column(4, textInput(ns(paste0("column_name_", i)), paste("Column", i, "Name"))),
          column(4, selectInput(ns(paste0("column_type_", i)), paste("Column", i, "Type"), choices = column_types)),
          column(4,
                 conditionalPanel(
                   condition = paste0("input['", ns(paste0("column_type_", i)), "'] == 'Choice'"),
                   textInput(ns(paste0("choice_values_", i)), "Enter possible values (comma-separated)")
                 )
          )
        )
      })
    })

    observeEvent(input$create_table, {
      req(con())
      table_name <- input$table_name
      num_columns <- input$num_columns

      columns <- sapply(1:num_columns, function(i) {
        col_name <- input[[paste0("column_name_", i)]]
        col_type <- input[[paste0("column_type_", i)]]

        if (col_type == "Choice") {
          enum_name <- paste0(table_name, "_", col_name, "_enum")
          enum_values <- input[[paste0("choice_values_", i)]]
          enum_query <- paste0("CREATE TYPE ", schema(), ".", enum_name, " AS ENUM (", paste0("'", gsub(",", "','", enum_values), "'"), ")")
          tryCatch({
            DatabaseConnector::dbExecute(con(), enum_query)
          }, error = function(e) {
            showNotification(paste("Error creating enum:", e$message), type = "error")
          })
          paste(col_name, enum_name)
        } else {
          paste(col_name, col_type)
        }
      })

      columns <- c("colid varchar(255) PRIMARY KEY", columns)
      columns_str <- paste(columns, collapse = ", ")

      query <- paste0("CREATE TABLE ", schema(), ".", table_name, " (", columns_str, ")")

      tryCatch({
        DatabaseConnector::dbExecute(con(), query)
        data_changed(data_changed() + 1)
        bs4Dash::updateTabItems(parent_session, "sidebar", selected = "tables")
        showNotification("Table created successfully", type = "message")
      }, error = function(e) {
        print(paste("Error creating table:", e$message))
        showNotification(paste("Error creating table:", e$message), type = "error")
      })
    })
  })
}
