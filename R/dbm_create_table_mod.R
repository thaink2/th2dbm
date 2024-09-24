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
      title = "Create New Table", width = 12, status = "primary", solidHeader = TRUE, background = "white",
      textInput(ns("table_name"), "Table Name"),
      numericInput(ns("num_columns"), "Number of Columns", value = 1, min = 1),
      uiOutput(ns("column_inputs")),
      uiOutput(ns("create_table"))
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
      "varchar", "numeric", "text", "date", "time", "timestamp",
      "boolean", "uuid", "Choice"
    )
    c_ids <- "COLUMN_IDs.csv"
    if (file.exists(c_ids)) {
      file.remove(c_ids)
    }


    output$column_inputs  <- renderUI({
      # req(input$table_num_items)
      req(input$num_columns)
      temp <- 1:input$num_columns
      lapply(temp, function(i) {
        new_col_id <- generateID(prefix = glue::glue("{input$table_name}_"))
        mod_col_bloc_server(id = paste0("column_", i), tab_name = input$table_name, indice = new_col_id)
        mod_col_bloc_ui(id = ns(paste0("column_", i)))
      })
    })


    output$create_table <- renderUI({
      req(input$table_name)
      actionButton(ns("create_table"), "Create Table", style = add_button_theme(), icon = icon("table"))
    })

    observeEvent(input$create_table, {
      req(con())
      if (!file.exists(c_ids)) {
        print(glue::glue("Le fichier CSV {c_ids} n'existe pas"))
        return(NULL)
      }
      action <- "create"
      table_name <- input$table_name
      num_columns <- input$num_columns
      tryCatch({
        search_query <- sprintf("SELECT * FROM th2metadata_table WHERE TH2DB_TABLE = '%s'", table_name)
        search_query <- DBI::dbGetQuery(con(), search_query)

        if (nrow(search_query) != 0 & action == "create") {
          th2dbm::th_shinyalert(
            title = glue::glue("{input$table_name} table already exists !"),
            confirmButtonCol = "#013DFF",
            text = "",
            type = "error"
          )
          showNotification(glue::glue("{input$table_name} table already exists !"), type = "error")
          return("table already exists !")
        }
      }, error = function(e) {
        print(paste("Error seaching table:", e$message))
        showNotification(paste("Error seaching table:", e$message), type = "error")
      })

      enum_queries <- character(0)
      columns <- character(0)

      for (i in 1:num_columns) {
        col_name <- input[[paste0("column_name_", i)]]
        col_type <- input[[paste0("column_type_", i)]]

        if (col_type == "Choice") {
          choices_values <- input[[paste0("choice_values_", i)]]
          enum_name <- paste0(table_name, "_", col_name, "_enum")
          enum_query <- paste0("CREATE TYPE ", schema(), ".", enum_name, " AS ENUM (",
                               paste0("'", choices_values, "'", collapse = ", "), ")")
          enum_queries <- c(enum_queries, enum_query)
          columns <- c(columns, paste(col_name, enum_name))
        } else {
          columns <- c(columns, paste(col_name, col_type))
        }
      }

      columns <- c("colid varchar(255) PRIMARY KEY", columns)
      columns_str <- paste(columns, collapse = ", ")

      tryCatch({
        # Create ENUM types
        for (query in enum_queries) {
          DatabaseConnector::dbExecute(con(), query)
        }

        # Create table
        query <- paste0("CREATE TABLE ", schema(), ".", table_name, " (", columns_str, ")")
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
