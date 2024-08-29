#' @export
mod_csv_to_db_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    uiOutput(ns("columns_id")),
    uiOutput(ns("add_button"))
  )
}

#' @export
mod_csv_to_db_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$columns_id <- renderUI({
      fluidRow(
        column(width = 4, textInput(inputId = ns("table_name"), label = glue::glue("Table name"), value = "")),
        column(width = 4, fileInput(inputId = ns("csv_file"), label = "CSV File")),
      )
    })

    output$add_button <- renderUI({
      req(input$table_name, input$csv_file)

      actionButton(inputId = ns("add_button"), label = "Add", style = add_button_theme(), icon = icon("plus"))
    })

    observeEvent(input$add_button, {
      file <- input$csv_file
      file_path <- file$datapath
      file_name <- file$name

      db_con <- connect_to_database()
      search_query <- sprintf("SELECT * FROM th2metadata_table WHERE TH2DB_TABLE = '%s'", input$table_name)
      search_query <- DBI::dbGetQuery(db_con, search_query)
      DBI::dbDisconnect(db_con)

      if (nrow(search_query) != 0) {
        th2dbm::th_shinyalert(
          title = glue::glue("{input$table_name} table already exists !"),
          confirmButtonCol = "#013DFF",
          text = "", type = "error"
        )

        removeModal()
        return("Table already exists")
      }
      # Remplacer '0' suivi de n'importe quelle extension par 'file_name' suivi de la même extension
      partie_avant_0 <- sub("/0.*", "/", file_path)
      new_file_path <- paste0(partie_avant_0, file_name)

      if (!file.exists(file_path)) {
        th2dbm::th_shinyalert(
          title = glue::glue("Le fichier CSV {file_path} n'existe pas"),
          text = "",
          confirmButtonCol = "#013DFF",
          type = "error"
        )
        return(NULL)
      }



      table_df <- readr::read_csv(file_path)

      colnames(table_df) <- toupper(colnames(table_df))

      if (!"COL_ID" %in% colnames(table_df) || is.na(table_df$COL_ID[1])) {
        table_df <- table_df %>%
          dplyr::mutate(COL_ID = generateID(prefix = input$table_name))
      }

      db_con <- connect_to_database()
      DBI::dbWriteTable(db_con, input$table_name, table_df, row.names = FALSE, overwrite = TRUE)
      DBI::dbDisconnect(db_con)

      if (input$table_name != "th2_ml_permissions") {
        permissions_value <- data.frame(
          OBJECT_ID = input$table_name, OBJECT_CREATOR = Sys.getenv("SHINYPROXY_USERNAME"),
          PERMITTED_USERS = Sys.getenv("SHINYPROXY_USERNAME"), OBJECT_TYPE = "db_table",
          PERMISSION_LEVEL = "Owner", PERMISSION_TIME = paste(Sys.time())
        )

        add_entry_to_table(new_entry = permissions_value, target_table = "th2_ml_permissions")
      }
      removeModal()
      th2dbm::th_shinyalert(
        title = glue::glue("{input$table_name} created successfully"),
        text = "",
        confirmButtonCol = "#013DFF",
        type = "success"
      )
    })
  })
}
