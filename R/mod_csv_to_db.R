#' User Interface for CSV to Database Module
#'
#' Provides the UI elements for importing a CSV file into a database table.
#'
#' @param id The namespace ID for the module.
#'
#' @return A `fluidPage` containing input elements for table name and CSV file selection.
#'
#' @export
mod_csv_to_db_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    uiOutput(ns("columns_id"))
  )
}

#' Server Logic for CSV to Database Module
#'
#' Handles the server-side logic for importing a CSV file into a database table.
#' It allows the user to specify the table name and select the CSV file. Upon import,
#' it checks for table existence, writes the data to the database, and optionally
#' adds permissions to the 'th2_ml_permissions' table.
#'
#' @param id The namespace ID for the module.
#'
#' @export
mod_csv_to_db_server <- function(id, con, data_changed, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    button_theme <- add_button_theme()
    output$columns_id <- renderUI({
      fluidPage(
        tags$head(tags$style(".progress-bar{background-color:#D95F02;}")),
        fluidRow(
          column(width = 2, uiOutput(ns("file_delim"))),
          column(width = 3, uiOutput(ns("tisefka_file"))),
          column(width = 2, uiOutput(ns("excel_tawriqt"))),
          column(width = 3, uiOutput(ns("data_name"))),
          column(width = 2, br(), uiOutput(ns("save_data")))
        ),
        fluidRow(
          column(width = 12, DT::DTOutput(ns("data_summary"))),
        )
      )
    })

    output$tisefka_file <- renderUI({
      fileInput(
        inputId = ns("tisefka_file"), label = "Choose File",
        multiple = FALSE,
        accept = c(".csv", ".txt", ".xlsx")
      )
    })

    output$file_delim <- renderUI({
      shinyWidgets::pickerInput(inputId = ns("file_delim"), label = "Delimiter", choices = c(",", ";", "\t", "|"))
    })

    file_tasetta <- reactive({
      req(input$tisefka_file)
      tail(unlist(strsplit(input$tisefka_file$datapath, split = "\\.")), 1)
    })

    excel_tiwriqin <- reactive({
      req(file_tasetta())
      if (file_tasetta() == "csv") {
        return(NULL)
      }
      if (file_tasetta() == "xlsx") {
        readxl::excel_sheets(data_path())
      }
    })

    output$excel_tawriqt <- renderUI({
      req(excel_tiwriqin())
      if (is.null(excel_tiwriqin())) {
        return(NULL)
      }
      shinyWidgets::pickerInput(
        inputId = ns("excel_tawriqt"),
        label = "Choose excel sheet:",
        choices = excel_tiwriqin(),
        options = list(
          style = "btn-primary"
        )
      )
    })

    csv_tisefka <- reactive({
      req(file_tasetta())
      ghred_tisefka_aqerru(input_file = input$tisefka_file$datapath, tala = file_tasetta(), tawriqt = input$excel_tawriqt, delim = input$file_delim)
    })

    tisefka_tizegzawin <- reactive({
      csv_tisefka() %>%
        dplyr::distinct(., .keep_all = TRUE) %>%
        janitor::remove_constant() %>%
        janitor::remove_empty() %>%
        janitor::clean_names()
    })

    output$data_summary <- DT::renderDT({
      req(tisefka_tizegzawin())
      DT::datatable(head(tisefka_tizegzawin(), 5), rownames = FALSE, options = list(scrollX = TRUE))
    })

    output$save_data <- renderUI({
      req(tisefka_tizegzawin(), input$data_name)
      actionButton(
        inputId = ns("save_data"),
        icon = icon("floppy-disk"),
        style = button_theme,
        label = "Save", class = "btn-primary"
      )
    })

    output$data_name <- renderUI({
      textInputCustom(inputId = ns("data_name"), value = NULL, label = "Model Name")
    })

    observeEvent(input$save_data, {
      file_path <- input$tisefka_file$datapath
      file_name <- input$tisefka_file$name

      search_query <- sprintf("SELECT * FROM th2metadata_table WHERE TH2DB_TABLE = '%s'", input$data_name)
      search_query <- DBI::dbGetQuery(con(), search_query)

      if (nrow(search_query) != 0) {
        th2dbm::th_shinyalert(
          title = glue::glue("La table {input$table_name} existe déjà!"),
          confirmButtonCol = "#013DFF",
          text = "", type = "error"
        )

        removeModal()
        return("Cette table existe déjà!")
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

      # Liste des mots-clés SQL courants (à compléter si nécessaire)
      sql_keywords <- c("default", "month", "year", "order", "group", "table", "select", "from", "where", "and", "or", "insert", "update", "delete", "drop", "create", "alter", "index", "view", "constraint")

      # Fonction pour échapper les noms de colonnes
      escape_column_names <- function(df) {
        new_names <- colnames(df)
        for (i in seq_along(new_names)) {
          if (tolower(new_names[i]) %in% sql_keywords) {
            new_names[i] <- paste0(new_names[i], "_")
          }
        }
        colnames(df) <- new_names
        return(df)
      }

      table_df <- tisefka_tizegzawin()

      table_df <- escape_column_names(table_df)

      DBI::dbWriteTable(con(), input$data_name, table_df, row.names = FALSE, overwrite = TRUE)

      data_changed(data_changed() + 1)
      bs4Dash::updateTabItems(parent_session, "sidebar", selected = "tables")
      removeModal()
      th2dbm::th_shinyalert(
        title = glue::glue("La table {input$data_name} créé avec succès!"),
        text = "",
        confirmButtonCol = "#013DFF",
        type = "success"
      )
    })
  })
}
