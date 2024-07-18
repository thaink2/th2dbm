library(glue)
library(dplyr)
library(shiny)
library(magrittr)


#' mod_th2use_db_table_ui
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @export
#'
#' @importFrom shiny NS tagList
mod_th2db_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      DT::dataTableOutput(ns("db_overview"))
    )
  )
}

#' mod_th2use_db_table_server
#'
#' @export
mod_th2db_overview_server <- function(id, target_table = "th2metadata_table", current_user = Sys.getenv("SHINYPROXY_USERNAME")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mod_refresh_file <- create_refresh_helper_file(mod_id = id)
    refresh_statement <- shiny::reactiveFileReader(intervalMillis = 3000, session = session, filePath = mod_refresh_file, readFunc = readRDS)

    db_overview <- reactive({
      req(refresh_statement())

      table_data <- retrieve_table_data(table_name = target_table)

      if (nrow(table_data) > 0) {
        table_data %>% dplyr::arrange(TH2DB_TABLE)
      }

      return(table_data)
    })



    output$db_overview <- DT::renderDataTable(
      {
        db_table_dt <- db_overview() %>%
          DT::datatable(options = list(
            scrollX = TRUE
          ))
      },
      server = TRUE
    )

    observeEvent(input$db_overview_rows_selected, {
      if (!have_permission_to_edit(target_object = target_table)) {
        th2dbm::th_shinyalert(
          title = "Permission warning",
          confirmButtonCol = "#013DFF",
          text = glue::glue("{verifier_format_email(Sys.getenv('SHINYPROXY_USERNAME'))}  you are not authorized to add new entry into '{target_table}'"), type = "error"
        )
        return(NULL)
      }

      rows_selected <- input$db_overview_rows_selected
      print(paste("target_row", db_overview()[rows_selected, ]))
      db_meta_list <- list(target_table = target_table, target_row = db_overview()[rows_selected, ])
      module_id <- generateID(prefix = "update_meta_entry")
      mod_row_selected_options_server(module_id, target_table = target_table, target_row = db_overview()[rows_selected, ], refresh_file = mod_refresh_file)
      showModal(
        modalDialog(
          title = "Entry Action", size = "m",
          icon = icon("pen"),
          mod_row_selected_options_ui(ns(module_id))
        )
      )
    })
  })
}
