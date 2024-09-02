library(glue)
library(dplyr)
library(shiny)
library(magrittr)
library(uuid)

#' mod_th2use_db_table_ui
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @export
#'
#' @importFrom shiny NS tagList
mod_th2use_db_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(width = 3, uiOutput(ns("testing_add_button"))),
        column(width = 3, uiOutput(ns("permission_choice"))),
        column(width = 3, uiOutput(ns("grant_add_button"))),
        column(width = 3, downloadButton(ns("downloadData"), "Download", icon = shiny::icon("download")))
      ),
      DT::dataTableOutput(ns("use_db_table_dt"))
    )
  )
}

#' mod_th2use_db_table_server
#'
#' @export
mod_th2use_db_table_server <- function(id, target_table = "test_table", current_user = Sys.getenv("SHINYPROXY_USERNAME")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mod_refresh_file <- create_refresh_helper_file(mod_id = id)
    refresh_statement <- shiny::reactiveFileReader(intervalMillis = 3000, session = session, filePath = mod_refresh_file, readFunc = readRDS)

    output$permission_choice <- renderUI({
      if (target_table %in% current_permission_table()) {
        selectInput(inputId = ns("change_target_table"), label = glue::glue("Permission table"), choices = current_permission_table(), selected = target_table)
      }
    })

    observeEvent(input$change_target_table, {
      target_table <- input$change_target_table
    })

    vars_table_metadata <- vars_table_metadata(current_target_table = target_table)

    use_db_table <- reactive({
      if (target_table %in% current_permission_table()) {
        req(refresh_statement(), input$change_target_table)
        target_table <- input$change_target_table
      } else {
        req(refresh_statement())
      }

      db_con <- connect_to_database()
      available_tables <- DBI::dbListTables(conn = db_con)
      if (!target_table %in% available_tables) {
        return(NULL)
      }
      test_table <- DBI::dbReadTable(conn = db_con, name = {
        target_table
      })


      if (target_table %in% current_permission_table()) {
        test_table <- test_table %>% arrange(OBJECT_ID)
      }

      DBI::dbDisconnect(db_con)

      return(test_table)
    })


    output$grant_add_button <- renderUI({
      req(input$change_target_table)
      if (target_table %in% current_permission_table()) {
        actionButton(inputId = ns("grant_test_entry"), label = "Grant entry", style = add_button_theme(), icon = icon("user-lock"))
      }
    })

    output$testing_add_button <- renderUI({
      actionButton(inputId = ns("add_test_entry"), label = "Add entry", style = add_button_theme(), icon = icon("plus"))
    })


    observeEvent(input$add_test_entry, {
      if (!have_permission_to_manage(target_object = target_table)) {
        th2dbm::th_shinyalert(
          title = "Permission warning",
          confirmButtonCol = "#013DFF",
          text = glue::glue("{verifier_format_email(Sys.getenv('SHINYPROXY_USERNAME'))}  you are not authorized to add new entry into '{target_table}'"), type = "error"
        )

        return(NULL)
      }
      db_meta_list <- list(target_table = target_table)
      module_id <- generateID(prefix = target_table)
      mod_th2new_entry_server(id = module_id, db_meta = db_meta_list, refresh_file = mod_refresh_file)
      showModal(
        modalDialog(
          title = "Add entry", size = "l",
          mod_th2new_entry_ui(id = ns(module_id))
        )
      )
    })

    observeEvent(input$grant_test_entry, {
      target_table <- input$change_target_table
      module_id <- generateID(prefix = "permission_")
      mod_ml_permissions_server(id = module_id, target_perm_table = target_table, action = "add", refresh_file = mod_refresh_file)
      showModal(
        modalDialog(
          title = "Permissions", size = "l",
          mod_ml_permissions_ui(ns(module_id))
        )
      )
    })
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(glue::glue("{target_table}_"), Sys.time(), ".csv", sep = "")
      },
      content = function(file) {
        data <- use_db_table() %>%
          dplyr::select(-COL_ID, -ID)
        readr::write_csv(x = data, file = file)
      }
    )

    output$use_db_table_dt <- DT::renderDataTable(
      {
        req(use_db_table())
        # if (!have_permission_to_see(target_object = target_table)) {
        #   th2dbm::th_shinyalert(
        #     title = "Permission warning",
        #     confirmButtonCol = "#013DFF",
        #     text = glue::glue("{verifier_format_email(Sys.getenv('SHINYPROXY_USERNAME'))}  you are not authorized to add new entry into '{target_table}'"), type = "error"
        #   )
        #   return(NULL)
        # }
        use_db_table() %>%
          prepare_db_table_view(target_table = target_table, current_permission_table = current_permission_table())
      },
      server = TRUE
    )


    observeEvent(input$use_db_table_dt_rows_selected, {
      if (target_table %in% current_permission_table()) {
        target_table <- input$change_target_table
      }
      if (!have_permission_to_edit(target_object = target_table)) {
        th2dbm::th_shinyalert(
          title = "Permission warning",
          confirmButtonCol = "#013DFF",
          text = glue::glue("{verifier_format_email(Sys.getenv('SHINYPROXY_USERNAME'))}  you are not authorized to add new entry into '{target_table}'"), type = "error"
        )
        return(NULL)
      }

      rows_selected <- input$use_db_table_dt_rows_selected
      db_meta_list <- list(target_table = target_table, target_row = use_db_table()[rows_selected, ])
      module_id <- generateID(prefix = "update_test_entry")
      mod_row_selected_options_server(module_id, target_table = target_table, target_row = use_db_table()[rows_selected, ], refresh_file = mod_refresh_file)
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
