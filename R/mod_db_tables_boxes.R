#' User Interface for Database Tables Boxes Module
#'
#' Creates a user interface to display database table information in boxes.
#' It includes a refresh button to update the table list dynamically.
#'
#' @param id The namespace ID for the module.
#'
#' @return A `fluidRow` containing a refresh button and a UI output for displaying
#' the table boxes.
#'
#' @export
mod_db_tables_boxes_ui <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(width = 12, offset = 10, uiOutput(ns("refresh"))),
    column(width = 12, uiOutput(ns("db_tables_box")))
  )
}

#' Server Logic for Database Tables Boxes Module
#'
#' Handles the server-side logic for displaying database table information. It
#' retrieves the list of available tables from the database, creates a box for each
#' table using the 'mod_table_box' module, and allows for refreshing the table list.
#'
#' @param id The namespace ID for the module.
#' @param mod_refresh_file The path to the file used for refreshing the module.
#'
#' @export
mod_db_tables_boxes_server <- function(id, mod_refresh_file) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    tryCatch(
      {
        db_con <- connect_to_database()

        refresh_statement <- shiny::reactiveFileReader(
          intervalMillis = 1000,
          session = session,
          filePath = mod_refresh_file,
          readFunc = readRDS
        )

        output$refresh <- renderUI({
          actionButton(inputId = ns("refresh"), label = "", style = add_button_theme(), icon = icon("arrows-rotate"), class = "btn-primary")
        })

        observeEvent(input$refresh, {
          saveRDS(object = Sys.time(), file = mod_refresh_file)
        })

        get_tables_list <- function() {
          tryCatch(
            {
              available_tables <- fetch_data_from_db(table = "th2metadata_table", cols = c("th2db_table"))
              available_unique_tables <- available_tables %>% dplyr::distinct(th2db_table)
              return(available_unique_tables)
            },
            error = function(e) {
              showModal(modalDialog(
                title = "Error retrieving tables",
                p("An error occurred while retrieving the list of tables:"),
                pre(as.character(e)),
                easyClose = TRUE
              ))
              return(NULL)
            }
          )
        }

        tables_list <- reactive({
          req(refresh_statement())
          get_tables_list()
        })

        observe({
          req(tables_list())
          available_tables <- tables_list() %>% dplyr::pull(th2db_table)

          if (length(available_tables) == 0) {
            toast(
              title = "No tables available",
              options = list(
                autohide = TRUE,
                class = "bg-red",
                position = "topRight"
              )
            )
          }

          table_list <- tryCatch(
            {
              lapply(available_tables, function(x) {
                module_id <- th2dbm::generateID(prefix = "data_card")
                mod_table_box_server(id = module_id, target_table = x, mod_refresh_file = mod_refresh_file)
                column(width = 4, mod_table_box_ui(id = ns(module_id)))
              })
            },
            error = function(e) {
              showModal(modalDialog(
                title = "Error creating boxes",
                p("An error occurred while creating table boxes:"),
                pre(as.character(e)),
                easyClose = TRUE
              ))
              return(NULL)
            }
          )
          output$db_tables_box <- renderUI({
            do.call(shiny::fluidRow, table_list)
          })
        })
      },
      error = function(e) {
        showModal(modalDialog(
          title = "Unexpected error",
          p("An unexpected error occurred:"),
          pre(as.character(e)),
          easyClose = TRUE
        ))
      }
    )
  })
}
