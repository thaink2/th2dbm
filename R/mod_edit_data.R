#' User Interface for Data Editing Module
#'
#' Displays a modal dialog for editing data in a table.
#'
#' @param id The namespace ID for the module.
#'
#' @return A modal dialog containing UI elements for editing table data.
#'
#' @export
mod_edit_data_ui <- function(id) {
  ns <- NS(id)
  showModal(modalDialog(
    size = "l",
    title = "Table update",
    easyClose = TRUE,
    fluidPage(
      uiOutput(ns("columns_id")),
      uiOutput(ns("create_table_button"))
    )
  ))
}

#' Server Logic for Data Editing Module
#'
#' Handles the server-side logic for editing table data. It retrieves column
#' metadata, renders UI elements for each column using the 'mod_col_bloc'
#' module, and provides a save button to update the table schema in the database.
#'
#' @param id The namespace ID for the module.
#' @param target_table The name of the table to be edited.
#' @param mod_refresh_file The path to the file used for refreshing the module.
#'
#' @export
mod_edit_data_server <- function(id, target_table, mod_refresh_file, data_change = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    c_ids <- "COLUMN_IDs.csv"

    output$columns_id <- renderUI({
      db_con <- connect_to_database()
      vars_table <- vars_table_metadata(current_target_table = target_table)
      req(vars_table_metadata)
      colnames(vars_table) <- toupper(colnames(vars_table))
      temp <- 1:nrow(vars_table)
      columns_id <- lapply(temp, function(x) {
        new_col_id <- generateID(prefix = "mod_id")
        indice_col_id <- generateID(prefix = "varid")

        col_meta <- list(
          col_name = vars_table[x, "VAR_ID"],
          col_unique = vars_table[x, "VAR_UNIQUE"],
          col_type = vars_table[x, "VAR_TYPE"],
          col_id = vars_table[x, "COLUMN_ID"],
          col_choice = vars_table[x, "COLUMN_CHOICES"],
          col_choose_var = vars_table[x, "CHOOSE_FROM_VAR"],
          col_choose_tab = vars_table[x, "CHOOSE_FROM_TABLE"]
        )

        mod_col_bloc_server(new_col_id, tab_name = target_table, meta = col_meta, indice = indice_col_id)
        mod_col_bloc_ui(ns(new_col_id))
      })
      columns_id
    })


    output$create_table_button <- renderUI({
      actionButton(
        inputId = ns("save_button"),
        label = "Save", style = add_button_theme(),
        icon = icon("save")
      )
    })

    observeEvent(input$save_button, {
      db_con <- connect_to_database()
      if (!file.exists(c_ids)) {
        th2dbm::th_shinyalert(
          title = glue::glue("Le fichier CSV {c_ids} n'existe pas"),
          text = "",
          confirmButtonCol = "#013DFF",
          type = "error"
        )
        return(NULL)
      }
      test_fields_type <- readr::read_csv(c_ids)

      bd_response <- create_update_metadata(table_metadata = test_fields_type, action_table = "update")

      th2dbm::th_shinyalert(
        title = glue::glue("{input$table_name} created successfully"),
        text = "",
        confirmButtonCol = "#013DFF",
        type = "success"
      )
      data_change(data_change() + 1)
      file.remove(c_ids)
      # Fermer le modal
      saveRDS(object = Sys.time(), file = mod_refresh_file)
      removeModal()
    })
  })
}
