#' User Interface for Row Selected Options Module
#'
#' Creates a UI for displaying options when a row is selected,
#' typically including update and delete buttons.
#'
#' @param id The namespace ID for the module.
#'
#' @return A `tagList` containing the UI elements for the row selected options.
#'
#' @export
dbm_selected_row_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      uiOutput(ns("options_buttons"))
    )
  )
}

#' Server Logic for Row Selected Options Module
#'
#' Handles the server-side logic for row selected options, including
#' updating and deleting rows from the specified table.
#'
#' @param id The namespace ID for the module.
#' @param target_table The name of the table associated with the selected row.
#' @param current_user The current user's username.
#' @param target_row The data frame representing the selected row.
#' @param refresh_file The path to the file used for refreshing the module.
#'
#' @export
dbm_selected_row_server <- function(id, table_name = "test_table",con, current_user = Sys.getenv("SHINYPROXY_USERNAME"), row_data = data.frame(), selected_row, data_changed = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    table_structure <- get_table_structure(con(), table_name)
    primary_key <- get_primary_key(con(), table_name)

    output$options_buttons <- renderUI({
      fluidRow(
        column(
          width = 6,
          actionButton(inputId = ns("update_selected_row"), label = "Update", style = add_button_theme(), icon = icon("pen"))
        ),
        column(
          width = 6,
          actionButton(inputId = ns("delete_selected_row"), label = "Delete", style = add_button_theme(), icon = icon("trash"))
        )
      )
    })

    observeEvent(input$update_selected_row, {
      module_id <- generateID(prefix = "update_selected_row")

      if (is.null(primary_key)) {
        print(paste("Unable to modify row: No primary key found"))
        showNotification("Unable to modify row: No primary key found", type = "error")
        return()
      }

      modifyRowServer(module_id, table_name = table_name, table_structure, row_data = row_data, con = con(), primary_key = primary_key, data_changed = data_changed)
      showModal(modalDialog(
        title = paste("Modify Row in", table_name), icon = icon("pen"), easyClose = TRUE, size = "l",
        modifyRowUI(ns(module_id), table_structure, row_data),
        footer = modalButton("Close")
      ))

    })

    observeEvent(input$delete_selected_row, {
      # Afficher une boîte de dialogue de confirmation avant de supprimer
      shinyalert::shinyalert(
        text = "Confirm deletion of this entry ?",
        title = "Confirmation de suppression",
        type = "warning",
        closeOnClickOutside = TRUE,
        showCancelButton = TRUE,
        confirmButtonCol = "#013DFF",
        imageUrl = "https://raw.githubusercontent.com/thaink2/thaink2publicimages/main/thaink2_logo_circle.png",
        showConfirmButton = TRUE,
        confirmButtonText = "Oui, supprimer!",
        cancelButtonText = "Non, annuler!",
        callbackR = function(value) {
          if (isTRUE(value)) {
            # Construire la requête de suppression
            id_to_delete <- row_data[, primary_key]
            print(id_to_delete)
            delete_query <- paste0("DELETE FROM ", table_name, " WHERE ", primary_key, " = '", id_to_delete, "'")

            # Exécuter la requête de suppression
            tryCatch({
              DBI::dbExecute(con(), delete_query)
              showNotification("Rows deleted successfully", type = "message")
              data_changed(data_changed() + 1)
            }, error = function(e) {
              showNotification(paste("Error deleting rows:", e$message), type = "error")
            })
            removeModal()
          } else {
            removeModal()
          }
        }
      )
    })
  })
}
