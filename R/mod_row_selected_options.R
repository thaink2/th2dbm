#' @export
mod_row_selected_options_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      uiOutput(ns("options_buttons"))
    )
  )
}

#' @export
mod_row_selected_options_server <- function(id, target_table = "test_table", current_user = Sys.getenv("SHINYPROXY_USERNAME"), target_row = data.frame(), refresh_file = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$options_buttons <- renderUI({
      # if(is.null(selected_row()))return(NULL)
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
      db_meta_list <- list(target_table = target_table, target_row = target_row)
      module_id <- generateID(prefix = "update_selected_row")

      mod_th2new_entry_server(id = module_id, db_meta = db_meta_list, refresh_file = refresh_file)
      showModal(
        modalDialog(
          title = "Update entry", size = "l",
          mod_th2new_entry_ui(ns(module_id))
        )
      )
    })

    observeEvent(input$delete_selected_row, {
      if (!have_permission_to_manage(target_object = target_table)) {
        th2dbm::th_shinyalert(
          title = "Permission warning",
          confirmButtonCol = "#013DFF",
          text = glue::glue("{verifier_format_email(Sys.getenv('SHINYPROXY_USERNAME'))}  you are not authorized to add new entry into '{target_table}'"), type = "error"
        )

        return(NULL)
      }

      if (target_table == "th2metadata_table") {
        id_col <- "COLUMN_ID"
      } else {
        id_col <- "COL_ID"
      }
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
            label_text <- delete_entry_in_table(target_table = target_table, unique_id_col = id_col, unique_id_val = target_row[1, id_col])
            refresh_file %>% saveRDS(object = Sys.time(), file = .)
            th2dbm::th_shinyalert(title = "Delete Entry", text = label_text, type = "success")
            removeModal()
          } else {
            removeModal()
          }
        }
      )
    })
  })
}
