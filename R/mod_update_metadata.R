#' @export
mod_update_metadata_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("update_col"))
}

#' @export
mod_update_metadata_server <-
  function(id) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      c_ids <- "COLUMN_IDs.csv"

      output$update_col <- renderUI({
        actionButton(
          inputId = ns(glue::glue("update_column_btn")),
          label = glue::glue("Update column"),
          icon = icon("pen")
        )
      })

      observeEvent(input$update_column_btn, {
        showModal(modalDialog(
          size = "l",
          title = "Table update",
          fluidPage(
            fluidRow(
              column(width = 2, uiOutput(ns("update_table_name")))
            ),
            uiOutput(ns("columns_id")),
            uiOutput(ns("create_table_button"))
          )
        ))
      })

      output$update_table_name <- renderUI({
        db_con <- connect_to_database()
        available_fields <- DBI::dbGetQuery(conn = db_con, "SELECT TH2DB_TABLE FROM th2metadata_table")
        selectInput(inputId = ns("table_name"), label = "Table Name", choices = available_fields, selected = "test_table")
      })


      output$columns_id <- renderUI({
        vars_table_metadata <- vars_table_metadata(current_target_table = input$table_name)
        req(vars_table_metadata)
        vars_table <<- vars_table_metadata
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

          mod_col_bloc_server(new_col_id, tab_name = input$table_name, meta = col_meta, indice = indice_col_id)
          mod_col_bloc_ui(ns(new_col_id))
        })
        columns_id
      })


      output$create_table_button <- renderUI({
        req(input$table_name)
        actionButton(
          inputId = ns("save_button"),
          label = "Save",
          icon = icon("save")
        )
      })

      observeEvent(input$save_button, {
        db_con <- connect_to_database()
        if (!file.exists(c_ids)) {
          th2product::th_shinyalert(
            title = glue::glue("Le fichier CSV {c_ids} n'existe pas"),
            text = "",
            confirmButtonCol = "#013DFF",
            type = "danger"
          )
          return(NULL)
        }
        test_fields_type <<- read.csv(c_ids, stringsAsFactors = FALSE)

        bd_response <- create_update_metadata(table_metadata = test_fields_type, action_table = "update")

        th2product::th_shinyalert(
          title = glue::glue("La table {input$table_name} mise à jour avec succès!"),
          text = "",
          confirmButtonCol = "#013DFF",
          type = "success"
        )
        file.remove(c_ids)
        # Fermer le modal
        removeModal()
      })
    })
  }
