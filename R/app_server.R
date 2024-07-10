#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
  # initalize database
  initialize_database(working_mode = "Prod", cluster = "aws")

  observeEvent(input$user_button, {
    mod_id <- generateID()
    mod_change_current_user_server(mod_id)
    showModal(
      modalDialog(
        mod_change_current_user_ui(mod_id)
      )
    )
  })
  # Appeler les modules serveur correspondant à chaque tabItem
  mod_th2_database_management_server("database")
  mod_th2_database_management_server("update_database", action = "update_tab")
  mod_update_metadata_server("update_database_col")
  mod_th2_database_management_server("delete_database", action = "delete_tab")
  mod_th2db_overview_server("view_db")
  mod_th2use_db_table_server(id = "test_main", target_table = "test_table")
  mod_th2use_db_table_server(id = "users_table", target_table = "users_table")
  mod_th2use_db_table_server(id = "permission_table", target_table = "th2_ml_permissions")
  mod_th2use_db_table_server(id = "secret_table", target_table = "secret_table")
  mod_th2use_db_table_server(id = "data_connection_params", target_table = "data_connection_params")
  mod_th2use_db_table_server(id = "sale_data", target_table = "sale_data")
  mod_beta_tester_manage_server(id = "beta_tester", target_table = "beta_tester_table")
  observeEvent(input$controlbar, {
    mod_change_current_user_ui("user")
    # if (input$controlbar) {
    #   showModal(modalDialog(
    #     title = "Alert",
    #     "The controlbar is opened.",
    #     easyClose = TRUE,
    #     footer = NULL
    #   ))
    # }
  })
}
