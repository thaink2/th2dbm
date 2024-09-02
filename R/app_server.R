#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @export

app_server <- function(input, output, session) {
  # initalize database
  initialize_database(working_mode = "Prod", cluster = "aws")
  options(shiny.maxRequestSize = 100000 * 1024^2)

  mod_refresh_file <- create_refresh_helper_file(mod_id = "db_tables_boxes")

  # Appeler les modules serveur correspondant à chaque tabItem
  mod_th2_database_management_server("database", mod_refresh_file = mod_refresh_file)
  mod_th2_database_management_server("update_database", action = "update_tab", mod_refresh_file = mod_refresh_file)
  mod_update_metadata_server("update_database_col", mod_refresh_file = mod_refresh_file)
  mod_th2_database_management_server("delete_database", action = "delete_tab", mod_refresh_file = mod_refresh_file)
  # mod_th2db_overview_server("view_db")
  mod_db_tables_boxes_server("db_tables_boxes", mod_refresh_file = mod_refresh_file)
}
