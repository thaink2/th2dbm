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
  db_config <- dbConfigServer("db_config", parent_session = session)
  output$dynamic_menu_items <- renderUI({
    if (!is.null(db_config$con())) {
      tagList(
        bs4Dash::bs4SidebarMenuItem("View Tables", tabName = "tables", icon = icon("eye")),
        bs4Dash::bs4SidebarMenuItem("Create Table", tabName = "create_db_table", icon = icon("plus")),
        # bs4Dash::bs4SidebarMenuSubItem("Update Table", tabName = "update_db_table", icon = icon("pen")),
        # bs4Dash::bs4SidebarMenuSubItem("Delete Table", tabName = "del_db_table", icon = icon("trash"))
      )
    }
  })
  data_change <- reactiveVal(0)

  # Appeler les modules serveur correspondant à chaque tabItem
  mod_th2_database_management_server("database", mod_refresh_file = mod_refresh_file, data_change = data_change, parent_session = session)
  mod_th2_database_management_server("update_database", action = "update_tab", mod_refresh_file = mod_refresh_file)
  mod_update_metadata_server("update_database_col", mod_refresh_file = mod_refresh_file)
  mod_th2_database_management_server("delete_database", action = "delete_tab", mod_refresh_file = mod_refresh_file)
  # mod_th2db_overview_server("view_db", data_change = data_change)
  mod_db_tables_boxes_server("db_tables_boxes", mod_refresh_file = mod_refresh_file, data_change = data_change)
}
