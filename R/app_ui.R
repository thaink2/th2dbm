#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @export
app_ui <- function(request) {
  shinyFeedback::useShinyFeedback(feedback = FALSE)
  tagList(
    # Logique de l'interface utilisateur de l'application
    bs4Dash::bs4DashPage(
      options = list(sidebarExpandOnHover = TRUE),
      header = prepare_app_header(),
      sidebar = bs4Dash::bs4DashSidebar(
        id = "sidebar",
        bs4Dash::bs4SidebarMenu(
          bs4Dash::bs4SidebarMenuItem(
            "Database Manager",
            icon = icon("database"),
            tabName = "database_management",
            startExpanded = TRUE,
            bs4Dash::bs4SidebarMenuSubItem("Create Table", tabName = "create_db_table", icon = icon("plus")),
            bs4Dash::bs4SidebarMenuSubItem("Update Table", tabName = "update_db_table", icon = icon("pen")),
            bs4Dash::bs4SidebarMenuSubItem("Delete Table", tabName = "del_db_table", icon = icon("trash")),
            bs4Dash::bs4SidebarMenuSubItem("View Table", tabName = "view_db_table", icon = icon("eye"))
          )
        )
      ),
      body = bs4Dash::dashboardBody(
        bs4Dash::tabItems(
          bs4Dash::tabItem(
            "create_db_table",
            bs4Dash::tabBox(
              width = 12, selected = "Add entry",
              title = "Add entry", icon = icon("plus"),
              mod_th2_database_management_ui("database")
            )
          ),
          bs4Dash::tabItem(
            "update_db_table",
            bs4Dash::tabBox(
              width = 12, selected = "Update Table",
              tabPanel(
                title = "Update Table", icon = icon("pen"),
                mod_th2_database_management_ui("update_database")
              ),
              tabPanel(
                title = "Update Columns", icon = icon("pen"),
                mod_update_metadata_ui("update_database_col")
              )
            )
          ),
          bs4Dash::tabItem(
            "del_db_table",
            bs4Dash::tabBox(
              width = 12, selected = "Overview",
              title = "Overview", icon = icon("trash"),
              mod_th2_database_management_ui("delete_database")
            )
          ),
          bs4Dash::tabItem(
            "view_db_table",
            bs4Dash::tabBox(
              width = 12, selected = "Overview",
              title = "Overview", icon = icon("eye"),
              mod_db_tables_boxes_ui("db_tables_boxes")
            )
          )
        )
      ),
      title = "Database Manager"
    )
  )
}
