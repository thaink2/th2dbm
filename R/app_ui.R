#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @export
app_ui <- function(request) {
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
            icon = shiny::icon("database"),
            tabName = "database_management",
            startExpanded = TRUE,
            bs4Dash::bs4SidebarMenuSubItem("Create Table", tabName = "create_db_table", icon = shiny::icon("plus")),
            bs4Dash::bs4SidebarMenuSubItem("Update Table", tabName = "update_db_table", icon = shiny::icon("pen")),
            bs4Dash::bs4SidebarMenuSubItem("Delete Table", tabName = "del_db_table", icon = shiny::icon("trash")),
            bs4Dash::bs4SidebarMenuSubItem("View Table", tabName = "view_db_table", icon = shiny::icon("eye"))
          ),
          bs4Dash::bs4SidebarMenuItem(
            "Data Handler",
            icon = shiny::icon("table"),
            tabName = "data_handler",
            startExpanded = TRUE,
            bs4Dash::bs4SidebarMenuSubItem("Testing", tabName = "manage_testing", icon = shiny::icon("flask"))
          )
        )
      ),
      body = bs4Dash::dashboardBody(
        bs4Dash::tabItems(
          bs4Dash::tabItem(
            "create_db_table",
            bs4Dash::tabBox(
              width = 12, selected = "Add entry",
              tabPanel(
                title = "Add entry", icon = shiny::icon("plus"),
                mod_th2_database_management_ui("database")
              )
            )
          ),
          bs4Dash::tabItem(
            "update_db_table",
            bs4Dash::tabBox(
              width = 12, selected = "Update Table",
              tabPanel(
                title = "Update Table", icon = shiny::icon("pen"),
                mod_th2_database_management_ui("update_database")
              ),
              tabPanel(
                title = "Update Columns", icon = shiny::icon("pen"),
                mod_update_metadata_ui("update_database_col")
              )
            )
          ),
          bs4Dash::tabItem(
            "del_db_table",
            bs4Dash::tabBox(
              width = 12, selected = "Overview",
              tabPanel(
                title = "Overview", icon = shiny::icon("trash"),
                mod_th2_database_management_ui("delete_database")
              )
            )
          ),
          bs4Dash::tabItem(
            "view_db_table",
            bs4Dash::tabBox(
              width = 12, selected = "Overview",
              tabPanel(
                title = "Overview", icon = shiny::icon("eye"),
                mod_th2db_overview_ui("view_db")
              )
            )
          ),
          bs4Dash::tabItem(
            "manage_testing",
            bs4Dash::tabBox(
              width = 12, selected = "General",
              tabPanel(
                title = "General", icon = shiny::icon("eye"),
                mod_th2use_db_table_ui(id = "test_main")
              ),
              tabPanel(
                title = "Gouvernance", icon = shiny::icon("user"),
                mod_th2use_db_table_ui(id = "users_table")
              ),
              tabPanel(
                title = "Permissions", icon = shiny::icon("user-lock"),
                mod_th2use_db_table_ui(id = "permission_table")
              ),
              tabPanel(
                title = "Secrets", icon = shiny::icon("lock"),
                mod_th2use_db_table_ui(id = "secret_table")
              ),
              tabPanel(
                title = "Data Connections", icon = shiny::icon("database"),
                mod_th2use_db_table_ui(id = "data_connection_params")
              ),
              tabPanel(
                title = "Sale data", icon = shiny::icon("database"),
                mod_th2use_db_table_ui(id = "sale_data")
              )
            )
          ),
          bs4Dash::tabItem(
            "manage_beta_tester",
            bs4Dash::tabBox(
              width = 12, selected = "Overview",
              tabPanel(
                title = "Overview", icon = shiny::icon("eye"),
                mod_beta_tester_manage_ui(id = "beta_tester")
              )
            )
          )
        )
      ),
      controlbar = bs4Dash::bs4DashControlbar(
        bs4Dash::controlbarMenu(
          id = "controlMenu",
          bs4Dash::controlbarItem("User Settings", icon = "users-cog", mod_change_current_user_ui("user"))
        )
      ),
      title = "Database Manager"
    )
  )
}
