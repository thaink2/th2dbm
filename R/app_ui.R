#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @export
app_ui <- function(request) {
  shinyFeedback::useShinyFeedback(feedback = FALSE)
  tagList(
    shiny.info::powered_by("thaink²", link = "https://www.thaink2.com/", position = "bottom right"),
    shinybusy::add_busy_bar(color = "#013DFF"),
    includeCSS(system.file("custom_icon.css", package = "SaldaeReporting")),
    bs4Dash::dashboardPage(
      header = prepare_app_header(),
      preloader = list(html = tagList(waiter::spin_clock(), "Loading ..."), color = "#013DFF"),
      title = "Database Manager & Viewer",
      sidebar = bs4Dash::bs4DashSidebar(
        bs4Dash::bs4SidebarMenu(
          id = "sidebar",
          bs4Dash::bs4SidebarMenuItem("Configuration", tabName = "config", icon = icon("cogs")),

          uiOutput("dynamic_menu_items")

        )
      ),
      body = bs4Dash::dashboardBody(
        bs4Dash::tabItems(
          bs4Dash::tabItem(tabName = "config", dbConfigUI("db_config")),
          bs4Dash::tabItem(
            "tables",
              mod_db_tables_boxes_ui("db_tables_boxes")
            ),

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
          )
        )
      )
    )
  )
}
