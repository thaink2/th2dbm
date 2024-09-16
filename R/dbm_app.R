#' Run the PostgreSQL Manager Shiny App
#'
#' This function launches the main Shiny application for the PostgreSQL Manager.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom bs4Dash dashboardPage dashboardHeader dashboardSidebar sidebarMenu menuItem dashboardBody tabItems tabItem

dbm_ui <- function(request){
    tagList(
        shiny.info::powered_by("thaink²", link = "https://www.thaink2.com/", position = "bottom right"),
        shinybusy::add_busy_bar(color = "#013DFF"),
        includeCSS(system.file("custom_icon.css", package = "SaldaeReporting")),
    bs4Dash::dashboardPage(
        header = prepare_app_header(),
        preloader = list(html = tagList(waiter::spin_clock(), "Loading ..."), color = "#013DFF"),
        title = "Database Manager & Viewer",
    bs4Dash::dashboardSidebar(
        bs4Dash::sidebarMenu(
            id = "sidebar",
            bs4Dash::menuItem("Configuration", tabName = "config", icon = icon("cogs")),
            bs4Dash::menuItem("Tables", tabName = "tables", icon = icon("table")),
            bs4Dash::menuItem("Create Table", tabName = "create", icon = icon("plus"))
        )
    ),
    bs4Dash::dashboardBody(
        bs4Dash::tabItems(
            bs4Dash::tabItem(tabName = "config", dbConfigUI("db_config")),
            bs4Dash::tabItem(tabName = "tables", tableListUI("table_list")),
            bs4Dash::tabItem(tabName = "create", createTableUI("create_table"))
        )
    )
    )
)
}

#' dbm_app_launcher
#' @export
dbm_server <- function(input, output, session){

    db_config <- dbConfigServer("db_config", parent_session = session)
    Sys.setenv("DB_PASSWD" = "f~3}~dcWK[<=;6Cx4");
    data_changed <- reactiveVal(0)

    tableListServer("table_list", db_config$con, db_config$schema, data_changed)
    createTableServer("create_table", db_config$con, db_config$schema, data_changed, parent_session = session)

    # Close the connection at the end of the session
    session$onSessionEnded(function() {
        if (!is.null(db_config$con())) {
            DatabaseConnector::disconnect(db_config$con())
        }
    })
}

#' bi_basic_app_launcher
#' @export
dbm_app_launcher <- function(target_port = 3838, target_host = '0.0.0.0'){
    if (file.exists("./R/initialize_db_configs.R")) init_envs()
    options(shiny.maxRequestSize = 100000 * 1024^2)
    shinyApp(ui = dbm_ui, server = dbm_server,
                    options = list(launch.browser = TRUE,
                                   port = target_port,
                                   host = target_host,
                                   java.parameters = "-Xss3m")
                    )
}
