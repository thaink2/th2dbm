#' launch_dbm_app
#' @export
launch_dbm_app <- function(target_port = 3838, target_host = "0.0.0.0"){
  shiny::shinyApp(ui = th2dbm::app_ui,
                  server = th2dbm::app_server,
                  options = list(launch.browser = TRUE,
                                 port = target_port,
                                 host = target_host))
}
