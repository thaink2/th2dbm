#' launch_dbm_app
#' @export
launch_dbm_app <- function(target_port = 3838, target_host = "0.0.0.0") {
  if (file.exists("./R/initialize_db_configs.R")) {
    th2dbm:::init_envs()
  } else {
    stop("Please initiate envs and run the 'init_envs()' function before launching the app")
  }
  shiny::shinyApp(
    ui = th2dbm::app_ui,
    server = th2dbm::app_server,
    options = list(
      launch.browser = TRUE,
      port = target_port,
      host = target_host
    )
  )
}
