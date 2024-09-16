#' Lancement de l'application de gestion de base de données
#'
#' Cette fonction lance l'application Shiny pour la gestion de base de données.
#' Elle initialise l'environnement de l'application si nécessaire, puis démarre l'application Shiny.
#'
#' @param target_port Le port sur lequel exécuter l'application (par défaut : 3838).
#' @param target_host L'adresse hôte à laquelle lier l'application (par défaut : "0.0.0.0", ce qui signifie que l'application sera accessible depuis n'importe quelle adresse IP sur l'hôte local).
#'
#' @export
launch_dbm_app <- function(target_port = 3838, target_host = "0.0.0.0") {
  if (file.exists("./R/initialize_db_configs.R")) {
    init_envs()
  }
  shinyApp(
    ui = th2dbm::app_ui,
    server = th2dbm::app_server,
    options = list(
      launch.browser = TRUE,
      port = target_port,
      host = target_host
    )
  )
}
