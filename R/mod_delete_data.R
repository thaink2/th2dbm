
#' Module UI pour la suppression de données
#'
#' Cette fonction crée une interface utilisateur modale pour confirmer la suppression de données spécifiques.
#' Elle fournit un bouton pour initier la suppression et affiche le nom des données à supprimer.
#'
#' @param id Identifiant unique pour le module.
#' @param data_name Nom des données à supprimer.
#' @importFrom shiny NS showModal modalDialog actionButton
#' @export
mod_delete_data_ui <- function(id, target_table) {
  ns <- NS(id)
  showModal(modalDialog(
    title = paste("Delete", target_table, "Data"), size = "xl", easyClose = FALSE, footer = target_table,
    actionButton(inputId = ns("delete_data"), style = add_button_theme(), icon = icon("trash"), label = "Delete", class = "btn-primary")
  ), )
}

#' Module Server pour la suppression de données
#'
#' Cette fonction serveur gère la logique de suppression de données.
#' Elle écoute les événements du bouton de suppression dans l'UI correspondante et exécute
#' la suppression des données dans le seau S3 spécifié, tout en mettant à jour les permissions associées.
#'
#' @param id Identifiant unique pour le module.
#' @param data_name Nom des données à supprimer.
#' @param s3_bucket Nom du seau S3 où les données sont stockées.
#' @param s3_prefix Préfixe dans le seau S3 pour spécifier le chemin des données.
#' @param object_creator Nom d'utilisateur de celui qui effectue la suppression, par défaut celui du système.
#' @param refresh_file Chemin vers un fichier helper pour rafraîchir les données après suppression.
#' @importFrom shiny moduleServer observeEvent
#' @importFrom shinyalert shinyalert
#' @export
mod_delete_data_server <- function(id, target_table, mod_refresh_file) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    observeEvent(input$delete_data, {
      shinyalert::shinyalert(
        text = glue::glue("Confirm deletion of {target_table} table ?"),
        title = "Confirmation de suppression",
        type = "warning",
        closeOnClickOutside = TRUE,
        showCancelButton = TRUE,
        imageUrl = "https://raw.githubusercontent.com/thaink2/thaink2publicimages/main/thaink2_logo_circle.png",
        showConfirmButton = TRUE,
        confirmButtonCol = "#013DFF",
        confirmButtonText = "Oui, supprimer!",
        cancelButtonText = "Non, annuler!",
        callbackR = function(value) {
          tryCatch(
            {
              if (value == TRUE) {
                db_con <- connect_to_database()
                delete_query <- sprintf("DELETE FROM th2metadata_table WHERE TH2DB_TABLE = '%s'", target_table)
                DBI::dbExecute(db_con, delete_query)
                DBI::dbExecute(db_con, "DELETE FROM th2_ml_permissions WHERE TH2DB_TABLE = '%s'", target_table)
                DBI::dbExecute(db_con, glue::glue("DROP TABLE IF EXISTS {target_table}"))
                DBI::dbDisconnect(db_con)
                label_text <- "Entry successfully deleted"

                th2dbm::th_shinyalert(
                  title = "Delete Entry",
                  confirmButtonCol = "#013DFF",
                  text = label_text,
                  type = "success"
                )
              }
            },
            error = function(e) {
              shinyFeedback::showToast(
                "error",
                "Error deleting data"
              )
            }
          )
          saveRDS(object = Sys.time(), file = mod_refresh_file)
          removeModal()
        }
      )
    })
  })
}
