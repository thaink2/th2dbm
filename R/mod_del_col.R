#' @export
mod_del_col_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("delete_col"))
}

#' @export
mod_update_metadata_server <-
  function(id) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
    })
  }
