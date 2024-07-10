#' @export
mod_pkg_versions_ui <- function(id = NULL) {
  ns <- NS(id)

  fluidPage(
    uiOutput(ns("versions_select"))
  )
}

#' @export
mod_pkg_versions_server <- function(id, package_df = NULL, current_user = Sys.getenv("SHINYPROXY_USERNAME"), package = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$versions_select <- renderUI({
      selectInput(
        inputId = ns(paste0("version_", package)),
        label = glue::glue("Versions de {package}"),
        choices = package_df %>%
          dplyr::filter(package == !!package) %>%
          dplyr::arrange(desc(version)) %>%
          dplyr::pull(version),
        selected = ""
      )
    })
  })
}
