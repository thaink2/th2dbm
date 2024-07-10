#' mod_thaink2_local_pkg_ui
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @export
#'
#' @importFrom shiny NS tagList

mod_thaink2_local_pkg_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    uiOutput(ns("pkg_id")),
    uiOutput(ns("pkg_versions")),
    uiOutput(ns("choose_th_pkg")),
    textOutput(ns("yourChoice"))
  )
}

#' mod_thaink2_local_pkg_server
#'
#' @export
mod_thaink2_local_pkg_server <-
  function(id) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      packages <- reactive({
        cp <- get_th2_local_pkg()
        for (i in cp %>% dplyr::pull(package)) {
          cp <- get_versions_by_package(i) %>%
            dplyr::bind_rows(cp)
        }
        return(cp)
      })

      output$pkg_id <- renderUI({
        req(packages())

        pck_choices <- packages() %>%
          dplyr::pull(package) %>%
          unique()
        selectizeInput(
          inputId = ns("pkg_id"),
          label = "Packages",
          choices = pck_choices,
          multiple = TRUE
        )
      })

      output$pkg_versions <- renderUI({
        req(input$pkg_id)

        lapply(input$pkg_id, function(pkg) {
          mod_pkg_versions_server(paste0("pkg_versions", pkg), package_df = packages(), package = pkg)
          mod_pkg_versions_ui(ns(paste0("pkg_versions", pkg)))
        })
      })

      output$choose_th_pkg <- renderUI({
        actionButton(
          inputId = ns("choose_th_pkg"),
          label = "Choose",
          icon = icon("paper-plane")
        )
      })

      # choiceText <- eventReactive(input$choose_th_pkg, {
      #   input$pkg_id
      # })

      output$yourChoice <- renderText({
        # choiceText()
        paste("Vous avez sélectionné:")
        # paste(lapply(input$pkg_id, function(pkg) {
        #   id <- paste0("version_", pkg)
        #   paste(pkg, , collapse = ":")
        # }),
        # collapse = ", "))
      })

      # Afficher le choix
      observeEvent(input$choose_th_pkg, {
        th2product::th_shinyalert(
          title = "Package sélectionné avec succès!",
          text = paste(input$pkg_id, collapse = ", "),
          confirmButtonCol = "#013DFF",
          type = "success"
        )
      })
    })
  }
