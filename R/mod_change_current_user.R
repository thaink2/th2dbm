#' @export
mod_change_current_user_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    uiOutput(ns("change_user")),
    actionButton(inputId = ns("change_user_button"), label = "", icon = icon("sync"))
  )
}

#' @export
mod_change_current_user_server <-
  function(id) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      print(Sys.getenv("SHINYPROXY_USERNAME"))


      db_con <- connect_to_database()
      req_statement <- glue::glue("SELECT DISTINCT USER_ID FROM users_table")
      var_choices <- DBI::dbGetQuery(conn = db_con, statement = req_statement) %>%
        dplyr::pull(user_id)
      DBI::dbDisconnect(conn = db_con)

      output$change_user <- renderUI({
        selectInput(inputId = ns("change_user"), label = "Current user", choices = var_choices, selected = Sys.getenv("SHINYPROXY_USERNAME"))
      })

      observeEvent(input$change_user_button, {
        Sys.setenv("SHINYPROXY_USERNAME" = input$change_user)
        th2product::th_shinyalert(
          title = "Change user",
          text = glue::glue("The current user is {Sys.getenv('SHINYPROXY_USERNAME')}"),
          confirmButtonCol = "#013DFF",
          type = "success"
        )
        removeModal()
        session$reload()
      })
    })
  }
