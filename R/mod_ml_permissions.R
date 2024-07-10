#' @export
mod_ml_permissions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(width = 4, uiOutput(ns("object_id"))),
        column(width = 4, uiOutput(ns("permitted_user"))),
        column(width = 2, uiOutput(ns("permission_level")))
      ),
      uiOutput(ns("grant_button"))
    )
  )
}

#' @export
mod_ml_permissions_server <- function(id, target_perm_table = "th2_ml_permissions", OBJECT_ID = NULL, OBJECT_TYPE = "pipeline", current_user = Sys.getenv("CURRENT_USER"), action = NULL, refresh_file = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    if (!have_permission_to_manage(target_object = target_perm_table)) {
      th2product::th_shinyalert(
        title = "Permission warning",
        confirmButtonCol = "#013DFF",
        text = glue::glue("{verifier_format_email(Sys.getenv('SHINYPROXY_USERNAME'))}, Vous n'êtes pas autorisé à ajouter une entrée à la table '{target_perm_table}'"), type = "error"
      )

      return(NULL)
    }
    output$object_id <- renderUI({
      db_con <- connect_to_database()
      req_statement <- glue::glue("SELECT DISTINCT OBJECT_ID FROM {target_perm_table}")
      var_choices <- DBI::dbGetQuery(conn = db_con, statement = req_statement)
      DBI::dbDisconnect(conn = db_con)
      if (is.null(OBJECT_ID)) {
        selectInput(inputId = ns("object_id"), label = glue::glue("Object ID"), choices = var_choices, selected = OBJECT_ID)
      } else {
        textInput(inputId = ns("object_id"), label = glue::glue("Object ID"), value = OBJECT_ID)
      }
    })

    output$permitted_user <- renderUI({
      var_choices <- th2utils::get_list_of_users()
      selectInput(inputId = ns("permitted_user"), label = glue::glue("Permitted user"), choices = var_choices, selected = NULL)
    })

    output$permission_level <- renderUI({
      selectInput(inputId = ns("permission_level"), label = glue::glue("Permission level"), choices = c("Edit", "View", "Manage", "None", "Deploy"), selected = NULL)
    })

    output$grant_button <- renderUI({
      req(input$permitted_user, input$permission_level)

      actionButton(inputId = ns("grant_button"), label = "Grant", icon = icon("check"))
    })

    observeEvent(input$grant_button, {
      if (is.null(OBJECT_ID)) {
        OBJECT_ID <- input$object_id
      }
      resp <- grant_ml_permission_db(
        target_table = target_perm_table,
        meta = list(
          OBJECT_ID = OBJECT_ID,
          CURRENT_OBJECT_TYPE = OBJECT_TYPE,
          current_PERMITTED_USERS = input$permitted_user,
          current_PERMISSION_LEVEL = input$permission_level
        ),
        permission_action = "grant"
      )
      refresh_file %>% saveRDS(object = Sys.time(), file = .)
      removeModal()
      th2product::th_shinyalert(
        title = "Permissions",
        confirmButtonCol = "#013DFF",
        text = resp
      )

    })
  })
}
