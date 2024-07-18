#' @export
#'
#' @importFrom shiny NS tagList
mod_beta_tester_manage_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(width = 3, uiOutput(ns("testing_add_button"))),
      column(width = 4, uiOutput(ns("manage_beta_tester_button")))
    ),
    DT::dataTableOutput(ns("use_db_table_dt"))
  )
}

#' @export
mod_beta_tester_manage_server <- function(id, target_table = "beta_tester_table") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mod_refresh_file <- create_refresh_helper_file(mod_id = id)
    refresh_statement <- shiny::reactiveFileReader(intervalMillis = 3000, session = session, filePath = mod_refresh_file, readFunc = readRDS)

    tester_table <- reactive({
      req(refresh_statement())
      db_con <- connect_to_database()
      available_tables <- DBI::dbListTables(conn = db_con)
      if (!target_table %in% available_tables) {
        return(NULL)
      }
      test_table <- DBI::dbReadTable(conn = db_con, name = target_table)
      DBI::dbDisconnect(db_con)
      return(test_table)
    })

    alert_shown <- reactiveVal(FALSE) # Initialize a reactive value

    output$testing_add_button <- renderUI({
      actionButton(inputId = ns("add_test_entry"), label = "Add entry", icon = icon("plus"))
    })


    observeEvent(input$add_test_entry, {
      if (!have_permission_to_manage(target_object = target_table)) {
        th2dbm::th_shinyalert(
          title = "Permission warning",
          confirmButtonCol = "#013DFF",
          text = glue::glue("{verifier_format_email(Sys.getenv('SHINYPROXY_USERNAME'))} you are not authorized to add new entry into '{target_table}'"), type = "error"
        )

        return(NULL)
      }
      db_meta_list <- list(target_table = target_table)
      module_id <- generateID(prefix = target_table)
      mod_th2new_entry_server(id = module_id, db_meta = db_meta_list, refresh_file = mod_refresh_file)
      showModal(
        modalDialog(
          title = "Add entry", size = "l",
          mod_th2new_entry_ui(id = ns(module_id))
        )
      )
    })

    output$use_db_table_dt <- DT::renderDataTable(
      {
        req(tester_table())
        if (!have_permission_to_see(target_object = target_table)) {
          th2dbm::th_shinyalert(
            title = "Permission warning",
            confirmButtonCol = "#013DFF",
            text = glue::glue("{verifier_format_email(Sys.getenv('SHINYPROXY_USERNAME'))} you are not authorized to add new entry into '{target_table}'"), type = "error"
          )

          return(NULL)
        }
        tester_table() %>%
          prepare_db_table_view(target_table = target_table)
      },
      server = TRUE
    )

    observeEvent(input$use_db_table_dt_rows_selected, {
      if (!have_permission_to_edit(target_object = target_table)) {
        th2dbm::th_shinyalert(
          title = "Permission warning",
          confirmButtonCol = "#013DFF",
          text = glue::glue("{verifier_format_email(Sys.getenv('SHINYPROXY_USERNAME'))}  you are not authorized to add new entry into '{target_table}'"), type = "error"
        )
        return(NULL)
      }

      rows_selected <- input$use_db_table_dt_rows_selected
      print(rows_selected)
      db_meta_list <- list(target_table = target_table, target_row = tester_table()[rows_selected, ])
      module_id <- generateID(prefix = "update_test_entry")
      mod_row_selected_options_server(module_id, target_table = target_table, target_row = tester_table()[rows_selected, ], refresh_file = mod_refresh_file)
      showModal(
        modalDialog(
          title = "Entry Action", size = "m",
          icon = icon("pen"),
          mod_row_selected_options_ui(ns(module_id))
        )
      )
    })

    observe({
      invalidateLater(1000, session) # Rafraichit la fonction chaque seconde
      current_time <- Sys.time()
      tester_data <- tester_table()

      expired_entries <- tester_data[tester_data$EXPIRATION_DATE < current_time, ]

      # S'il n'y a pas d'entrées expirées, arrêtez la fonction
      if (nrow(expired_entries) == 0 || is.null(expired_entries)) {
        return("No expired products to notify")
      }

      body <- "<p>Voici la liste des produits dont la date d'expiration est passee :</p>"
      body <- paste(body, "<ul>")
      for (i in seq_len(nrow(expired_entries))) {
        body <- paste(body, sprintf(
          "<li>Produit : %s, Assigned to : %s, expiry date : %s</li>",
          expired_entries$PRODUCT[i],
          expired_entries$USER_MAIL[i],
          expired_entries$EXPIRATION_DATE[i]
        ))
      }
      body <- paste(body, "</ul>")
      # Traiter chaque entrée expirée
      if (!alert_shown()) { # check if entries are expired and if alert is not
        alert_shown(TRUE)
      }
    })
  })
}
