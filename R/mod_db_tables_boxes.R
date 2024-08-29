#' @encoding UTF-8

#' mod_db_tables_boxes
#'
#' @export
mod_db_tables_boxes_ui <- function(id) {
  ns <- NS(id)

  # bs4Dash::tabBox(
  #   width = 12,
  #   title = div(style = "display: flex; ", tagList(uiOutput(ns("refresh")))),
  #   uiOutput(ns("db_tables_box"))
  # )
  fluidRow(
    column(width = 12, offset = 10, uiOutput(ns("refresh"))),
    column(width = 12, uiOutput(ns("db_tables_box")))
  )
}

#' mod_db_tables_boxes
#'
#' @export
mod_db_tables_boxes_server <- function(id, mod_refresh_file) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Envelopper l'ensemble du code du module serveur dans un bloc tryCatch
    tryCatch(
      {
        # 1. Amélioration : Gestion des erreurs lors de la connexion à la base de données
        db_con <- connect_to_database()

        refresh_statement <- shiny::reactiveFileReader(
          intervalMillis = 1000,
          session = session,
          filePath = mod_refresh_file,
          readFunc = readRDS
        )

        output$refresh <- renderUI({
          actionButton(inputId = ns("refresh"), label = "", style = add_button_theme(), icon = icon("arrows-rotate"), class = "btn-primary")
        })

        observeEvent(input$refresh, {
          saveRDS(object = Sys.time(), file = mod_refresh_file)
        })

        # 2. Amélioration : Fonction séparée pour récupérer la liste des tables
        get_tables_list <- function() {
          tryCatch(
            {
              available_tables <- fetch_data_from_db(table = "th2metadata_table", col = c("th2db_table"))
              available_unique_tables <- available_tables %>% dplyr::distinct(th2db_table)
              return(available_unique_tables)
            },
            error = function(e) {
              showModal(modalDialog(
                title = "Erreur lors de la récupération des tables",
                p("Une erreur s'est produite lors de la récupération de la liste des tables :"),
                pre(as.character(e)),
                easyClose = TRUE
              ))
              return(NULL)
            }
          )
        }

        # 3. Amélioration : `tables_list` est maintenant réactif
        tables_list <- reactive({
          req(refresh_statement())
          get_tables_list()
        })


        observe({
          req(tables_list())
          available_tables <- tables_list() %>% dplyr::pull(th2db_table)

          if (length(available_tables) == 0) {
            toast(
              title = "Aucune table disponible",
              options = list(
                autohide = TRUE,
                class = "bg-red",
                position = "topRight"
              )
            )
          }

          # 5. Amélioration : Gestion des erreurs dans la création des boîtes
          table_list <- tryCatch(
            {
              lapply(available_tables, function(x) {
                module_id <- th2dbm::generateID(prefix = "data_card")
                mod_table_box_server(id = module_id, target_table = x, mod_refresh_file = mod_refresh_file)
                column(width = 4, mod_table_box_ui(id = ns(module_id)))
              })
            },
            error = function(e) {
              showModal(modalDialog(
                title = "Erreur lors de la création des boîtes",
                p("Une erreur s'est produite lors de la création des boîtes de table :"),
                pre(as.character(e)),
                easyClose = TRUE
              ))
              return(NULL)
            }
          )
          output$db_tables_box <- renderUI({
            do.call(shiny::fluidRow, table_list)
          })
        })

        # Fin du bloc tryCatch principal
      },
      error = function(e) {
        # Afficher un message d'erreur générique à l'utilisateur
        showModal(modalDialog(
          title = "Erreur inattendue",
          p("Une erreur inattendue s'est produite :"),
          pre(as.character(e)),
          easyClose = TRUE
        ))
      }
    )
  })
}
