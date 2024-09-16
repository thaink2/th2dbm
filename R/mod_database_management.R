#' mod_th2_database_management_ui
#'
#' @description Interface utilisateur pour le module de gestion de la base de données du package th2dbm.
#'
#' Cette interface utilisateur fournit les éléments pour la création, la suppression et la mise à jour des tables dans la base de données.
#' Elle génère dynamiquement les composants de l'interface utilisateur en fonction de l'interaction de l'utilisateur et des actions sélectionnées.
#'
#' @param id Un identifiant unique pour les composants de l'interface utilisateur du module shiny.
#'
#' @export
#'
#' @importFrom shiny NS tagList

mod_th2_database_management_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(width = 3, uiOutput(ns("create_table"))),
    column(width = 3, uiOutput(ns("create_table_with_csv"))),
    column(width = 3, uiOutput(ns("del_table_name"))),
  )
}

#' mod_th2_database_management_server
#'
#' @description Logique serveur pour le module de gestion de la base de données du package th2dbm.
#'
#' Ce module serveur gère la logique des tâches de gestion de la base de données telles que la création de nouvelles tables,
#' la suppression de tables, la mise à jour des métadonnées des tables et la gestion des entrées des colonnes. Il utilise
#' la programmation réactive pour répondre aux entrées des utilisateurs et déclencher les opérations de base de données en conséquence.
#'
#' @param id Un identifiant unique pour les fonctions du serveur du module shiny.
#' @param current_user Le nom d'utilisateur de l'utilisateur courant interagissant avec le module,
#'                     par défaut celui obtenu de la variable d'environnement "SHINYPROXY_USERNAME".
#' @param action L'action à entreprendre, pouvant être 'create', 'update_tab', ou d'autres opérations sur la base de données.
#'               'create' est utilisé par défaut si non spécifié.
#'
#' @export
#'
#' @importFrom shiny moduleServer observeEvent renderUI showModal modalDialog fluidPage
#'              fluidRow column textInput numericInput renderUI selectInput
#' @importFrom DBI dbConnect dbGetQuery dbDisconnect dbExecute
#' @importFrom shinyalert shinyalert
#' @importFrom glue glue

mod_th2_database_management_server <-
  function(id, current_user = Sys.getenv("SHINYPROXY_USERNAME"), menu_items = NULL, action = "create", mod_refresh_file) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      mod_refresh_file <- create_refresh_helper_file(mod_id = id)
      refresh_statement <- reactiveFileReader(intervalMillis = 3000, session = session, filePath = mod_refresh_file, readFunc = readRDS)

      c_ids <- "COLUMN_IDs.csv"
      if (file.exists(c_ids)) {
        file.remove(c_ids)
      }

      # création du button
      output$create_table <- renderUI({
        if (action == "create") {
          action_label <- "create"
          action_icon <- "plus"
        } else if (action == "update_tab") {
          action_label <- "update"
          action_icon <- "pen"
        } else if (action == "delete_tab") {
          action_label <- "delete"
          action_icon <- "trash"
        }

        premiere_lettre_maj <- toupper(substr(action_label, 1, 1))
        reste_du_texte_min <- tolower(substr(action_label, 2, nchar(action_label)))
        action_label_Cap <- paste0(premiere_lettre_maj, reste_du_texte_min)

        actionButton(
          inputId = ns(glue::glue("{action_label}_table")),
          label = glue::glue("{action_label_Cap} table"), style = add_button_theme(),
          icon = icon(action_icon)
        )
      })

      output$create_table_with_csv <- renderUI({
        if (action == "create") {
          actionButton(
            inputId = ns(glue::glue("create_table_with_csv")),
            label = glue::glue("Create table with csv"), style = add_button_theme(),
            icon = icon("table")
          )
        }
      })

      observeEvent(input$create_table_with_csv, {
        module_id <- generateID()
        mod_csv_to_db_server(id = module_id)
        showModal(
          modalDialog(
            title = "Add CSV file", size = "l",
            mod_csv_to_db_ui(id = ns(module_id))
          )
        )
      })

      # ====================== CREATE TABLE ===========================================

      observeEvent(input$create_table, {
        showModal(modalDialog(
          size = "l",
          title = "Important message",
          fluidPage(
            fluidRow(
              column(width = 2, uiOutput(ns("table_name"))),
              column(width = 2, uiOutput(ns("table_num_items")))
            ),
            uiOutput(ns("columns_id")),
            uiOutput(ns("create_table_button"))
          )
        ))
      })

      output$table_name <- renderUI({
        textInput(
          inputId = ns("table_name"),
          label = "Table Name",
          value = "",
          placeholder = "sales"
        )
      })

      output$table_num_items <- renderUI({
        numericInput(
          inputId = ns("table_num_items"),
          label = "Number of Fields",
          value = NULL
        )
      })



      observeEvent(input$table_num_items, {
        req(input$table_num_items, input$table_name)
        temp <- 1:input$table_num_items
        lapply(temp, function(i) {
          new_col_id <- generateID(prefix = glue::glue("{input$table_name}_"))
          mod_col_bloc_server(paste0("column_", i), tab_name = input$table_name, indice = new_col_id)
        })
      })

      output$columns_id <- renderUI({
        req(input$table_num_items)
        temp <- 1:input$table_num_items
        columns_id <- lapply(temp, function(i) {
          mod_col_bloc_ui(ns(paste0("column_", i)))
        })
        columns_id
      })



      # ================== UPDATE TABLE ===================================
      observeEvent(input$update_table, {
        showModal(modalDialog(
          size = "l",
          title = "Table update",
          fluidPage(
            fluidRow(
              column(width = 2, uiOutput(ns("update_table_name"))),
              column(width = 2, uiOutput(ns("table_num_items")))
            ),
            uiOutput(ns("columns_id")),
            uiOutput(ns("create_table_button"))
          )
        ))
      })

      output$update_table_name <- renderUI({
        db_con <- connect_to_database()
        available_fields <- DBI::dbGetQuery(conn = db_con, "SELECT TH2DB_TABLE FROM th2metadata_table")
        DBI::dbDisconnect(db_con)
        selectInput(inputId = ns("table_name"), label = "Table Name", choices = available_fields, selected = "test_table")
      })

      output$del_table_name <- renderUI({
        req(input$delete_table)
        db_con <- connect_to_database()
        available_fields <- DBI::dbGetQuery(conn = db_con, "SELECT TH2DB_TABLE FROM th2metadata_table")
        DBI::dbDisconnect(db_con)
        selectInput(inputId = ns("table_name_del"), label = "Table Name", choices = available_fields, selected = NULL)
      })

      observe({
        req(input$table_name_del)
        target_table <- input$table_name_del
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
            if (isTRUE(value)) {
              db_con <- connect_to_database()
              delete_query <- sprintf("DELETE FROM th2metadata_table WHERE TH2DB_TABLE = '%s'", target_table)
              DBI::dbExecute(db_con, delete_query)
              DBI::dbExecute(db_con, "DELETE FROM th2_ml_permissions WHERE TH2DB_TABLE = '%s'", target_table)
              DBI::dbExecute(db_con, glue::glue("DROP TABLE IF EXISTS {target_table}"))
              DBI::dbDisconnect(db_con)
              label_text <- "Entry successfully deleted"
              saveRDS(object = Sys.time(), file = mod_refresh_file)
              th2dbm::th_shinyalert(
                title = "Delete Entry",
                confirmButtonCol = "#013DFF",
                text = label_text,
                type = "success"
              )
            }
            saveRDS(object = Sys.time(), file = mod_refresh_file)
            removeModal()
          }
        )
      })

      # ===================== SAVE BUTTON ==============================

      output$create_table_button <- renderUI({
        req(input$table_num_items, input$table_name)
        actionButton(
          inputId = ns("save_button"), style = add_button_theme(),
          label = "Save",
          icon = icon("save")
        )
      })

      observeEvent(input$save_button, {
        db_con <- connect_to_database()
        if (!file.exists(c_ids)) {
          shinyalert::shinyalert(
            title = glue::glue("Le fichier CSV {c_ids} n'existe pas"),
            text = "",
            confirmButtonCol = "#013DFF",
            type = "error"
          )
          return(NULL)
        }
        search_query <- sprintf("SELECT * FROM th2metadata_table WHERE TH2DB_TABLE = '%s'", input$table_name)
        search_query <- DBI::dbGetQuery(db_con, search_query)
        DBI::dbDisconnect(db_con)

        if (nrow(search_query) != 0 & action == "create") {
          th2dbm::th_shinyalert(
            title = glue::glue("{input$table_name} table already exists !"),
            confirmButtonCol = "#013DFF",
            text = "",
            type = "error"
          )

          removeModal()
          return("table already exists !")
        }
        test_fields_type <- readr::read_csv(c_ids, col_types = readr::cols(VAR_UNIQUE = readr::col_character()))

        if (!"COL_ID" %in% search_query$var_id) {
          id_entry <- data.frame(
            TH2DB_TABLE = input$table_name, VAR_ID = "COL_ID", VAR_TYPE = "uuid", COLUMN_ID = generateID(prefix = glue::glue("{input$table_name}")),
            VAR_UNIQUE = "true", stringsAsFactors = FALSE
          )
          test_fields_type <- dplyr::bind_rows(test_fields_type, id_entry)
        }

        # Trouver les VAR_ID de test_fields présents dans search_query
        matched_ids <- test_fields_type$VAR_ID[test_fields_type$VAR_ID %in% search_query$var_id]

        # Si au moins un VAR_ID correspond, afficher un message
        if (length(matched_ids) > 0) {
          th2dbm::th_shinyalert(
            title = glue("Les VAR_ID suivants de test_fields existent dans search_query : {paste(matched_ids, collapse = ', ')}"),
            text = "",
            confirmButtonCol = "#013DFF",
            type = "error"
          )
          return(NULL)
        } else {
          colnames(search_query) <- toupper(colnames(search_query))
          test_fields_type <- dplyr::bind_rows(search_query, test_fields_type)
        }



        colnames(test_fields_type) <- toupper(colnames(test_fields_type))

        test_fields <- test_fields_type
        bd_response <- create_update_metadata(table_metadata = test_fields_type, action_table = action)

        if (input$table_name != "th2_ml_permissions") {
          permissions_value <- data.frame(
            OBJECT_ID = input$table_name, OBJECT_CREATOR = current_user,
            PERMITTED_USERS = current_user, OBJECT_TYPE = "db_table",
            PERMISSION_LEVEL = "Owner", PERMISSION_TIME = paste(Sys.time())
          )

          add_entry_to_table(new_entry = permissions_value, target_table = "th2_ml_permissions")
        }
        th2dbm::th_shinyalert(
          title = glue::glue("{input$table_name} created successfully"),
          text = "",
          confirmButtonCol = "#013DFF",
          type = "success"
        )

        file.remove(c_ids)
        # Fermer le modal
        saveRDS(object = Sys.time(), file = mod_refresh_file)
        removeModal()
      })
    })
  }
