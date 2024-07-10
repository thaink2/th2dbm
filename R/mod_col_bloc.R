library(glue)
library(dplyr)
library(shiny)
library(magrittr)

#' @export
mod_col_bloc_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("columns_id"))
}

#' @export
mod_col_bloc_server <- function(id, tab_name = "test_table",
                                current_user = Sys.getenv("SHINYPROXY_USERNAME"),
                                meta = list(col_id = NULL, col_name = "", col_unique = FALSE, col_type = "text", col_choice = "", col_choose_var = "", col_choose_tab = ""), indice = "") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    c_ids <- "COLUMN_IDs.csv"

    if (meta$col_unique == "false") {
      meta$col_unique <- FALSE
    } else if (meta$col_unique == "true") {
      meta$col_unique <- TRUE
    }

    # UI pour l'entrée des données
    output$columns_id <- renderUI({
      columns_types <- c("text", "numeric", "choices", "text_area", "date_time", "boolean", "current_user", "password", "uuid", "CHOOSE_FROM_TABLE_var") # CHOOSE_FROM_TABLE_var

      fluidRow(
        column(width = 2, textInput(inputId = ns("column_name_id"), label = glue::glue("Column name"), value = meta$col_name)),
        column(width = 2, checkboxInput(inputId = ns("primary_key"), label = glue::glue("Unique"), value = meta$col_unique)),
        column(width = 3, selectInput(inputId = ns("column_type_id"), label = "Column type", choices = columns_types, selected = meta$col_type)),
        column(width = 2, uiOutput(ns("COLUMN_CHOICES"))),
        column(width = 2, uiOutput(ns("CHOOSE_FROM_VAR")))
      )
    })

    output$COLUMN_CHOICES <- renderUI({
      req(input$column_type_id)
      if (input$column_type_id == "choices") {
        col_choices <- strsplit(meta$col_choice, split = ",") %>% unlist()

        selectizeInput(inputId = ns("COLUMN_CHOICES"), choices = col_choices, selected = col_choices, options = list(create = TRUE), label = "Add choices", multiple = TRUE)
      } else if (input$column_type_id == "CHOOSE_FROM_TABLE_var") {
        db_con <- connect_to_database()
        available_tables <- DBI::dbListTables(conn = db_con)
        available_fields <- available_tables[available_tables != "th2metadata_table"]
        selectInput(inputId = ns("CHOOSE_FROM_TABLE"), label = "Tagret Table", choices = available_fields, selected = meta$col_choose_tab)
      }
    })
    output$CHOOSE_FROM_VAR <- renderUI({
      req(input$CHOOSE_FROM_TABLE)
      db_con <- connect_to_database()
      available_fields <- DBI::dbListFields(conn = db_con, name = input$CHOOSE_FROM_TABLE)
      selectInput(inputId = ns("CHOOSE_FROM_VAR"), label = "Target Column", choices = available_fields, selected = meta$col_choose_var)
    })


    columns_infos <- reactive({
      req(input$column_name_id)
      req(input$column_type_id)
      # req(input$COLUMN_CHOICES)
      column_choice <- ""
      CHOOSE_FROM_VAR <- ""
      CHOOSE_FROM_TABLE <- ""

      if (input$column_type_id == "choices") {
        column_choice <- input$COLUMN_CHOICES
      }
      if (input$column_type_id == "CHOOSE_FROM_TABLE_var") {
        CHOOSE_FROM_VAR <- input$CHOOSE_FROM_VAR
        CHOOSE_FROM_TABLE <- input$CHOOSE_FROM_TABLE
      }
      if (input$primary_key == FALSE) {
        primary_key <- "false"
      } else if (input$primary_key == TRUE) {
        primary_key <- "true"
      }

      c(VAR_ID = toupper(input$column_name_id), VAR_UNIQUE = primary_key, VAR_TYPE = input$column_type_id, COLUMN_CHOICES = column_choice, CHOOSE_FROM_VAR = CHOOSE_FROM_VAR, CHOOSE_FROM_TABLE = CHOOSE_FROM_TABLE)
    })

    observeEvent(columns_infos(), {
      # Lire ou initialiser le dataframe
      if (file.exists(c_ids)) {
        columns_df <- read.csv(c_ids, stringsAsFactors = FALSE)
      } else {
        if (!is.null(meta$col_id)) indice <- meta$col_id
        columns_df <- data.frame(TH2DB_TABLE = tab_name, VAR_ID = "", VAR_TYPE = "", VAR_UNIQUE = "", COLUMN_ID = indice, COLUMN_CHOICES = "", CHOOSE_FROM_TABLE = "", CHOOSE_FROM_VAR = "", stringsAsFactors = FALSE)
      }

      # Convertir les choix en chaîne de caractères pour stockage
      COLUMN_CHOICES <- ifelse(is.null(input$COLUMN_CHOICES), "", paste(input$COLUMN_CHOICES, collapse = ","))

      new_var_id <- columns_infos()["VAR_ID"]
      new_entry <- data.frame(
        TH2DB_TABLE = tab_name, VAR_ID = new_var_id, VAR_TYPE = columns_infos()["VAR_TYPE"], VAR_UNIQUE = columns_infos()["VAR_UNIQUE"], COLUMN_ID = indice,
        COLUMN_CHOICES = COLUMN_CHOICES, CHOOSE_FROM_TABLE = columns_infos()["CHOOSE_FROM_TABLE"], CHOOSE_FROM_VAR = columns_infos()["CHOOSE_FROM_VAR"], stringsAsFactors = FALSE
      )

      # Mettre à jour ou ajouter la nouvelle entrée
      if (indice %in% columns_df$COLUMN_ID) {
        # Mise à jour de la ligne existante
        idx <- which(columns_df$COLUMN_ID == indice)
        columns_df[idx, ] <- new_entry
      } else {
        # Ajout de la nouvelle entrée
        columns_df <- rbind(columns_df, new_entry)
      }

      # Écrire le dataframe mis à jour dans le fichier CSV
      write.csv(columns_df, c_ids, row.names = FALSE)
    })
  })
}
