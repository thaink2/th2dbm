#' mod_th2_entry_ui
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @export
#'
#' @importFrom shiny NS tagList
mod_th2_entry_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("entry_box"))
  )
}

#' mod_th2entry_server
#'
#' @export
mod_th2_entry_server <- function(id, entry_data = NULL, current_user = Sys.getenv("SHINYPROXY_USERNAME"), entry_meta = list(entry_name = "text", entry_type = "NUMERIC", entry_unique = "false", CHOOSE_FROM_VAR = "feature", CHOOSE_FROM_TABLE = "features_table", entry_choices = "", entry_table = "test_table")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$entry_value <- renderUI({
      if (entry_meta$entry_type == "text") {
        textInput(inputId = ns("entry_value"), label = "", value = entry_data)
      } else if (entry_meta$entry_type == "text_area") {
        textAreaInput(
          inputId = ns("entry_value"), label = "", width = 300,
          height = 100, value = entry_data
        )
      } else if (entry_meta$entry_type %in% c("numeric", "NUMERIC")) {
        numericInput(inputId = ns("entry_value"), label = "", value = entry_data)
      } else if (entry_meta$entry_type %in% c("date_time", "date")) {
        if (is.null(entry_data)) {
          entry_data <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        }

        shinyDatetimePickers::datetimeMaterialPickerInput(inputId = ns("entry_value"), label = "", value = entry_data)
      } else if (entry_meta$entry_type %in% c("BOOLEAN", "boolean")) {
        selectInput(inputId = ns("entry_value"), label = "", choices = c(TRUE, FALSE), selected = entry_data)
      } else if (entry_meta$entry_type %in% c("choices")) {
        entry_choices <- strsplit(entry_meta$entry_choices, split = ",") %>% unlist()
        selectInput(inputId = ns("entry_value"), label = "", choices = entry_choices, selected = entry_data)
      } else if (entry_meta$entry_type %in% c("CHOOSE_FROM_VAR")) {
        entry_choices <- strsplit(entry_meta$entry_choices, split = ",") %>% unlist()
        selectInput(inputId = ns("entry_value"), label = "", choices = entry_choices, selected = entry_data)
      } else if (entry_meta$entry_type %in% c("current_user")) {
        selectInput(inputId = ns("entry_value"), label = "", choices = Sys.getenv("SHINYPROXY_USERNAME"), selected = Sys.getenv("SHINYPROXY_USERNAME"))
      } else if (entry_meta$entry_type %in% c("uuid")) {
        if (is.null(entry_data)) {
          uuid_value <- generateID(entry_meta$entry_table)
        } else {
          uuid_value <- entry_data
        }
        textInput(inputId = ns("entry_value"), label = "", value = uuid_value)
      } else if (entry_meta$entry_type %in% c("password")) {
        passwordInput(inputId = ns("entry_value"), label = "", value = entry_data)
      } else if (entry_meta$entry_type %in% c("CHOOSE_FROM_TABLE_var")) {
        db_con <- connect_to_database()
        CHOOSE_FROM_VAR <- entry_meta$CHOOSE_FROM_VAR
        CHOOSE_FROM_TABLE <- entry_meta$CHOOSE_FROM_TABLE
        req_statement <- glue::glue("SELECT DISTINCT {CHOOSE_FROM_VAR} FROM {CHOOSE_FROM_TABLE}")
        var_choices <- DBI::dbGetQuery(conn = db_con, statement = req_statement) %>%
          dplyr::pull(!!CHOOSE_FROM_VAR)
        DBI::dbDisconnect(conn = db_con)
        selectInput(inputId = ns("entry_value"), label = "", choices = var_choices, selected = entry_data)
      } else {
        textInput(inputId = ns("entry_value"), label = "", value = entry_data)
      }
    })

    randomStatus <- reactive({
      sample(c("primary", "success", "info", "warning", "danger", "navy", "teal", "purple"), 1)
    })
    output$entry_box <- renderUI({
      bs4Dash::box(
        title = entry_meta$entry_name, status = randomStatus()[1], width = 12, background = "white", solidHeader = TRUE,
        uiOutput(ns("entry_value"))
      )
    })

    observeEvent(input$entry_value, {
      entry_temp_file <- "./new_entry_temp_file.rds"
      if (entry_meta$entry_type == "password") {
        df_entry <- data.frame(password = input$entry_value)
        current_entry <- df_entry %>%
          encryptr::encrypt(password) %>%
          pull(password)
      } else {
        current_entry <- input$entry_value
      }


      if (file.exists(entry_temp_file)) {
        entry_values <- readRDS(entry_temp_file)
        entry_values[[entry_meta$entry_name]] <- current_entry
      } else {
        entry_values <- list()
        entry_values[[entry_meta$entry_name]] <- current_entry
      }
      saveRDS(entry_values, entry_temp_file)
    })
  })
}



#' mod_th2new_entry_ui
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @export
#'
#' @importFrom shiny NS tagList
mod_th2new_entry_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      uiOutput(ns("all_entries")),
      uiOutput(ns("save_new_entries"))
    )
  )
}

#' mod_th2new_entry_server
#'
#' @export
mod_th2new_entry_server <- function(id, db_meta = list(
  target_table = "test_table",
  current_user = Sys.getenv("SHINYPROXY_USERNAME"),
  target_row = data.frame(),
  hidden_vars = list()
),
  data_change = NULL,
refresh_file = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    target_table <- db_meta$target_table

    entry_temp_file <- "./new_entry_temp_file.rds"

    saveRDS(list(), entry_temp_file)

    entry_values <- reactiveFileReader(intervalMillis = 1000, session = session, filePath = entry_temp_file, readFunc = readRDS)
    vars_table_metadata <- vars_table_metadata(current_target_table = target_table)

    output$all_entries <- renderUI({
      req(vars_table_metadata)
      current_row <- db_meta$target_row
      metadata <- vars_table_metadata
      temp_var <- 1:nrow(metadata)
      all_entries <- fluidRow(
        lapply(temp_var, function(x) {
          current_row_type <- metadata[x, "VAR_TYPE"]
          current_row_value <- current_row[1, metadata[x, "VAR_ID"]]
          if (current_row_type == "date_time") {
            current_row_value <<- paste(as.POSIXct(as.numeric(current_row_value)))
          }

          mod_th2_entry_server(
            id = metadata[x, "VAR_ID"],
            entry_data = current_row_value,
            entry_meta = list(
              entry_name = metadata[x, "VAR_ID"],
              entry_type = metadata[x, "VAR_TYPE"],
              entry_unique = metadata[x, "VAR_UNIQUE"],
              entry_choices = metadata[x, "COLUMN_CHOICES"],
              CHOOSE_FROM_TABLE = metadata[x, "CHOOSE_FROM_TABLE"],
              CHOOSE_FROM_VAR = metadata[x, "CHOOSE_FROM_VAR"],
              entry_table = target_table
            )
          )
          column(width = 4, mod_th2_entry_ui(ns(metadata[x, "VAR_ID"])))
        })
      )

      return(all_entries)
    })

    output$save_new_entries <- renderUI({
      vars_table_metadata <- vars_table_metadata(current_target_table = target_table)
      req(entry_values())
      req(vars_table_metadata)

      if (length(entry_values()) == nrow(vars_table_metadata)) {
        if (is.null(db_meta$target_row) || nrow(db_meta$target_row) == 0) {
          label_text <- "Save"
        } else {
          label_text <- "Update"
        }
        actionButton(inputId = ns("save_update_entries"), label = label_text, icon = icon("save"))
      }
    })


    observeEvent(input$save_update_entries,
                 {
                   if (target_table == "data_connection_params") {
                     entry_values_df <<- data.frame(entry_values())
                     cols_to_encrypt <- names(entry_values_df)[grepl("^PARAM", names(entry_values_df))]
                     entry_values_df[cols_to_encrypt] <- lapply(entry_values_df[cols_to_encrypt], encrypt_column)
                   } else {
                     entry_values_df <- data.frame(entry_values())
                   }
                   entry_value_encrypt <<- entry_values_df

                   if (is.null(db_meta$target_row) || nrow(db_meta$target_row) == 0) {
                     response <- add_entry_to_table(new_entry = entry_values_df, target_table = db_meta$target_table)
                   } else {
                     response <- update_entry_in_table(
                       updated_entry = data.frame(entry_values()),
                       target_table = db_meta$target_table,
                       unique_id_col = "COL_ID",
                       unique_id_val = db_meta$target_row[1, "COL_ID"]
                     ) # fonction qui est dans database_helper
                   }
                   remove_shiny_inputs(id = id, .input = input)
                   refresh_file %>% saveRDS(object = Sys.time(), file = .)
                   removeModal()
                   data_change(data_change() + 1)
                   th2product::th_shinyalert(
                     title = "New Entry",
                     text = glue::glue(response$message),
                     confirmButtonCol = "#013DFF",
                     type = response$status
                   )
                 },
                 ignoreNULL = TRUE,
                 ignoreInit = TRUE
    )
  })
}
