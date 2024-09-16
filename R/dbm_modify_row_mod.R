# Module UI
modifyRowUI <- function(id, table_structure, row_data) {
  ns <- NS(id)

  column_inputs <- lapply(1:nrow(table_structure), function(i) {
    column_name <- table_structure$column_name[i]
    data_type <- table_structure$data_type[i]
    current_value <- row_data[[column_name]]

    if (column_name == "colid") {
      return(NULL)
    }

    input_field <- switch(data_type,
                          "integer" = numericInput(ns(paste0("col_", column_name)), column_name, value = as.numeric(current_value)),
                          "bigint" = numericInput(ns(paste0("col_", column_name)), column_name, value = as.numeric(current_value)),
                          "numeric" = numericInput(ns(paste0("col_", column_name)), column_name, value = as.numeric(current_value), step = 0.01),
                          "real" = numericInput(ns(paste0("col_", column_name)), column_name, value = as.numeric(current_value), step = 0.01),
                          "double precision" = numericInput(ns(paste0("col_", column_name)), column_name, value = as.numeric(current_value), step = 0.01),
                          "char" = textInput(ns(paste0("col_", column_name)), column_name, value = current_value),
                          "varchar" = textInput(ns(paste0("col_", column_name)), column_name, value = current_value),
                          "text" = textAreaInput(ns(paste0("col_", column_name)), column_name, value = current_value),
                          "date" = dateInput(ns(paste0("col_", column_name)), column_name, value = as.Date(current_value)),
                          "time" = textInput(ns(paste0("col_", column_name)), column_name, value = current_value),
                          "timestamp" = dateInput(ns(paste0("col_", column_name)), column_name, value = as.Date(current_value)),
                          "boolean" = checkboxInput(ns(paste0("col_", column_name)), column_name, value = as.logical(current_value)),
                          "uuid" = textInput(ns(paste0("col_", column_name)), column_name, value = current_value),
                          "json" = textAreaInput(ns(paste0("col_", column_name)), column_name, value = current_value),
                          textInput(ns(paste0("col_", column_name)), column_name, value = current_value)
    )

    if (grepl("enum", data_type, ignore.case = TRUE)) {
      enum_values <- dbGetQuery(con, paste0("SELECT unnest(enum_range(NULL::", data_type, "))::text AS enum_value"))$enum_value
      input_field <- selectInput(ns(paste0("col_", column_name)), column_name,
                                 choices = enum_values,
                                 selected = current_value)
    }

    input_field
  })

  tagList(
    column_inputs,
    actionButton(
      inputId = ns("modify_row"),
      label = glue::glue("Modify Row"), style = add_button_theme(),
      icon = icon("pen")
    )
  )
}

modifyRowServer <- function(id, table_name, table_structure, row_data, data_changed, con, primary_key) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$modify_row, {
      column_names <- table_structure$column_name
      values <- lapply(column_names, function(col) {

        if (col == "colid") {
          current_value <- row_data[[col]]
          return(current_value)
        }
        value <- input[[paste0("col_", col)]]
        data_type <- table_structure$data_type[table_structure$column_name == col]

        # Convertir la valeur en fonction du type de données
        switch(data_type,
               "integer" = as.integer(value),
               "bigint" = as.integer(value),
               "numeric" = as.numeric(value),
               "real" = as.numeric(value),
               "double precision" = as.numeric(value),
               "boolean" = as.logical(value),
               "date" = as.Date(value),
               "time" = value,
               "timestamp" = as.POSIXct(value),
               # Pour les autres types, laisser la valeur telle quelle
               value
        )
      })

      print(values)
      # Créer un data frame avec la ligne modifiée
      modified_row <- as.data.frame(values, stringsAsFactors = FALSE)
      names(modified_row) <- column_names

      # Construire la requête UPDATE
      update_query <- sprintf("UPDATE %s SET ", table_name)

      # Génération des clauses SET pour la mise à jour
      set_clauses <- sapply(column_names, function(col) {
        if (!is.null(modified_row[[col]])) {
          # Escape single quotes in values to prevent SQL injection and syntax errors
          safe_value <- gsub("'", "''", as.character(modified_row[[col]]), fixed = TRUE)
          sprintf("%s = '%s'", col, safe_value)
        } else {
          sprintf("%s = NULL", col)
        }
      }, USE.NAMES = FALSE)

      # Finalisation de la requête d'update avec les conditions WHERE
      update_query <- paste(
        update_query, paste(set_clauses, collapse = ", "),
        sprintf("WHERE %s = '%s'", primary_key, row_data[[primary_key]])
      )

      tryCatch({
        DBI::dbExecute(con, update_query)
        showNotification("Row modified successfully", type = "message")
        data_changed(data_changed() + 1)
      }, error = function(e) {
        print(paste("Error modifying row:", e$message))
        showNotification(paste("Error modifying row:", e$message), type = "error")
      })
    })
  })
}
