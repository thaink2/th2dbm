# Module UI
addRowUI <- function(id, table_structure) {
  ns <- NS(id)

  column_inputs <- lapply(1:nrow(table_structure), function(i) {
    column_name <- table_structure$column_name[i]
    data_type <- table_structure$data_type[i]

    input_field <- switch(data_type,
                          "integer" = numericInput(ns(paste0("col_", column_name)), column_name, value = 0),
                          "bigint" = numericInput(ns(paste0("col_", column_name)), column_name, value = 0),
                          "numeric" = numericInput(ns(paste0("col_", column_name)), column_name, value = 0, step = 0.01),
                          "real" = numericInput(ns(paste0("col_", column_name)), column_name, value = 0, step = 0.01),
                          "double precision" = numericInput(ns(paste0("col_", column_name)), column_name, value = 0, step = 0.01),
                          "char" = textInput(ns(paste0("col_", column_name)), column_name, value = ""),
                          "varchar" = textInput(ns(paste0("col_", column_name)), column_name, value = ""),
                          "text" = textAreaInput(ns(paste0("col_", column_name)), column_name, value = ""),
                          "date" = dateInput(ns(paste0("col_", column_name)), column_name),
                          "time" = textInput(ns(paste0("col_", column_name)), column_name, value = "00:00:00"),
                          "timestamp" = dateInput(ns(paste0("col_", column_name)), column_name),
                          "boolean" = checkboxInput(ns(paste0("col_", column_name)), column_name, value = FALSE),
                          "uuid" = textInput(ns(paste0("col_", column_name)), column_name, value = ""),
                          "json" = textAreaInput(ns(paste0("col_", column_name)), column_name, value = "{}"),
                          textInput(ns(paste0("col_", column_name)), column_name, value = "")
    )

    div(
      input_field,
      if (grepl("enum", data_type, ignore.case = TRUE)) {
        selectInput(ns(paste0("col_", column_name)), column_name,
                    choices = c("", strsplit(gsub("[()]", "", data_type), ",")[[1]]))
      }
    )
  })

  tagList(
    column_inputs,
    actionButton(ns("add_row"), "Add Row")
  )
}

addRowServer <- function(id, table_name, table_structure, con) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$add_row, {
      column_names <- table_structure$column_name
      values <- lapply(column_names, function(col) {
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

      # Créer un data frame avec une seule ligne
      new_row <- as.data.frame(values, stringsAsFactors = FALSE)
      names(new_row) <- column_names

      tryCatch({
        DBI::dbWriteTable(con(), table_name, new_row, append = TRUE, row.names = FALSE)
        showNotification("Row added successfully", type = "message")
        removeModal()
      }, error = function(e) {
        showNotification(paste("Error adding row:", e$message), type = "error")
      })
    })
  })
}
