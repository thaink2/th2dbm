# Module UI
addRowUI <- function(id, table_structure) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("column_inputs")),
    actionButton(
      inputId = ns("add_row"),
      label = "Add Row",
      style = add_button_theme(),
      icon = icon("plus")
    )
  )
}

# Module Server
addRowServer <- function(id, table_name, table_structure, con, schema = "public", data_changed) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Fonction pour récupérer le vrai nom du type enum
    get_enum_type_name <- function(column_name, schema, table_name) {
      query <- paste0("
        SELECT t.typname
        FROM pg_attribute a
        JOIN pg_type t ON a.atttypid = t.oid
        WHERE a.attrelid = '", schema, ".", table_name, "'::regclass
        AND a.attname = '", column_name, "'
      ")
      result <- DatabaseConnector::dbGetQuery(con(), query)
      as.character(result[[1]])
    }

    # Fonction pour récupérer les valeurs d'un enum
    get_enum_values <- function(enum_type) {
      query <- paste0("SELECT unnest(enum_range(NULL::", enum_type, "))")
      result <- DatabaseConnector::dbGetQuery(con(), query)
      as.character(result[[1]])
    }

    # Générer les inputs pour chaque colonne
    output$column_inputs <- renderUI({
      lapply(1:nrow(table_structure), function(i) {
        column_name <- table_structure$column_name[i]
        data_type <- table_structure$data_type[i]

        if (column_name == "colid") {
          return(NULL)
        }

        if (data_type == "USER-DEFINED") {
          data_type <- get_enum_type_name(column_name = column_name, schema = schema, table_name = table_name)
        }

        input_field <- switch(data_type,
                              "numeric" = numericInput(ns(paste0("col_", column_name)), column_name, value = 0, step = 0.01),
                              "varchar" = textInput(ns(paste0("col_", column_name)), column_name, value = ""),
                              "text" = textAreaInput(ns(paste0("col_", column_name)), column_name, value = ""),
                              "date" = dateInput(ns(paste0("col_", column_name)), column_name),
                              "time" = textInput(ns(paste0("col_", column_name)), column_name, value = "00:00:00"),
                              "timestamp" = dateInput(ns(paste0("col_", column_name)), column_name),
                              "boolean" = checkboxInput(ns(paste0("col_", column_name)), column_name, value = FALSE),
                              "uuid" = textInput(ns(paste0("col_", column_name)), column_name, value = generateID("entry")),
                              {
                                # Pour les types enum
                                enum_values <- get_enum_values(data_type)
                                selectInput(ns(paste0("col_", column_name)), column_name, choices = enum_values)
                              }
        )

        div(input_field)
      })
    })

    observeEvent(input$add_row, {
      column_names <- table_structure$column_name
      values <- lapply(column_names, function(col) {
        if (col == "colid") {
          return(generateID())
        }

        value <- input[[paste0("col_", col)]]
        data_type <- table_structure$data_type[table_structure$column_name == col]

        if (data_type == "USER-DEFINED") {
          data_type <- get_enum_type_name(col, schema, table_name)
        }

        switch(data_type,
               "numeric" = as.numeric(value),
               "boolean" = as.logical(value),
               "date" = as.Date(value),
               "time" = value,
               "timestamp" = as.POSIXct(value),
               value
        )
      })

      new_row <- data.frame(setNames(values, column_names), stringsAsFactors = FALSE)

      tryCatch({
        columns <- paste(column_names, collapse = ", ")
        result <- sapply(column_names, function(x) any(grepl("enum_", x, fixed = TRUE)))
        var_enum <- column_names[result]
        print(var_enum)
        sapply(new_row, function(x) {
          print(x)
          print(var_enum)
          print(get_enum_type_name(var_enum, schema, table_name))
          print(paste('avant: ',new_row[[var_enum]]))
          if (x == new_row[[var_enum]]) {
            t <- get_enum_values(get_enum_type_name(var_enum, schema, table_name))
            print(t)
            new_row[[var_enum]] <- as.factor(new_row[[var_enum]])
          }
          print(paste('après: ',new_row[[var_enum]]))
        })

        print(new_row)
        DBI::dbWriteTable(
          conn = con(),
          name = DBI::Id(schema = schema, table = table_name),
          value = new_row,
          append = TRUE,
          row.names = FALSE
        )
        data_changed(data_changed() + 1)
        showNotification("Row added successfully", type = "message")
        removeModal()
      }, error = function(e) {
        print(paste("Error adding row:", e$message))
        showNotification(paste("Error adding row:", e$message), type = "error")
      })
    })
  })
}
