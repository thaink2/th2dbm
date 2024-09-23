#' UI for table list
#'
#' This function creates the user interface for the table list module.
#'
#' @param id The module ID
#' @return A fluidRow containing the table boxes
#' @export
#' @importFrom shiny NS fluidRow column uiOutput
tableListUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(width = 12, uiOutput(ns("table_boxes")))
  )
}

#' Server function for table list
#'
#' This function contains the server logic for the table list module.
#'
#' @param id The module ID
#' @param con Reactive expression containing the database connection
#' @param schema Reactive expression containing the database schema
#' @return None
#' @export
#' @importFrom shiny moduleServer reactive req observeEvent showModal modalDialog removeModal showNotification
#' @importFrom bs4Dash box boxDropdown boxDropdownItem
#' @importFrom DT renderDT datatable
#' @importFrom DatabaseConnector dbGetQuery dbExecute
tableListServer <- function(id, con, schema, data_changed) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    column_types <- c(
      "varchar", "integer", "bigint", "numeric", "real", "double precision",
      "char", "text", "date", "time", "timestamp",
      "boolean", "uuid", "json", "Choice"
    )


    get_tables <- reactive({
      req(con())
      data_changed()
      tryCatch({
        DatabaseConnector::dbGetQuery(con(), paste0("
          SELECT
            table_name,
            pg_size_pretty(pg_total_relation_size(quote_ident(table_name))) AS size,
            (SELECT count(*) FROM information_schema.columns WHERE table_name=tables.table_name AND table_schema='", schema(), "') AS columns,
            pg_stat_get_live_tuples(pg_class.oid) AS rows
          FROM information_schema.tables
          JOIN pg_class ON tables.table_name = pg_class.relname
          WHERE table_schema = '", schema(), "'
        "))
      }, error = function(e) {
        showNotification(paste("Error fetching tables:", e$message), type = "error")
        print(paste("Error fetching tables:", e$message))
        return(data.frame())
      })
    })

    output$table_boxes <- renderUI({
      req(con())
      tables <- get_tables()
      tables <- tables[tables$table_name != "th2metadata_table", ]
      if (nrow(tables) == 0) {
        return(p("No tables found in the database."))
      }
      fluidRow(
        lapply(1:nrow(tables), function(i) {
          column(
            width = 3,
            bs4Dash::box(
              title = tables$table_name[i],
              status = "primary",
              solidHeader = TRUE,
              width = NULL,
              dropdownMenu = bs4Dash::boxDropdown(
                icon = icon("wrench"),
                # bs4Dash::boxDropdownItem("Add", id = ns(paste0("add_", tables$table_name[i])), icon = icon("plus")),
                bs4Dash::boxDropdownItem("Edit", id = ns(paste0("modify_", tables$table_name[i])), icon = icon("pen")),
                bs4Dash::boxDropdownItem("Delete", id = ns(paste0("delete_", tables$table_name[i])), icon = icon("trash"))
              ),
              p(paste("Size:", tables$size[i])),
              p(paste("Columns:", tables$columns[i])),
              p(paste("Rows:", tables$rows[i])),
              div(
                style = "display: flex; justify-content: space-between; align-items: center;",
                actionButton(ns(paste0("open_", tables$table_name[i])), "Open", style = add_button_theme(), icon = icon("eye")),
                actionButton(ns(paste0("add_", tables$table_name[i])), "Add entry", icon = icon("plus"))
              )
            )
          )
        })
      )
    })

    observe({
      tables <- get_tables()
      tables <- tables[tables$table_name != "th2metadata_table", ]

      lapply(tables$table_name, function(table_name) {

        get_table_structure <- reactive({
          data_changed()
          DBI::dbGetQuery(con(), paste0("SELECT column_name, data_type FROM information_schema.columns WHERE table_name = '", table_name, "'"))
        })
        input_al_id <- generateID(table_name)
        # Ouverture de la table
        observeEvent(input[[paste0("open_", table_name)]], {
          showModal(modalDialog(
            title = paste("Content of", table_name),
            size = "xl",
            easyClose = TRUE,
            footer = NULL,
            DT::DTOutput(ns(paste0("table_", table_name)))
          ))
          output[[paste0("table_", table_name)]] <- renderDT({
            data_changed()
            data <- DBI::dbGetQuery(con(), paste("SELECT * FROM", table_name))
            data <- data %>% dplyr::select(-colid)

            DT::datatable(data, options = list(scrollX = TRUE))
          })
        })

        observeEvent(input[[paste0("table_", table_name, "_rows_selected")]], {
          selected_row <- input[[paste0("table_", table_name, "_rows_selected")]]

          if (length(selected_row) == 0) {
            showNotification("Please select a row to modify", type = "warning")
            return()
          }

          data <- dbGetQuery(con(), paste("SELECT * FROM", table_name))
          row_data <- data[selected_row, ]

          module_id <- generateID(prefix = "update_test_entry")
          dbm_selected_row_server(module_id, table_name = table_name, row_data = row_data, selected_row = selected_row, data_changed = data_changed, con = con)
          showModal(
            modalDialog(
              title = "Entry Action", size = "m", easyClose = TRUE,
              icon = icon("pen"),
              dbm_selected_row_ui(ns(module_id))
            )
          )
        })

        # Ajout d'une entrée
        observeEvent(input[[paste0("add_", table_name)]], {
          module_id <- generateID(prefix = "add_entry")
          addRowServer(id = module_id, table_name = table_name, schema = schema(), table_structure = get_table_structure(), con = con, data_changed = data_changed)
          showModal(modalDialog(
            title = paste("Add Entry to:", table_name),
            size = "xl",
            easyClose = TRUE,
            bs4Dash::tabBox(
              width = 12, selected = "Add Entry",
              tabPanel("Add Entry",
              addRowUI(ns(module_id), table_structure = get_table_structure()),
              )
            ),
            footer = modalButton("Close")
          ))
        })

        output[[paste0("entry_", input_al_id)]] <- renderUI({
          columns <- get_table_structure()$column_name
          lapply(columns, function(column) {
            textInput(ns(paste0("entry_", input_al_id, "_", column)), column)
          })
        })

        # Modification de la table
        observeEvent(input[[paste0("modify_", table_name)]], {
          showModal(modalDialog(
            title = paste("Modify Table:", table_name),
            size = "xl",
            easyClose = TRUE,
            bs4Dash::tabBox(
              width = 12, selected = "Add Column",
              tabPanel("Add Column",
                       selectInput(ns(paste0("new_column_type_", input_al_id)), "Column Type", choices = column_types),
                       textInput(ns(paste0("new_column_", input_al_id)), "New Column Name"),
                       actionButton(ns(paste0("add_column_", input_al_id)), "Add Column",style = add_button_theme(), icon = icon("plus"))
              ),
              tabPanel("Delete Column",
                       uiOutput(ns(paste0("column_to_delete_", input_al_id))),
                       actionButton(ns(paste0("delete_column_btn_", input_al_id)), "Delete Column" ,style = add_button_theme(), icon = icon("trash"))
              ),
              tabPanel("Rename Column",
                       uiOutput(ns(paste0("column_to_rename_", input_al_id))),
                       textInput(ns(paste0("new_column_name_", input_al_id)), "New Column Name"),
                       actionButton(ns(paste0("rename_column_", input_al_id)), "Rename Column", style = add_button_theme(), icon = icon("pen"))
              ),
              tabPanel("Change Column Type",
                       uiOutput(ns(paste0("column_to_change_", input_al_id))),
                       selectInput(ns(paste0("new_column_type_change_", input_al_id)), "New Column Type", choices = column_types),
                       actionButton(ns(paste0("change_column_type_", input_al_id)), "Change Type", style = add_button_theme(), icon = icon("pen"))
              ),
              tabPanel("Set/Unset Unique",
                       uiOutput(ns(paste0("column_unique_", input_al_id))),
                       checkboxInput(ns(paste0("is_unique_", input_al_id)), "Is Unique"),
                       actionButton(ns(paste0("set_unique_", input_al_id)), "Apply", style = add_button_theme(), icon = icon("pen"))
              )
            ),
            footer = modalButton("Close")
          ))
        })

        output[[paste0("column_to_delete_", input_al_id)]] <- renderUI({
          columns <- get_table_structure()$column_name
          selectInput(ns(paste0("column_to_delete_", input_al_id)), "Select Column", choices = columns)
        })

        output[[paste0("column_to_rename_", input_al_id)]] <- renderUI({
          columns <- get_table_structure()$column_name
          selectInput(ns(paste0("column_to_rename_", input_al_id)), "Select Column", choices = columns)
        })

        output[[paste0("column_to_change_", input_al_id)]] <- renderUI({
          columns <- get_table_structure()$column_name
          selectInput(ns(paste0("column_to_change_", input_al_id)), "Select Column", choices = columns)
        })

        output[[paste0("column_unique_", input_al_id)]] <- renderUI({
          columns <- get_table_structure()$column_name
          selectInput(ns(paste0("column_unique_", input_al_id)), "Select Column", choices = columns)
        })

        # Add Column
        observeEvent(input[[paste0("add_column_", input_al_id)]], {
          new_column <- input[[paste0("new_column_", input_al_id)]]
          column_type <- input[[paste0("new_column_type_", input_al_id)]]
          if (column_type == "Choice") {
            choice_values <- input[[paste0("choice_values_", input_al_id)]]
            enum_name <- paste0(table_name, "_", new_column, "_enum")
            enum_query <- paste0("CREATE TYPE ", enum_name, " AS ENUM (", paste0("'", gsub(",", "','", choice_values), "'"), ")")
            DBI::dbExecute(con(), enum_query)
            column_type <- enum_name
          }
          query <- paste0("ALTER TABLE ", table_name, " ADD COLUMN ", new_column, " ", column_type)
          tryCatch({
            DBI::dbExecute(con(), query)
            showNotification("Column added successfully", type = "message")
            data_changed(data_changed() + 1)
          }, error = function(e) {
            showNotification(paste("Error adding column:", e$message), type = "error")
            print(paste("Error adding column:", e$message))
          })
        })

        # Delete Column
        observeEvent(input[[paste0("delete_column_btn_", input_al_id)]], {
          column_to_delete <- input[[paste0("column_to_delete_", input_al_id)]]
          query <- paste0("ALTER TABLE ", table_name, " DROP COLUMN ", column_to_delete)
          tryCatch({
            DBI::dbExecute(con(), query)
            showNotification("Column deleted successfully", type = "message")
            data_changed(data_changed() + 1)
          }, error = function(e) {
            showNotification(paste("Error deleting column:", e$message), type = "error")
            print(paste("Error deleting column:", e$message))
          })
        })

        # Rename Column
        observeEvent(input[[paste0("rename_column_", input_al_id)]], {
          old_name <- input[[paste0("column_to_rename_", input_al_id)]]
          new_name <- input[[paste0("new_column_name_", input_al_id)]]
          query <- paste0("ALTER TABLE ", table_name, " RENAME COLUMN ", old_name, " TO ", new_name)
          tryCatch({
            DBI::dbExecute(con(), query)
            showNotification("Column renamed successfully", type = "message")
            data_changed(data_changed() + 1)
          }, error = function(e) {
            showNotification(paste("Error renaming column:", e$message), type = "error")
            print(paste("Error renaming column:", e$message))
          })
        })

        # Change Column Type
        observeEvent(input[[paste0("change_column_type_", input_al_id)]], {
          column <- input[[paste0("column_to_change_", input_al_id)]]
          new_type <- input[[paste0("new_column_type_change_", input_al_id)]]
          if (new_type == "Choice") {
            choice_values <- input[[paste0("choice_values_change_", input_al_id)]]
            enum_name <- paste0(table_name, "_", column, "_enum")
            enum_query <- paste0("CREATE TYPE ", enum_name, " AS ENUM (", paste0("'", gsub(",", "','", choice_values), "'"), ")")
            DBI::dbExecute(con(), enum_query)
            new_type <- enum_name
          }
          query <- paste0("ALTER TABLE ", table_name, " ALTER COLUMN ", column, " TYPE ", new_type)
          tryCatch({
            DBI::dbExecute(con(), query)
            showNotification("Column type changed successfully", type = "message")
            data_changed(data_changed() + 1)
          }, error = function(e) {
            showNotification(paste("Error changing column type:", e$message), type = "error")
            print(paste("Error changing column type:", e$message))
          })
        })

        # Set/Unset Unique
        observeEvent(input[[paste0("set_unique_", input_al_id)]], {
          column <- input[[paste0("column_unique_", input_al_id)]]
          is_unique <- input[[paste0("is_unique_", input_al_id)]]
          if (is_unique) {
            query <- paste0("ALTER TABLE ", table_name, " ADD CONSTRAINT ", column, "_unique UNIQUE (", column, ")")
          } else {
            query <- paste0("ALTER TABLE ", table_name, " DROP CONSTRAINT IF EXISTS ", column, "_unique")
          }
          tryCatch({
            DBI::dbExecute(con(), query)
            showNotification("Unique constraint updated successfully", type = "message")
            data_changed(data_changed() + 1)
          }, error = function(e) {
            showNotification(paste("Error updating unique constraint:", e$message), type = "error")
            print(paste("Error updating unique constraint:", e$message))
          })
        })

        # Suppression de la table
        observeEvent(input[[paste0("delete_", table_name)]], {
          showModal(modalDialog(
            title = paste("Delete Table:", table_name), easyClose = TRUE,
            p("Are you sure you want to delete this table? This action cannot be undone."),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns(paste0("confirm_delete_", input_al_id)), "Delete", style = add_button_theme(), icon = icon("trash"))
            )
          ))
        })

        observeEvent(input[[paste0("confirm_delete_", input_al_id)]], {
          query <- paste0("DROP TABLE ", table_name)
          tryCatch({
            DBI::dbExecute(con(), query)
            showNotification("Table deleted successfully", type = "message")
            data_changed(data_changed() + 1)
          }, error = function(e) {
            showNotification(paste("Error deleting table:", e$message), type = "error")
            print(paste("Error deleting table:", e$message))
          })
          removeModal()
        })
      })
    })

  })
}
