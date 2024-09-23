mod_table_box_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("table_box"))
}

mod_table_box_server <- function(id, target_table, mod_refresh_file, data_change = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    temp_data_dir <- "./mydata/"

    if (!dir.exists(temp_data_dir)) {
      dir.create(temp_data_dir)
    }

    tables_content <- reactive({
      data_change()
      content <- fetch_data_from_db(table = target_table)

      if (length(content) == 0) {
        return(NULL)
      }

      return(content)
    })

    output$table_box <- renderUI({
      # req(tables_content())

      table_content <- tables_content()

      data_dim <- paste(ncol(table_content), nrow(table_content), sep = " x ")

      bs4Dash::box(
        title = paste(target_table, "(", data_dim, ")"),
        status = "primary",
        solidHeader = TRUE,
        gradient = TRUE,
        collapsible = FALSE,
        closable = FALSE,
        maximizable = FALSE,
        width = 12,
        id = ns(paste0("data_card_", target_table)),
        bs4Dash::boxProfile(
          title = target_table,
          subtitle = data_dim,
          type = 2,
          image = "https://upload.wikimedia.org/wikipedia/commons/thumb/2/29/Postgresql_elephant.svg/1200px-Postgresql_elephant.svg.png"
        ),
        tags$br(),
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          actionButton(ns(paste0("open_", target_table)), "Open", style = add_button_theme(), icon = icon("eye")),
          actionButton(ns(paste0("append_", target_table)), "Add entry", icon = icon("plus"))
        ),
        icon = icon("database"),
        background = "white",
        boxToolSize = "sm",
        dropdownMenu = bs4Dash::boxDropdown(
          icon = icon("wrench"),
          bs4Dash::boxDropdownItem("Edit", id = ns(paste0("edit_", target_table)), icon = icon("pen")),
          bs4Dash::boxDropdownItem("Export", id = ns(paste0("export_", target_table)), icon = icon("download")),
          bs4Dash::boxDropdownItem("Delete", id = ns(paste0("delete_", target_table)), icon = icon("trash"))
        )
      )
    })

    observeEvent(input[[paste0("open_", target_table)]], {
      showModal(modalDialog(
        title = paste("Content of", target_table),
        size = "xl",
        easyClose = TRUE,
        footer = NULL,
        DT::DTOutput(ns(paste0("table_", target_table)))
      ))
      output[[paste0("table_", target_table)]] <- renderDT({
        data <- tables_content() %>% dplyr::select(-col_id, -id)

        DT::datatable(data, options = list(scrollX = TRUE))
      })
    })


    observeEvent(input[[paste0("explore_", target_table)]], {
      if (length(target_table) == 0) {
        return(NULL)
      }
      data_card_name <- paste0("data_card_", target_table)

      data_temp_file <- paste0(temp_data_dir, "data_", target_table, ".rds")

      saveRDS(NULL, file = data_temp_file)

      tables_content() %>% saveRDS(file = data_temp_file)
      mod_data_explorer_server(data_card_name, data_temp_file)
    })

    observeEvent(input[[paste0("delete_", target_table)]], {
      mod_delete_data_server("data_delete", target_table = target_table, mod_refresh_file = mod_refresh_file, data_change = data_change)
      mod_delete_data_ui(ns("data_delete"), target_table = target_table)
    })

    observeEvent(input[[paste0("export_", target_table)]], {
      showModal(modalDialog(
        title = paste("export", target_table, "Data"), size = "m", easyClose = TRUE, footer = target_table,
        div(
          class = "text-center",
          th2_download_button(outputId = ns("download_data"), label = "Download")
        )
      ))
    })

    observeEvent(input[[paste0("edit_", target_table)]], {
      mod_edit_data_server("data_edit", target_table = target_table, mod_refresh_file = mod_refresh_file, data_change = data_change)
      mod_edit_data_ui(ns("data_edit"))
    })

    observeEvent(input[[paste0("append_", target_table)]], {
      db_meta_list <- list(target_table = target_table)
      module_id <- generateID(prefix = target_table)
      mod_th2new_entry_server(id = module_id, db_meta = db_meta_list, refresh_file = mod_refresh_file, data_change = data_change)
      showModal(
        modalDialog(
          title = "Add entry", size = "xl", easyClose = TRUE,
          mod_th2new_entry_ui(id = ns(module_id))
        )
      )
    })


    output$download_data <- downloadHandler(filename = function() {
      paste("data_", target_table, "_", Sys.Date(), ".csv", sep = "")
    }, content = function(download_file) {
      tables_content() %>%
        readr::write_csv(x = ., file = download_file)
      removeModal()
    })
  })
}
