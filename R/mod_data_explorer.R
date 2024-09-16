#' data_explorer_ui Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shiny NS tagList
#' @export

mod_data_explorer_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(width = 12, uiOutput(ns("data_explore_actions")))
  )
}


#' data_explorer_server Server Functions
#' @export
mod_data_explorer_server <- function(id, data_temp_file) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    button_theme <- add_button_theme()

    temp_data <- reactiveFileReader(intervalMillis = 1000, session, filePath = data_temp_file, readFunc = readRDS)

    output$data_view <- DT::renderDT({
      temp_data() %>%
        DT::datatable(
          options = list(scrollX = TRUE)
        )
    })


    edited_data <- update_variables_server(id = "edit_data_cols", data = reactive({
      temp_data()
    }))

    filtered_data <- filter_data_server(id = "filter_data", data = reactive({
      temp_data()
    }))




    output$data_explore_actions <- renderUI({
      fluidPage(
        fluidRow(
          column(width = 2, actionButton(inputId = ns("data_view_action"), style = button_theme, label = "", icon = icon("eye"), class = "btn-primary")),
          column(width = 2, actionButton(inputId = ns("data_edit_action"), style = button_theme, label = "", icon = icon("pen"), class = "btn-primary")),
          column(width = 2, actionButton(inputId = ns("data_filter_action"), style = button_theme, label = "", icon = icon("filter"), class = "btn-primary"))
        )
      )
    })

    observeEvent(input$data_view_action, {
      showModal(modalDialog(
        title = paste("Data Explorer"), icon = icon("eye"), easyClose = TRUE, size = "xl",
        DT::DTOutput(ns("data_view")),
        footer = list(modalButton("Cancel", icon = icon("xmark")))
      ))
    })

    observeEvent(input$data_edit_action, {
      showModal(modalDialog(
        title = paste("Data Explorer"), easyClose = TRUE, size = "xl",
        update_variables_ui(id = ns("edit_data_cols"), title = "Data Editor"),
        footer = list(
          modalButton("Cancel", icon = icon("xmark"))
        )
      ))
    })

    observeEvent(input$data_filter_action, {
      showModal(modalDialog(
        title = paste("Data Explorer"), easyClose = TRUE, size = "xl",
        filter_data_ui(id = ns("filter_data")),
        footer = list(
          actionButton(inputId = ns("filter_data_save"), style = button_theme, label = "Filter", icon = icon("save"), class = "btn-primary"),
          modalButton("Cancel", icon = icon("xmark"))
        )
      ))
    })


    observeEvent(edited_data(), {
      saveRDS(edited_data(), data_temp_file)
      removeModal()
    })

    observeEvent(input$filter_data_save, {
      if (nrow(filtered_data$filtered()) > 0) {
        saveRDS(filtered_data$filtered(), data_temp_file)
      }
      shinyalert::shinyalert(
        title = "Data Explorer",
        imageUrl = "https://raw.githubusercontent.com/thaink2/thaink2publicimages/main/thaink2_logo_circle.png",
        text = "Data Filtered", type = "success", confirmButtonCol = "#013DFF"
      )
      removeModal()
    })
  })
}
