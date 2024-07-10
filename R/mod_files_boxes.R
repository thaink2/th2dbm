#' @export
mod_files_boxes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      uiOutput(ns("directory_path")),
      uiOutput(ns("files_boxes"))
    )
  )
}

#' @export
mod_files_boxes_server <- function(id, current_user = Sys.getenv("SHINYPROXY_USERNAME")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$directory_path <- renderUI({
      fluidRow(
        column(width = 4, textInput(inputId = ns("folder_path"), label = "Folder Path", placeholder = "reports/")),
        column(width = 4, radioButtons(inputId = ns("subdirs"), label = "Include subdirs ?", choices = c("Yes" = TRUE, "No" = FALSE), selected = FALSE)),
        column(width = 4, uiOutput(ns("button_search")))
      )
    })

    output$button_search <- renderUI({
      req(input$folder_path)
      actionButton(inputId = ns("submit"), label = "List", icon = icon("sync"))
    })

    files_details_df <- eventReactive(input$submit, {
      folder_path <- input$folder_path
      subdirs <- input$subdirs

      if (!dir.exists(folder_path)) {
        cat("Le dossier spécifié n'existe pas.\n")
        return(NULL)
      }


      # shinyalert::shinyalert(title = "Files Boxes", text = "", type = "success")
      # print(glue::glue("folder path: {folder_path} - include subdirs: {subdirs}"))
      files_details <- list_files_details(directory_path = folder_path, include_subdirs = subdirs)
      # print(files_details)

      return(files_details)
    })

    output$files_boxes <- renderUI({
      req(files_details_df())
      file_details <- files_details_df()$file_details
      all_boxes <- fluidRow(
        lapply(file_details, function(l) {
          meta <- list(file_name = l$name, file_path = l$path, file_size = l$size)
          mod_file_details_box_server(id = (l$name), file_meta = meta)
          column(width = 5, mod_file_details_box_ui(ns(l$name)))
        })
      )

      return(all_boxes)
    })
  })
}
