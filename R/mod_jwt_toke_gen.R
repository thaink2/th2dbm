mod_jwt_gen_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("jwt_inputs"))
}

mod_jwt_gen_server <- function(id, target_table, mod_refresh_file) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$jwt_inputs <- renderUI({
      fluidRow(
        column(width = 2, uiOutput(ns("user_mail"))),
        column(width = 2, uiOutput(ns("target_service"))),
        column(width = 3, uiOutput(ns("addition_info"))),
        column(width = 3, uiOutput(ns("generate_token")))
      )
    })

    output$target_service <- renderUI({
      textInput(inputId = ns("target_service"), label = "Service", placeholder = "forecasting")
    })
    output$user_mail <- renderUI({
      textInput(inputId = ns("user_mail"),label = "User Mail", placeholder = "test@thaink2.com")
    })
    output$addition_info <- renderUI({
      textInput(inputId = ns("addition_info"), label = "Additional Infos", placeholder = "scope")
    })
    output$generate_token <- renderUI({
      req(input$user_mail,input$target_service)
      actionButton(inputId = ns("generate_token"), label = "Generate", icon = icon("key"))
    })

    observeEvent(input$generate_token,{
      jwt_claim <- jose::jwt_claim(user_mail = input$user_mail,
                                   target_service = input$target_service,
                                   addition_info = input$addition_info)
      encrypted_token <- jose::jwt_encode_hmac(claim  = jwt_claim, secret = Sys.getenv("ENCRYPT_PASS"))
      showModal(
        modalDialog(title = "JWT Token Value", size = "xl",
                    h2(encrypted_token))
      )
      Sys.sleep(5)
      removeModal()
    })
    #============
  })
}

#' launch_jst_gen_app
#' @export
launch_jwt_gen_app <- function(){
  library(shiny)
  ui <- fluidPage(
    title = "thaink² services token generator",
    mod_jwt_gen_ui("test")
  )
  server <- function(input, output, session) {
    mod_jwt_gen_server("test")
  }
  shinyApp(ui, server, options = list(launch.browser = T, port = 3838, host = "0.0.0.0"))
}

