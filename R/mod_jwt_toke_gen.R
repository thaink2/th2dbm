#' Prepare the Application Header
#' Creates a custom header for a Shiny application, including the application logo and links.
#' @export
prepare_bi_app_header <- function(app_title = "BI & Reporting", app_logo = NULL, app_logo_circle = NULL,logo_href = NULL) {
  if(is.null(app_logo_circle))app_logo_circle <- "https://raw.githubusercontent.com/thaink2/thaink2publicimages/main/thaink2_logo_circle.png"
  if(is.null(app_logo))app_logo <- "https://static.wixstatic.com/media/9aacb8_9886b3a143c2470d96ec76a181e67e49~mv2.png/v1/fill/w_195,h_46,al_c,q_85,usm_0.66_1.00_0.01,enc_auto/thaink2-logo-blue-big.png"
  if(is.null(logo_href))logo_href <- "https://www.thaink2.com/en"
  bs4Dash::bs4DashNavbar(
    skin = "light",
    title = bs4Dash::bs4DashBrand(
      title = bs4Dash::bs4DashBrand(title = app_title, image = app_logo_circle)
    ),
    rightUi = shiny::tagList(
      shiny::tags$li(
        class = "nav-item dropdown",
        shiny::tags$a(
          href = logo_href,
          target = "_blank",
          class = "nav-link",
          shiny::tags$img(src = app_logo, style = "height: 30px")
        )
      )
    )
  )
}

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
        column(width = 3, br(), uiOutput(ns("generate_token")))
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
      actionButton(inputId = ns("generate_token"), style = add_button_theme(), label = "Generate", icon = icon("key"))
    })

    observeEvent(input$generate_token,{
      jwt_claim <- jose::jwt_claim(user_mail = input$user_mail,
                                   target_service = input$target_service,
                                   addition_info = input$addition_info)
      encrypted_token <- jose::jwt_encode_hmac(claim  = jwt_claim, secret = Sys.getenv("ENCRYPT_PASS"))
      showModal(
        modalDialog(title = "JWT Token Value", size = "xl",
                    fluidPage(
                      h6(encrypted_token)
                    )
      ))
      Sys.sleep(5)
      removeModal()
    })
    #============
  })
}

add_button_theme <- function(btn_col = "#fff", background_col = "#013DFF", border_col = "#013DFF") {
  style <- glue::glue("color: {btn_col}; background-color: {background_col}; border-color: {border_col};
                                border-radius: 10px;
                               border-width: 2px")
  return(style)
}

jwt_app_ui <- function(request){
  bs4Dash::bs4DashPage(
    options = list(sidebarExpandOnHover = TRUE),
    header = prepare_bi_app_header(app_title = "JWT generator"),
    sidebar = bs4Dash::bs4DashSidebar(
      id = "sidebar",
      bs4Dash::bs4SidebarMenu(
        bs4Dash::bs4SidebarMenuItem(
          "JWT Generator",
          icon = icon("key"),
          tabName = "jwt_generator",
          startExpanded = TRUE)
      )
    ),
    body = bs4Dash::dashboardBody(
      mod_jwt_gen_ui("thaink2jwt")
    )
  )
}
#' launch_jst_gen_app
#' @export
launch_jwt_gen_app <- function(){
  jwt_app_server <- function(input, output, session){
    mod_jwt_gen_server("thaink2jwt")
  }
  shiny::shinyApp(ui = jwt_app_ui, server = jwt_app_server, options = list(launch.browser = T, port = 3838, host = "0.0.0.0"))
}

