

#' @export
passwordInputCustom <- function(inputId = NULL, label = NULL, value = "") {
  htmltools::tagQuery(passwordInput(inputId = inputId, label = label, value = value))$find("input")$
    addAttrs("autocomplete" = "new-password")$allTags()
}

#' @export
textInputCustom <- function(inputId = NULL, label = NULL, value = "", maxlength = 50) {
  htmltools::tagQuery(textInput(inputId = inputId, label = label, value = value))$find("input")$
    addAttrs("maxlength" = maxlength)$allTags()
}

#' Add Button Theme
#'
#' Creates CSS styles for a button with custom colors for text, background, and border.
#'
#' @param btn_col The text color of the button.
#' @param background_col The background color of the button.
#' @param border_col The border color of the button.
#'
#' @return A CSS style string for the button.
#'
#' @export
add_button_theme <- function(btn_col = "#fff", background_col = "#013DFF", border_col = "#013DFF") {
  style <- glue::glue("color: {btn_col}; background-color: {background_col}; border-color: {border_col};
                                border-radius: 10px;
                               border-width: 2px")
  return(style)
}

#' Create a Download Button
#'
#' Creates a styled download button for Shiny applications.
#'
#' @param outputId The ID of the output element associated with the download.
#' @param label The text label displayed on the button.
#' @param class Additional CSS classes to apply to the button.
#' @param ... Other arguments passed to the underlying `tags$a` function.
#' @param icon The icon to display on the button (default: download icon).
#'
#' @return An HTML 'a' tag representing the download button.
#'
#' @export
th2_download_button <- function(outputId, label = "Download", class = NULL, ..., icon = icon("download")) {
  tags$a(
    id = outputId, class = paste(
      "btn btn-primary shiny-download-link",
      class
    ), href = "", target = "_blank", download = NA,
    label, ...
  )
}
