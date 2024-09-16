#' HTML Dependency for Datamods
#'
#' Creates an HTML dependency for the 'datamods' package, including
#' JavaScript and CSS assets.
#'
#' @return An `htmltools::htmlDependency` object representing the dependency.
#'
#' @importFrom htmltools htmlDependency
#' @importFrom utils packageVersion
#'
#' @noRd
html_dependency_datamods <- function() {
  htmlDependency(
    name = "datamods",
    version = packageVersion("datamods"),
    src = list(href = "datamods", file = "assets"),
    package = "datamods",
    script = "js/datamods.js",
    stylesheet = "css/datamods.css"
  )
}


#' Toggle Widget Enable/Disable State
#'
#' Enables or disables a Shiny widget based on its input ID.
#'
#' @param inputId The input ID of the widget to toggle.
#' @param enable A logical value indicating whether to enable (`TRUE`) or disable (`FALSE`) the widget.
#' @param session The Shiny session object.
#'
#' @noRd
toggle_widget <- function(inputId,
                          enable = TRUE,
                          session = getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    type = "datamods-toggleWidget",
    message = list(id = session$ns(inputId), enable = enable)
  )
}


#' Insert an Alert into a Placeholder
#'
#' Inserts a Shiny alert into a designated placeholder element in the UI.
#'
#' @param selector The ID of the alert, where the placeholder has the suffix "-placeholder".
#' @param ... Additional arguments passed to the `shinyWidgets::alert` function.
#'
#' @noRd
#'
#' @importFrom shiny removeUI insertUI
#' @importFrom shinyWidgets alert
insert_alert <- function(selector, ...) {
  removeUI(selector = paste0("#", selector, "-result"))
  insertUI(
    selector = paste0("#", selector, "-placeholder"),
    ui = alert(
      id = paste0(selector, "-result"),
      ...
    )
  )
}

#' Show UI Element
#'
#' Sends a custom message to show a specific UI element.
#'
#' @param selector The CSS selector of the element to show.
#' @param inline Whether to show the element inline or not.
#' @param id The ID of the element to show.
#' @param session The Shiny session object.
#'
#' @noRd
showUI <- function(selector = NULL,
                   inline = FALSE,
                   id = NULL,
                   session = getDefaultReactiveDomain()) {
  if (!is.null(id)) {
    id <- session$ns(id)
  }
  session$sendCustomMessage(
    type = "datamods-showUI",
    message = dropNulls(list(
      selector = selector,
      inline = inline,
      id = id
    ))
  )
}

#' Hide UI Element
#'
#' Sends a custom message to hide a specific UI element.
#'
#' @param selector The CSS selector of the element to hide.
#' @param inline Whether the element is inline or not.
#' @param id The ID of the element to hide.
#' @param session The Shiny session object.
#'
#' @noRd
hideUI <- function(selector = NULL,
                   inline = FALSE,
                   id = NULL,
                   session = getDefaultReactiveDomain()) {
  if (!is.null(id)) {
    id <- session$ns(id)
  }
  session$sendCustomMessage(
    type = "datamods-hideUI",
    message = dropNulls(list(
      selector = selector,
      inline = inline,
      id = id
    ))
  )
}

#' Enable a Tab
#'
#' Enables a specific tab in a tabsetPanel.
#'
#' @param id The ID of the tabsetPanel.
#' @param value The value of the tab to enable.
#' @param session The Shiny session object.
#'
#' @noRd
enable_tab <- function(id, value, session = getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    type = "datamods-enableTab",
    message = list(id = session$ns(id), value = value)
  )
}

#' Disable a Tab
#'
#' Disables a specific tab in a tabsetPanel.
#'
#' @param id The ID of the tabsetPanel.
#' @param value The value of the tab to disable.
#' @param session The Shiny session object.
#'
#' @noRd
disable_tab <- function(id, value, session = getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    type = "datamods-disableTab",
    message = list(id = session$ns(id), value = value)
  )
}

#' Update Tab Label
#'
#' Updates the label of a specific tab in a tabsetPanel.
#'
#' @param id The ID of the tabsetPanel.
#' @param value The value of the tab to update.
#' @param label The new label for the tab.
#' @param session The Shiny session object.
#'
#' @importFrom htmltools doRenderTags
#' @noRd
update_tab_label <- function(id, value, label, session = getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    type = "datamods-updateTabLabel",
    message = list(id = session$ns(id), value = value, label = doRenderTags(label))
  )
}


#' Create a Success Alert
#'
#' Generates a success alert message with optional information about imported data.
#'
#' @param data The imported data (optional).
#' @param trigger_return The trigger for displaying the alert ("button" or other).
#' @param btn_show_data Whether to include a button to show the data.
#' @param extra Additional content to include in the alert.
#' @param session The Shiny session object.
#'
#' @return An HTML tag list representing the success alert.
#'
#' @importFrom htmltools tagList tags
#' @importFrom shiny icon getDefaultReactiveDomain
#' @noRd
make_success_alert <- function(data,
                               trigger_return,
                               btn_show_data,
                               extra = NULL,
                               session = getDefaultReactiveDomain()) {
  if (identical(trigger_return, "button")) {
    success_message <- tagList(
      tags$b(phosphoricons::ph("check", weight = "bold"), i18n("Data ready to be imported!")),
      sprintf(
        i18n("data has %s obs. of %s variables."),
        nrow(data), ncol(data)
      ),
      extra
    )
  } else {
    success_message <- tagList(
      tags$b(phosphoricons::ph("check", weight = "bold"), i18n("Data successfully imported!")),
      sprintf(
        i18n("data has %s obs. of %s variables."),
        nrow(data), ncol(data)
      ),
      extra
    )
  }
  if (isTRUE(btn_show_data)) {
    success_message <- tagList(
      success_message,
      tags$br(),
      actionLink(
        inputId = session$ns("see_data"),
        label = tagList(phosphoricons::ph("table"), i18n("click to see data"))
      )
    )
  }
  return(success_message)
}

#' Insert an Error Alert
#'
#' Inserts an error alert into a specified placeholder.
#'
#' @param mssg The error message to display.
#' @param selector The ID of the placeholder where the alert will be inserted.
#' @param session The Shiny session object.
#'
#' @noRd
insert_error <- function(mssg = i18n("Something went wrong..."),
                         selector = "import",
                         session = getDefaultReactiveDomain()) {
  insert_alert(
    selector = session$ns(selector),
    status = "danger",
    tags$b(phosphoricons::ph("warning"), i18n("Ooops")),
    mssg
  )
}

#' Create a Help Popup
#'
#' Creates a help popup with the provided text.
#'
#' @param text The help text to display in the popup.
#'
#' @return An HTML tag list representing the help popup.
#'
#' @importFrom htmltools tagList tags doRenderTags
#' @noRd
help_popup <- function(text) {
  tagList(
    tags$span(
      phosphoricons::ph("question", title = i18n("Help")),
      `data-toggle` = "popover",
      `data-trigger` = "focus",
      title = i18n("Help"),
      `data-html` = "true",
      `data-content` = htmltools::doRenderTags(text),
      tabindex = "0",
      role = "button"
    ),
    tags$script(
      "$(function () { $(\'[data-toggle=\"popover\"]\').popover({container: 'body'}); })"
    )
  )
}

#' Create an Import Button
#'
#' Creates a styled "Import data" button.
#'
#' @param session The Shiny session object.
#'
#' @return An action button for importing data.
#'
#' @importFrom shiny actionButton icon getDefaultReactiveDomain
#' @noRd
button_import <- function(session = getDefaultReactiveDomain()) {
  actionButton(
    inputId = session$ns("confirm"),
    label = tagList(
      phosphoricons::ph("arrow-circle-right", title = i18n("Import data")),
      i18n("Import data")
    ),
    width = "100%", style = add_button_theme(),
    disabled = "disabled",
    class = "btn-primary",
    `aria-label` = i18n("Import data")
  )
}

#' Create a Close Modal Button
#'
#' Creates a button to close a modal dialog.
#'
#' @return A button element to close the modal.
#'
#' @noRd
button_close_modal <- function() {
  tags$button(
    phosphoricons::ph("x", title = i18n("Close"), height = "2em"),
    class = "btn btn-link",
    style = css(border = "0 none", position = "absolute", top = "5px", right = "5px"),
    `data-dismiss` = "modal",
    `data-bs-dismiss` = "modal",
    `aria-label` = i18n("Close")
  )
}


#' Get Primary Color from Theme
#'
#' Retrieves the primary color from the current Bootstrap theme.
#'
#' @return The primary color as a hex string. If no theme is detected, returns a default color.
#'
#' @importFrom bslib bs_current_theme is_bs_theme bs_get_variables
#' @noRd
get_primary_color <- function() {
  theme <- bslib::bs_current_theme()
  if (!bslib::is_bs_theme(theme)) {
    return("#013DFF")
  }
  primary <- bslib::bs_get_variables(theme, "primary")
  unname(primary)
}
