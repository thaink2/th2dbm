#' Select, rename and convert variables
#'
#' @param id Module id. See [moduleServer()].
#' @param title Module's title, if \code{TRUE} use the default title,
#'  use \code{NULL} for no title or a \code{shiny.tag} for a custom one.
#'
#' @return A \code{reactive} function returning the updated data.
#' @export
#'
#' @name update-variables
#'
#' @importFrom shiny uiOutput actionButton icon
#' @importFrom htmltools tagList tags
#' @importFrom shinyWidgets html_dependency_pretty textInputIcon dropMenu
#'
update_variables_ui <- function(id, title = TRUE) {
  ns <- NS(id)
  if (isTRUE(title)) {
    title <- tags$h4(
      i18n$t("Update & select variables"),
      class = "datamods-title"
    )
  }
  tags$div(
    class = "datamods-update",
    html_dependency_pretty(),
    title,
    tags$div(
      style = "min-height: 25px;",
      tags$div(
        uiOutput(outputId = ns("data_info"), inline = TRUE),
        tagAppendAttributes(
          dropMenu(
            placement = "bottom-end",
            actionButton(
              inputId = ns("settings"),
              label = phosphoricons::ph("gear"),
              class = "pull-right float-right",
              style = add_button_theme()
            ),
            textInputIcon(
              inputId = ns("format"),
              label = i18n$t("Date format:"),
              value = "%Y-%m-%d",
              icon = list(phosphoricons::ph("clock"))
            ),
            textInputIcon(
              inputId = ns("origin"),
              label = i18n$t("Date to use as origin to convert date/datetime:"),
              value = "1970-01-01",
              icon = list(phosphoricons::ph("calendar"))
            ),
            textInputIcon(
              inputId = ns("dec"),
              label = i18n$t("Decimal separator:"),
              value = ".",
              icon = list("0.00")
            )
          ),
          style = "display: inline;color: white; background-color: #013DFF; border-color: #013DFF;
                                border-radius: 10px;
                               border-width: 2px"
        )
      ),
      tags$br(),
      toastui::datagridOutput(outputId = ns("table"))
    ),
    tags$br(),
    tags$div(
      id = ns("update-placeholder"),
      shinyWidgets::alert(
        id = ns("update-result"),
        status = "warning",
        phosphoricons::ph("info"),
        color = "#007bff",
        i18n$t(paste(
          "Select, rename and convert variables in table above,",
          "then apply changes by clicking button below."
        ))
      )
    ),
    actionButton(
      inputId = ns("validate"),
      label = tagList(
        phosphoricons::ph("arrow-circle-right", title = i18n$t("Apply changes")),
        i18n$t("Apply changes")
      ),
      width = "100%",
      style = add_button_theme()
    )
  )
}

#' @export
#'
#' @param id Module's ID
#' @param data a \code{data.frame} or a \code{reactive} function returning a \code{data.frame}.
#' @param height Height for the table.
#'
#' @rdname update-variables
#'
#' @importFrom shiny moduleServer reactiveValues reactive renderUI reactiveValuesToList validate need reactiveVal
#' @importFrom rlang call2 expr
#' @importFrom data.table setorderv
update_variables_server <- function(id, data, height = NULL) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns
      updated_data <- reactiveValues(x = NULL)
      rv_ignit <- reactiveVal(0)

      data_r <- reactive({
        if (is.reactive(data)) {
          data()
        } else {
          data
        }
      })

      output$data_info <- renderUI({
        req(data_r())
        data <- data_r()
        sprintf(i18n$t("Data has %s observations and %s variables."), nrow(data), ncol(data))
      })

      variables_r <- reactive({
        validate(
          need(data(), i18n$t("No data to display."))
        )
        data <- data_r()
        updated_data$x <- NULL
        summary_vars(data)
      })

      output$table <- toastui::renderDatagrid({
        req(variables_r())
        variables <- variables_r()
        update_variables_datagrid(
          variables,
          height = height,
          selectionId = ns("row_selected"),
          buttonId = "validate"
        )
      })

      observeEvent(input$table_data,
        {
          ignit <- rv_ignit()
          if (ignit > 0) {
            updated_data$list_rename <- NULL
            updated_data$list_select <- NULL
            updated_data$list_mutate <- NULL
            data <- data_r()
            new_selections <- input$row_selected
            if (length(new_selections) < 1) {
              new_selections <- seq_along(data)
            }

            data_inputs <- as.data.table(input$table_data)
            setorderv(data_inputs, "rowKey")

            old_names <- data_inputs$name
            new_names <- data_inputs$name_toset
            new_names[new_names == "Enter new name"] <- NA
            new_names[is.na(new_names)] <- old_names[is.na(new_names)]
            new_names[new_names == ""] <- old_names[new_names == ""]


            new_classes <- data_inputs$class_toset
            new_classes[new_classes == "Select new class"] <- NA

            data_sv <- variables_r()
            vars_to_change <- get_vars_to_convert(data_sv, setNames(as.list(new_classes), old_names))

            res_update <- try(
              {
                # convert
                if (nrow(vars_to_change) > 0) {
                  data <- convert_to(
                    data = data,
                    variable = vars_to_change$name,
                    new_class = vars_to_change$class_to_set,
                    origin = input$origin,
                    format = input$format,
                    dec = input$dec
                  )
                }
                list_mutate <- attr(data, "code_03_convert")

                # rename
                list_rename <- setNames(
                  as.list(old_names),
                  unlist(new_names, use.names = FALSE)
                )
                list_rename <- list_rename[names(list_rename) != unlist(list_rename, use.names = FALSE)]
                names(data) <- unlist(new_names, use.names = FALSE)

                # select
                list_select <- setdiff(names(data), names(data)[new_selections])
                data <- data[, new_selections, drop = FALSE]
              },
              silent = FALSE
            )

            if (inherits(res_update, "try-error")) {
              insert_error(selector = "update")
            } else {
              insert_alert(
                selector = ns("update"),
                status = "success",
                tags$b(phosphoricons::ph("check"), i18n$t("Data successfully updated!"))
              )
              updated_data$x <- data
              updated_data$list_rename <- list_rename
              updated_data$list_select <- list_select
              updated_data$list_mutate <- list_mutate
            }
          }
          rv_ignit(1)
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      return(reactive({
        data <- updated_data$x
        code <- list()
        if (!is.null(data) && isTruthy(updated_data$list_mutate) && length(updated_data$list_mutate) > 0) {
          code <- c(code, list(call2("mutate", !!!updated_data$list_mutate)))
        }
        if (!is.null(data) && isTruthy(updated_data$list_rename) && length(updated_data$list_rename) > 0) {
          code <- c(code, list(call2("rename", !!!updated_data$list_rename)))
        }
        if (!is.null(data) && isTruthy(updated_data$list_select) && length(updated_data$list_select) > 0) {
          code <- c(code, list(expr(select(-any_of(c(!!!updated_data$list_select))))))
        }
        if (length(code) > 0) {
          attr(data, "code") <- Reduce(
            f = function(x, y) expr(!!x %>% !!y),
            x = code
          )
        }
        return(data)
      }))
    }
  )
}

# utils -------------------------------------------------------------------


#' Get variables classes from a \code{data.frame}
#'
#' @param data a \code{data.frame}
#'
#' @return a \code{character} vector as same length as number of variables
#' @noRd
#'
get_classes <- function(data) {
  classes <- lapply(
    X = data,
    FUN = function(x) {
      paste(class(x), collapse = ", ")
    }
  )
  unlist(classes, use.names = FALSE)
}


#' Get count of unique values in variables of \code{data.frame}
#'
#' @param data a \code{data.frame}
#'
#' @return a \code{numeric} vector as same length as number of variables
#' @noRd
#'
#' @importFrom data.table uniqueN
#'
get_n_unique <- function(data) {
  u <- lapply(data, FUN = function(x) {
    if (is.atomic(x)) {
      uniqueN(x)
    } else {
      NA_integer_
    }
  })
  unlist(u, use.names = FALSE)
}



#' Add padding 0 to a vector
#'
#' @param x a \code{vector}
#'
#' @return a \code{character} vector
#' @noRd
#'
pad0 <- function(x) {
  NAs <- which(is.na(x))
  x <- formatC(x, width = max(nchar(as.character(x)), na.rm = TRUE), flag = "0")
  x[NAs] <- NA
  x
}



#' Variables summary
#'
#' @param data a \code{data.frame}
#'
#' @return a \code{data.frame}
#' @noRd
summary_vars <- function(data) {
  data <- as.data.frame(data)
  datsum <- data.frame(
    name = names(data),
    class = get_classes(data),
    n_missing = unname(colSums(is.na(data))),
    stringsAsFactors = FALSE
  )
  datsum$p_complete <- 1 - datsum$n_missing / nrow(data)
  datsum$n_unique <- get_n_unique(data)
  datsum
}



add_var_toset <- function(data, var_name, default = "") {
  datanames <- names(data)
  datanames <- append(
    x = datanames,
    values = paste0(var_name, "_toset"),
    after = which(datanames == var_name)
  )
  data[[paste0(var_name, "_toset")]] <- default
  data[, datanames]
}

#' @importFrom toastui datagrid grid_columns grid_format grid_style_column
#'  grid_style_column grid_editor grid_editor_opts grid_selection_row
update_variables_datagrid <- function(data, height = NULL, selectionId = NULL, buttonId = NULL) {
  data <- add_var_toset(data, "name", "Enter new name")
  data <- add_var_toset(data, "class", "Select new class")

  gridTheme <- getOption("datagrid.theme")
  if (length(gridTheme) < 1) {
    apply_grid_theme()
  }
  on.exit(toastui::reset_grid_theme())

  grid <- datagrid(
    data = data,
    theme = "default",
    colwidths = NULL
  )
  grid <- grid_columns(
    grid = grid,
    columns = c("name", "name_toset", "class", "class_toset", "n_missing", "p_complete", "n_unique"),
    header = c("Name", "New name", "Class", "New class", "Missing values", "Complete obs.", "Unique values"),
    minWidth = 120
  )
  grid <- grid_format(
    grid = grid,
    "p_complete",
    formatter = toastui::JS("function(obj) {return (obj.value*100).toFixed(0) + '%';}")
  )
  grid <- grid_style_column(
    grid = grid,
    column = "name_toset",
    fontStyle = "italic"
  )
  grid <- grid_style_column(
    grid = grid,
    column = "class_toset",
    fontStyle = "italic"
  )
  grid <- grid_editor(
    grid = grid,
    column = "name_toset",
    type = "text"
  )
  grid <- grid_editor(
    grid = grid,
    column = "class_toset",
    type = "select",
    choices = c("Select new class", "character", "factor", "numeric", "integer", "date", "datetime")
  )
  grid <- grid_editor_opts(
    grid = grid,
    editingEvent = "click",
    actionButtonId = buttonId
  )
  grid <- grid_selection_row(
    grid = grid,
    inputId = selectionId,
    type = "checkbox",
    return = "index"
  )
  return(grid)
}




#' Convert a variable to specific new class
#'
#' @param data A \code{data.frame}
#' @param variable Name of the variable to convert
#' @param new_class Class to set
#' @param ... Other arguments passed on to methods.
#'
#' @return A \code{data.frame}
#' @noRd
#'
#' @importFrom utils type.convert
#' @importFrom rlang sym expr
#'
convert_to <- function(data,
                       variable,
                       new_class = c("character", "factor", "numeric", "integer", "date", "datetime"),
                       ...) {
  new_class <- match.arg(new_class, several.ok = TRUE)
  stopifnot(length(new_class) == length(variable))
  args <- list(...)
  if (length(variable) > 1) {
    for (i in seq_along(variable)) {
      data <- convert_to(data, variable[i], new_class[i], ...)
    }
    return(data)
  }
  if (identical(new_class, "character")) {
    data[[variable]] <- as.character(x = data[[variable]], ...)
    attr(data, "code_03_convert") <- c(
      attr(data, "code_03_convert"),
      setNames(list(expr(as.character(!!sym(variable)))), variable)
    )
  } else if (identical(new_class, "factor")) {
    data[[variable]] <- as.factor(x = data[[variable]])
    attr(data, "code_03_convert") <- c(
      attr(data, "code_03_convert"),
      setNames(list(expr(as.factor(!!sym(variable)))), variable)
    )
  } else if (identical(new_class, "numeric")) {
    data[[variable]] <- as.numeric(type.convert(data[[variable]], as.is = TRUE, ...))
    attr(data, "code_03_convert") <- c(
      attr(data, "code_03_convert"),
      setNames(list(expr(as.numeric(!!sym(variable)))), variable)
    )
  } else if (identical(new_class, "integer")) {
    data[[variable]] <- as.integer(x = data[[variable]], ...)
    attr(data, "code_03_convert") <- c(
      attr(data, "code_03_convert"),
      setNames(list(expr(as.integer(!!sym(variable)))), variable)
    )
  } else if (identical(new_class, "date")) {
    data[[variable]] <- as.Date(x = data[[variable]], ...)
    attr(data, "code_03_convert") <- c(
      attr(data, "code_03_convert"),
      setNames(list(expr(as.Date(!!sym(variable), origin = !!args$origin))), variable)
    )
  } else if (identical(new_class, "datetime")) {
    data[[variable]] <- as.POSIXct(x = data[[variable]], ...)
    attr(data, "code_03_convert") <- c(
      attr(data, "code_03_convert"),
      setNames(list(expr(as.POSIXct(!!sym(variable)))), variable)
    )
  }
  return(data)
}

#' Get variable(s) to convert
#'
#' @param vars Output of [summary_vars()]
#' @param classes_input List of inputs containing new classes
#'
#' @return a `data.table`.
#' @noRd
#'
#' @importFrom data.table data.table as.data.table

get_vars_to_convert <- function(vars, classes_input) {
  vars <- as.data.table(vars)
  classes_input <- data.table(
    name = names(classes_input),
    class_to_set = unlist(classes_input, use.names = FALSE),
    stringsAsFactors = FALSE
  )
  classes_input <- classes_input[!is.na(class_to_set) & class_to_set != ""]
  classes_df <- merge(x = vars, y = classes_input, by = "name")
  classes_df <- classes_df[!is.na(class_to_set)]
  classes_df[class != class_to_set]
}
