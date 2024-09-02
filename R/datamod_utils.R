#' Null-coalescing operator
#'
#' Provides a null-coalescing operation, returning the right-hand side value if the left-hand side is NULL.
#'
#' @param x The left-hand side value.
#' @param y The right-hand side value, used if `x` is NULL.
#' @return Either `x` if it's not NULL, or `y` otherwise.
`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' Drop NULL elements from a list or vector
#'
#' Removes NULL elements from the input list or vector.
#'
#' @param x The list or vector to process.
#' @return The input with NULL elements removed.
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

#' Drop NULL or empty elements
#'
#' Removes NULL or empty (zero-length or empty string) elements from the input list or vector
#'
#' @param x The list or vector to process
#' @return The input with NULL or empty elements removed.
dropNullsOrEmpty <- function(x) {
  x[!vapply(x, nullOrEmpty, FUN.VALUE = logical(1))]
}

#' Check for NULL or empty elements
#'
#' Checks if an element is NULL, has zero length, or is an empty string
#'
#' @param x The element to check
#' @return TRUE if the element is NULL, has zero length, or is an empty string, FALSE otherwise.
nullOrEmpty <- function(x) {
  is.null(x) || length(x) == 0 || x == ""
}

#' Drop list columns from a data.frame or data.table
#'
#' Removes columns of type 'list' from the input data.frame or data.table.
#'
#' @param x The data.frame or data.table to process
#' @return The input with list columns removed.
#' @importFrom data.table .SD
dropListColumns <- function(x) {
  type_col <- vapply(
    X = x,
    FUN = typeof,
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
  if (inherits(x, "data.table")) {
    x[, .SD, .SDcols = type_col != "list"]
  } else {
    x[, type_col != "list", drop = FALSE]
  }
}


#' Search for object with specific class in an environment
#'
#' @param what a class to look for
#' @param env An environment
#'
#' @return Character vector of the names of objects, NULL if none
search_obj <- function(what = "data.frame", env = globalenv()) {
  all <- ls(name = env)
  objs <- lapply(
    X = all,
    FUN = function(x) {
      if (inherits(get(x, envir = env), what = what)) {
        x
      } else {
        NULL
      }
    }
  )
  objs <- unlist(objs)
  if (length(objs) == 1 && objs == "") {
    NULL
  } else {
    objs
  }
}

#' Convert to specified output class
#'
#' Converts the input to a specified class, handling NULL inputs and preserving 'sf' class if present.
#'
#' @param x The input to convert
#' @param return_class The desired output class ("data.frame", "data.table", "tbl_df", or "raw")
#' @return The converted input, or NULL if the input was NULL
#' @importFrom data.table as.data.table
#' @importFrom tibble as_tibble
as_out <- function(x, return_class = c("data.frame", "data.table", "tbl_df", "raw")) {
  if (is.null(x)) {
    return(NULL)
  }
  return_class <- match.arg(return_class)
  if (identical(return_class, "raw")) {
    return(x)
  }
  is_sf <- inherits(x, "sf")
  x <- if (identical(return_class, "data.frame")) {
    as.data.frame(x)
  } else if (identical(return_class, "data.table")) {
    as.data.table(x)
  } else {
    as_tibble(x)
  }
  if (is_sf) {
    class(x) <- c("sf", class(x))
  }
  return(x)
}

#' Generate a random ID
#'
#' Generates a random ID string of specified length using hexadecimal characters
#'
#' @param bytes The number of bytes to use for generating the ID (default: 12)
#' @return A random ID string
genId <- function(bytes = 12) {
  paste(format(as.hexmode(sample(256, bytes, replace = TRUE) - 1), width = 2), collapse = "")
}

#' Create unique IDs from a vector
#'
#' Converts a vector of values to unique IDs, handling empty inputs and ensuring uniqueness
#'
#' @param x The vector of values to convert to IDs
#' @return A vector of unique IDs, or NULL if the input was empty
makeId <- function(x) {
  if (length(x) < 1) {
    return(NULL)
  }
  x <- as.character(x)
  x <- lapply(X = x, FUN = function(y) {
    paste(as.character(charToRaw(y)), collapse = "")
  })
  x <- unlist(x, use.names = FALSE)
  make.unique(x, sep = "_")
}

#' Check for membership in a table, allowing for NULL or empty tables
#'
#' This infix operator checks if elements in 'x' are present in 'table'.
#' If 'table' is NULL or empty, it always returns TRUE.
#'
#' @param x The values to check for membership
#' @param table The table to check against
#' @return A logical vector indicating membership, with TRUE if 'table' is NULL or empty
`%inT%` <- function(x, table) {
  if (!is.null(table) && !"" %in% table) {
    x %in% table
  } else {
    rep_len(TRUE, length(x))
  }
}


#' Check for non-membership in a table, allowing for NULL or empty tables
#'
#' This infix operator checks if elements in 'x' are NOT present in 'table'
#' If table is NULL or empty, it always returns FALSE
#'
#' @param x The values to check for non-membership
#' @param table The table to check against
#' @return A logical vector indicating non-membership, with FALSE if 'table' is NULL or empty
`%inF%` <- function(x, table) {
  if (!is.null(table) && !"" %in% table) {
    x %in% table
  } else {
    rep_len(FALSE, length(x))
  }
}

#' Generate header with class information
#'
#' Creates a header element with class information for a given value in a dataset
#'
#' @param data The dataset containing the value
#' @param value The value to generate the header for
#' @return An HTML tag list containing the header with class information, or an empty string if the value is not found
#' @importFrom utils hasName
header_with_classes <- function(data) {
  function(value) {
    if (!hasName(data, value)) {
      return("")
    }
    classes <- tags$div(
      style = "font-style: italic; font-weight: normal; font-size: small;",
      get_classes(data[, value, drop = FALSE])
    )
    tags$div(title = value, value, classes)
  }
}

#' Split a string into a vector
#'
#' Splits a string into a vector using a specified delimiter, handling NULL inputs
#'
#' @param x The string to split
#' @param split The delimiter to use for splitting (default: ",")
#' @return A vector of split strings, or NULL if the input was NULL
split_char <- function(x, split = ",") {
  if (is.null(x)) {
    return(NULL)
  }
  unlist(strsplit(x, split = split))
}


#' Apply a grid theme
#'
#' Sets a specific theme for grid visualization, defining colors and borders for cells and areas.
apply_grid_theme <- function() {
  toastui::set_grid_theme(
    cell.normal.background = "#FFF",
    cell.normal.border = "#D8DEE9",
    cell.normal.showVerticalBorder = TRUE,
    cell.normal.showHorizontalBorder = TRUE,
    cell.header.border = "#D8DEE9",
    area.header.border = "#4C566A",
    cell.summary.border = "#D8DEE9",
    cell.summary.showVerticalBorder = TRUE,
    cell.summary.showHorizontalBorder = TRUE
  )
}
