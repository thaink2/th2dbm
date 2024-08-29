#' Prepare Database Table for Display
#'
#' This function prepares a database table for visualization in a Shiny application. It handles specific formatting and transformations based on the table and column types.
#'
#' @param use_db_table The data.frame or data.table containing the database table data.
#' @param target_table The name of the target table.
#' @param secret_tables A vector of table names considered secret, for which the 'COL_ID' column is removed.
#' @param styling_var The column name used for conditional formatting (default: "STATUS").
#' @param current_permission_table (Optional) A vector of table names with specific permissions.
#'
#' @return A DT datatable object ready for display, with potential formatting applied.
#' @export
prepare_db_table_view <- function(use_db_table = NULL, target_table = NULL, secret_tables = c("secret_table"), styling_var = "STATUS", current_permission_table = NULL) {
  vars_table_metadata <- vars_table_metadata(current_target_table = target_table)
  if (target_table %in% secret_tables) {
    db_table <- use_db_table %>%
      dplyr::select(-COL_ID)
  } else {
    db_table <- use_db_table %>%
      dplyr::select(-COL_ID)
  }

  # Identification des colonnes de type 'date_time' basée sur 'current_var'
  current_var_date <- vars_table_metadata %>%
    dplyr::filter(TH2DB_TABLE == target_table, VAR_TYPE == "date_time")

  # Appliquer la conversion de date conditionnelle sur les colonnes identifiées
  if (nrow(current_var_date) > 0) {
    db_table <- convert_date_columns(db_table, current_var_date$VAR_ID)
  }

  # Créer l'objet DT
  if (target_table == "data_connection_params") {
    db_table <- db_table[, c("TABLE_ID", "DATA_SOURCE", "OWNER", "CREATED_AT", "UPDATED_AT", "PARAM1", "PARAM2", "PARAM3", "PARAM4", "PARAM5", "PARAM6", "PARAM7", "PARAM8")]
    # ================== Décryptage des données cryptés ==============================
    #
    cols_to_decrypt <- names(db_table)[grepl("^PARAM", names(db_table))]

    db_table[cols_to_decrypt] <- lapply(db_table[cols_to_decrypt], decrypt_column)
  } else if (target_table %in% current_permission_table) {
    db_table <- db_table %>%
      arrange(OBJECT_ID)
  }


  # Appliquer le formatage conditionnel si la colonne 'status' existe
  dt_table_dt <- DT::datatable(db_table, selection = "single", options = list(
    scrollX = TRUE
  ))
  if ("STATUS" %in% colnames(db_table)) {
    dt_table_dt <- dt_table_dt %>%
      DT::formatStyle(
        "STATUS",
        color = DT::styleEqual(c("failed", "success"), c("white", "white")),
        backgroundColor = DT::styleEqual(c("failed", "success"), c("orange", "green"))
      )
  }

  if ("ACTION" %in% colnames(db_table)) {
    dt_table_dt <- dt_table_dt %>%
      DT::formatStyle(
        "ACTION",
        color = DT::styleEqual(c("failed", "success"), c("white", "white")),
        backgroundColor = DT::styleEqual(c("To Do", "In Progress", "Resolved"), c("purple", "lightblue", "blue"))
      )
  }
  return(dt_table_dt)
}

#' Convert Date Columns
#'
#' This helper function converts specific columns within a data.frame or data.table from numeric timestamps to formatted date-time strings.
#'
#' @param db_table The data.frame or data.table containing the data.
#' @param date_columns A vector of column names to convert to date-time format.
#'
#' @return The modified data.frame or data.table with converted date columns.
convert_date_columns <- function(db_table, date_columns) {
  db_table %>% mutate(across(all_of(date_columns), ~ case_when(
    !is.na(as.numeric(.)) ~ as.character(as.POSIXct(as.numeric(.), origin = "1970-01-01", tz = "UTC")),
    TRUE ~ paste(.)
  )))
}


