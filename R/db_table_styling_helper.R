#' preapre_db_table_view
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

convert_date_columns <- function(db_table, date_columns) {
  db_table %>% mutate(across(all_of(date_columns), ~ case_when(
    !is.na(as.numeric(.)) ~ as.character(as.POSIXct(as.numeric(.), origin = "1970-01-01", tz = "UTC")),
    TRUE ~ paste(.)
  )))
}

#' @export
have_permission_to_see <- function(permission_table = "th2_ml_permissions", target_object = "test_table") {
  permissions_db <- grant_ml_permission_db(
    target_table = permission_table,
    meta = list(
      OBJECT_ID = target_object,
      current_user = Sys.getenv("SHINYPROXY_USERNAME")
    ),
    permission_action = "get"
  )

  # Vérifiez que permissions_db n'est pas NULL et contient le champ PERMISSION_LEVEL
  if (is.null(permissions_db) || is.null(permissions_db$PERMISSION_LEVEL)) {
    return(FALSE)
  }

  # S'assurer que PERMISSION_LEVEL est un vecteur de longueur 1 pour éviter des erreurs avec ||
  if (length(permissions_db$PERMISSION_LEVEL) != 1) {
    warning("PERMISSION_LEVEL contains more than one value, which is not expected.")
    return(FALSE)
  }

  # Utilisez || pour une évaluation de court-circuit sur une valeur scalaire
  if (permissions_db$PERMISSION_LEVEL == "None") {
    return(FALSE)
  } else {
    return(TRUE)
  }
}


#' @export
have_permission_to_edit <- function(permission_table = "th2_ml_permissions", target_object = "test_table") {
  permissions_db <- grant_ml_permission_db(
    target_table = permission_table,
    meta = list(
      OBJECT_ID = target_object,
      current_user = Sys.getenv("SHINYPROXY_USERNAME")
    ),
    permission_action = "get"
  )

  if (is.null(permissions_db) || permissions_db$PERMISSION_LEVEL %in% c("View", "None")) {
    return(F)
  } else {
    return(T)
  }
}

#' @export
have_permission_to_manage <- function(permission_table = "th2_ml_permissions", target_object = "test_table") {
  permissions_db <- grant_ml_permission_db(
    target_table = permission_table,
    meta = list(
      OBJECT_ID = target_object,
      current_user = Sys.getenv("SHINYPROXY_USERNAME")
    ),
    permission_action = "get"
  )


  if (is.null(permissions_db) || !permissions_db$PERMISSION_LEVEL %in% c("Owner", "Manage")) {
    return(F)
  } else {
    return(T)
  }
}
