#' Initialiser la base de données
#'
#' Initialise la base de données en vérifiant l'existence de la table cible et
#' en la créant si elle n'existe pas. Optionnellement, initie le produit en
#' appelant un script d'initialisation.
#'
#' @param target_table Nom de la table cible à initialiser ou à vérifier.
#'                     Par défaut, "th2metadata_table".
#' @param working_mode Le mode de travail qui peut influencer l'initialisation
#'                     du produit. Par défaut, "Dev".
#' @import dplyr
#' @export
initialize_database <- function(target_table = "th2metadata_table",
                                working_mode = "Dev", file_path = "./R/initializer.R", cluster = "aws") {
  aws_db_con <- connect_to_database()
  # Établir connexion à la base de données
  db_con <- connect_to_database()

  # Copier les données de la table th2metadata_table de la base de données postgres vers rdb
  if (cluster != "aws") {
    copy_table(db_source = aws_db_con, db_target = db_con, target_table = "th2metadata_table")

    reserved_words <- c("TABLE", "SELECT", "INSERT", "UPDATE", "DELETE", "CREATE", "DROP", "ALTER", "WHERE", "FROM", "JOIN", "ON", "AND", "OR", "NOT", "NULL", "INDEX", "UNIQUE", "PRIMARY", "KEY")

    # Fonction pour vérifier et échapper les noms de colonnes
    escape_reserved_words <- function(name) {
      if (toupper(name) %in% reserved_words) {
        return(paste0('"', name, '"'))
      } else {
        return(name)
      }
    }
    # Fonction pour obtenir le type SQL correspondant
    get_sql_type <- function(var_type) {
      switch(var_type,
        "text" = "TEXT",
        "current_user" = "TEXT", # Ajustez selon les besoins
        "uuid" = "UUID",
        "date_time" = "TIMESTAMP",
        "choices" = "TEXT", # Ajustez selon les besoins
        "numeric" = "NUMERIC",
        "CHOOSE_FROM_TABLE_var" = "TEXT", # Ajustez selon les besoins
        "TEXT"
      )
    }

    # Fonction pour créer la table avec sa structure uniquement
    create_table_structure <- function(con, table_name, columns) {
      # Vérifier si la table existe déjà

      columns_sql <- paste(sapply(seq_len(nrow(columns)), function(i) {
        col <- columns[i, ]
        column_name <- escape_reserved_words(col[["VAR_ID"]])
        column_type <- get_sql_type(col[["VAR_TYPE"]])
        paste0(column_name, " ", column_type)
      }), collapse = ", ")

      create_table_query <- paste0("CREATE TABLE ", table_name, " (", columns_sql, ")")
      print(create_table_query)
      DBI::dbExecute(con, create_table_query)
    }
    th2metadata_table <- vars_table_metadata(db_con = aws_db_con, current_target_table = "th2metadata_table")

    # Créer les tables dans la base de données cible
    unique_tables <- unique(th2metadata_table$TH2DB_TABLE)
    for (table in unique_tables) {
      columns <- th2metadata_table[th2metadata_table$TH2DB_TABLE == table, ]
      table_exists <- DBI::dbExistsTable(db_con, table)

      if (!table_exists) {
        create_table_structure(db_con, table, columns)
      }
    }
    DBI::dbDisconnect(conn = aws_db_con)
  }

  # Liste des tables disponibles
  available_tables <- DBI::dbListTables(conn = db_con)

  # Création de la table si non existante
  if (!(target_table %in% available_tables)) {
    if (target_table == "th2metadata_table") {
      table_fields <- data.frame(
        TH2DB_TABLE = "tab_name",
        VAR_ID = "test_var",
        VAR_TYPE = "BOOLEAN",
        VAR_UNIQUE = "",
        COLUMN_ID = "",
        COLUMN_CHOICES = "",
        CHOOSE_FROM_TABLE = "",
        CHOOSE_FROM_VAR = ""
      )
    }


    # Créer la table avec les champs définis
    DBI::dbCreateTable(
      conn = db_con,
      name = target_table,
      fields = table_fields
    )
  }
  DBI::dbDisconnect(conn = db_con)
}

#' @export

copy_table <- function(db_source, db_target, target_table) {
  # Lire les données de la table source
  data <- DBI::dbReadTable(db_source, target_table)

  # Vérifier si la table existe déjà dans la base de données cible
  table_exists <- DBI::dbExistsTable(db_target, target_table)

  if (table_exists) {
    # Lire les données existantes dans la table cible
    target_data <- DBI::dbReadTable(db_target, target_table)

    # Vérifier que les colonnes correspondent
    if (!all(names(data) %in% names(target_data)) || !all(names(target_data) %in% names(data))) {
      # Supprimer la table cible si les colonnes ne correspondent pas
      DBI::dbRemoveTable(db_target, target_table)
      table_exists <- FALSE
    }
  }

  if (!table_exists) {
    # Créer la table dans la base de données cible avec la même structure que la source
    DBI::dbCreateTable(db_target, target_table, data)
  }

  # Insérer les données dans la table cible
  DBI::dbWriteTable(db_target, target_table, data, append = TRUE)
}

# ==============================================================================
#' Installer les pilotes de base de données
#'
#' Télécharge et installe les pilotes JDBC nécessaires pour se connecter à
#' divers types de bases de données.
#'
#' @param jdbcs_drivers_path Chemin du dossier où les pilotes JDBC seront
#' téléchargés et stockés. Si NULL, un chemin par défaut est utilisé en fonction
#' du système d'exploitation.
#' @param db_type Le type de base de données pour lequel télécharger les
#' pilotes. Par défaut, "all", signifiant tous les types pris en charge.
#' @export
th2_install_db_drivers <- function(jdbcs_drivers_path = NULL, db_type = "all") {
  # Définir le chemin par défaut en fonction du système d'exploitation
  if (is.null(jdbcs_drivers_path)) {
    jdbcs_drivers_path <- if (Sys.info()["sysname"] != "Windows") {
      "/home/automl/jdbcDrivers/"
    } else {
      "c:/temp/jdbcDrivers"
    }
  }

  # Télécharger les pilotes JDBC si le répertoire n'existe pas
  if (!dir.exists(jdbcs_drivers_path)) {
    DatabaseConnector::downloadJdbcDrivers(
      dbms = db_type,
      pathToDriver = jdbcs_drivers_path
    )
  }

  # Configurer l'environnement pour utiliser les pilotes téléchargés
  Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = jdbcs_drivers_path)

  # Retourner le chemin des pilotes JDBC
  return(jdbcs_drivers_path)
}

# ==============================================================================
#' Se connecter à la base de données
#'
#' Établit une connexion à une base de données spécifiée, soit SQLite soit
#' une base de données gérée via JDBC.
#'
#' @param db_type Type de la base de données à connecter, utilise une variable
#'                d'environnement par défaut pour déterminer le type.
#' @param db_params Liste des paramètres de connexion à la base de données,
#'                  incluant l'hôte, le nom de la base de données, le mot de
#'                  passe, etc.
#' @param db_file_name Nom de fichier pour les connexions à la base de données
#'                     SQLite, utilise une variable d'environnement par défaut.
#' @export
connect_to_database <- function(
    db_type = Sys.getenv("CURRENT_DB"),
    db_params = list(
      host = Sys.getenv("TH_HOST"),
      database = Sys.getenv("TH_DATABASE"),
      password = Sys.getenv("TH_DB_PASSWORD"),
      username = Sys.getenv("TH_DB_USERNAME"),
      port = Sys.getenv("TH_PORT")
    ),
    db_file_name = Sys.getenv("CURRENT_DB_DIR"),
    extended_types = FALSE,
    path_driver = NULL) {
  # Gestion des connexions SQLite
  if (db_type == "sqlite") {
    # Ajustement du chemin du fichier SQLite selon le système d'exploitation
    db_file_name <- ifelse(
      Sys.info()["sysname"] == "Windows",
      db_file_name,
      "/home/automl/th2_test.sqlite"
    )
    db_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_file_name, extended_types = extended_types)
  } else {
    print(paste("Host:", db_params$host))
    # Construction de la chaîne de connexion pour les bases de données via JDBC
    path_to_drive <- th2_install_db_drivers(jdbcs_drivers_path = path_driver, db_type = db_type)
    data_source_server <- paste0(db_params$host, "/", db_params$database)
    db_con <- DatabaseConnector::connect(
      dbms = db_type,
      server = data_source_server,
      user = db_params$username,
      password = db_params$password,
      port = as.numeric(db_params$port),
      pathToDriver = path_to_drive
    )
  }
  return(db_con)
}

# ==============================================================================
#' Créer ou Mettre à Jour la Métadonnée
#'
#' Cette fonction permet de créer ou de mettre à jour les métadonnées dans une table spécifiée.
#'
#' @param table_metadata Data frame contenant les métadonnées à ajouter ou à mettre à jour.
#' @param action_table Action à effectuer, "create" pour créer ou "update" pour mettre à jour.
#' @export
create_update_metadata <- function(table_metadata = NULL, action_table = "create") {
  # Établir une connexion à la base de données
  db_con <- connect_to_database()

  # Normalisation des noms de colonnes en majuscules
  colnames(table_metadata) <- toupper(colnames(table_metadata))

  # Extraction du nom de la table cible depuis les métadonnées
  target_table <- table_metadata %>%
    dplyr::pull(TH2DB_TABLE) %>%
    unique()

  # Préparation des champs de données pour la création de table
  data_fields <- table_metadata %>%
    dplyr::mutate(id = "id", value = "NA") %>%
    dplyr::select(VAR_ID, id, value) %>%
    tidyr::pivot_wider(names_from = "VAR_ID", values_from = "value")

  # Vérification de l'existence de la table 'th2metadata_table'
  available_tables <- DBI::dbListTables(conn = db_con)
  if (!"th2metadata_table" %in% available_tables) {
    return(NULL)
  }
  # Gestion des actions "create" ou "update"
  if (action_table != "create") {
    # Pour "update", suppression des entrées existantes avant la mise à jour
    query_statement <- glue::glue(
      "DELETE FROM th2metadata_table WHERE TH2DB_TABLE = '{target_table}'"
    )
    DBI::dbExecute(db_con, statement = query_statement)
  } else {
    # Pour "create", création de la nouvelle table avec les champs spécifiés
    DBI::dbCreateTable(conn = db_con, name = target_table, fields = data_fields)
  }
  # Insertion des nouvelles métadonnées
  DBI::dbWriteTable(
    conn = db_con, name = "th2metadata_table",
    value = table_metadata, append = TRUE
  )
  # Déconnexion de la base de données
  DatabaseConnector::disconnect(connection = db_con)
  if (exists("db_con")) rm(db_con)
  return("metadata updated")
}

# ==============================================================================
#' Récupérer les Données d'une Table
#'
#' Récupère les données d'une table spécifique à partir de la base de données.
#'
#' @param db_con Connexion active à la base de données.
#' @param table_name Nom de la table à partir de laquelle récupérer les données.
#' @export
retrieve_table_data <- function(db_con = connect_to_database(), table_name = NULL) {
  db_con <- connect_to_database()
  # Vérification que la connexion et le nom de la table ne sont pas NULL
  if (is.null(db_con) || is.null(table_name)) {
    stop("Database connection and table name must not be NULL")
  }

  # Vérification de l'existence de la table dans la base de données
  tables <- DBI::dbListTables(db_con)
  if (!(table_name %in% tables)) {
    stop("Table does not exist in the database")
  }

  # Lecture et retour des données de la table spécifiée
  data <- DBI::dbReadTable(conn = db_con, name = table_name)

  # Déconnexion de la base de données
  DBI::dbDisconnect(conn = db_con)

  return(data)
}

# ==============================================================================
#' Métadonnées de la Table des Variables
#'
#' Récupère les métadonnées pour une table de variables spécifique.
#'
#' @param current_target_table Nom de la table cible pour laquelle récupérer les métadonnées.
#' @export
vars_table_metadata <- function(db_con = connect_to_database(), current_target_table = "test_table") {
  # Vérification de l'existence de la table 'th2metadata_table'
  available_tables <- DBI::dbListTables(conn = db_con)
  if (!"th2metadata_table" %in% available_tables) {
    DBI::dbDisconnect(db_con)
    stop("La table 'th2metadata_table' n'existe pas dans la base de données.")
  }

  # Construction et exécution de la requête SQL pour récupérer les métadonnées
  if (current_target_table == "th2metadata_table") {
    query_statement <- glue::glue(
      "SELECT * FROM th2metadata_table"
    )
  } else {
    query_statement <- glue::glue(
      "SELECT * FROM th2metadata_table WHERE TH2DB_TABLE = '{current_target_table}'"
    )
  }
  query_res <- DBI::dbSendQuery(db_con, statement = query_statement)

  # Récupération des résultats de la requête
  th2metadata_table <- DBI::dbFetch(query_res)

  # Normalisation des noms de colonnes en majuscules
  colnames(th2metadata_table) <- toupper(colnames(th2metadata_table))

  # Nettoyage et fermeture de la connexion
  DBI::dbClearResult(query_res)
  DBI::dbDisconnect(db_con)

  return(th2metadata_table)
}

#' Table des permissions
#'
#' Récupère les tables de permission
#'
#' @export
current_permission_table <- function() {
  db_con <- connect_to_database()
  available_tables <- DBI::dbListTables(conn = db_con)

  if (!"th2metadata_table" %in% available_tables) {
    return(NULL)
  }
  query_statement <- glue::glue("SELECT TH2DB_TABLE FROM th2metadata_table")

  query_res <- DBI::dbSendQuery(db_con, statement = query_statement)
  th2metadata_table <- DBI::dbFetch(query_res)
  colnames(th2metadata_table) <- toupper(colnames(th2metadata_table))
  perm_table <- th2metadata_table %>%
    filter(grepl("permissions", TH2DB_TABLE)) %>%
    pull(TH2DB_TABLE) %>%
    unique()

  return(perm_table)
}


# ==============================================================================
#' Ajouter une Entrée à une Table
#'
#' Ajoute une nouvelle entrée à une table spécifique dans la base de données.
#'
#' @param new_entry Data frame contenant les nouvelles entrées à ajouter.
#' @param target_table Nom de la table cible où ajouter les nouvelles entrées.
#' @export
add_entry_to_table <- function(new_entry = NULL, target_table = NULL) {
  # Vérification que les paramètres ne sont pas NULL
  if (is.null(new_entry) || is.null(target_table)) {
    return(list(message = "Les paramètres 'new_entry' et 'target_table' ne doivent pas être NULL.", status = "error"))
  }

  # Ajout d'un ID unique si nécessaire
  if (!"COL_ID" %in% colnames(new_entry) || is.na(new_entry$COL_ID[1])) {
    new_entry <- new_entry %>%
      dplyr::mutate(COL_ID = generateID(prefix = target_table))
  }

  # Connexion à la base de données
  db_con <- connect_to_database()

  # Vérification de l'existence de la table cible
  available_tables <- DBI::dbListTables(conn = db_con)
  if (!target_table %in% available_tables) {
    DBI::dbDisconnect(db_con)
    return(list(message = "La table {target_table} n'existe pas dans la base de données.", status = "error"))
  }

  query_statement <- glue::glue("SELECT VAR_ID FROM th2metadata_table WHERE TH2DB_TABLE = '{target_table}' AND VAR_UNIQUE = 'true' ")

  query_res <- DBI::dbSendQuery(db_con, statement = query_statement)
  primary_keys <- DBI::dbFetch(query_res)

  existing_entries <- DBI::dbReadTable(conn = db_con, name = target_table)

  if (nrow(primary_keys) > 0) {
    values <- lapply(primary_keys$var_id, function(var_id) {
      print(var_id)
      print(var_id != "COL_ID" & any(new_entry[[var_id]] %in% existing_entries[[var_id]]))
      if (var_id != "COL_ID" & any(new_entry[[var_id]] %in% existing_entries[[var_id]])) {
        new_entry[[var_id]]
      }
    })
    values <- Filter(Negate(is.null), values)

    if (length(values) > 0) {
      # DBI::dbDisconnect(db_con)
      return(list(message = glue::glue("{paste(values, collapse = ', ')} existe déjà."), status = "error"))
    }
  }
  # Ajout de la nouvelle entrée à la table
  DBI::dbWriteTable(db_con, name = target_table, value = new_entry, append = TRUE)

  # Fermeture de la connexion
  DBI::dbDisconnect(db_con)
  return(list(message = "Nouvelle entrée ajoutée avec succès.", status = "success"))
}


# ==============================================================================
#' Mettre à Jour une Entrée dans une Table
#'
#' Met à jour une entrée spécifique dans une table de la base de données.
#'
#' @param updated_entry Data frame contenant les données mises à jour.
#' @param target_table Nom de la table où effectuer la mise à jour.
#' @param unique_id_col Nom de la colonne contenant l'identifiant unique.
#' @param unique_id_val Valeur de l'identifiant unique de l'entrée à mettre à jour.
#' @export
update_entry_in_table <- function(updated_entry = NULL, target_table = NULL,
                                  unique_id_col = NULL, unique_id_val = NULL) {
  # Vérification que tous les paramètres requis sont fournis
  if (is.null(updated_entry) || is.null(target_table) ||
    is.null(unique_id_col) || is.null(unique_id_val)) {
    stop("Missing required parameter")
    return(list(message = "Missing required parameter", status = "error"))
  }

  # Établissement de la connexion à la base de données
  db_con <- connect_to_database()

  # Construction de la requête d'update
  update_query <- sprintf("UPDATE %s SET ", target_table)

  # Génération des clauses SET pour la mise à jour
  set_clauses <- sapply(names(updated_entry), function(col) {
    if (!is.null(updated_entry[[col]])) {
      # Escape single quotes in values to prevent SQL injection and syntax errors
      safe_value <- gsub("'", "''", updated_entry[[col]], fixed = TRUE)
      sprintf("%s = '%s'", col, safe_value)
    } else {
      sprintf("%s = NULL", col)
    }
  }, USE.NAMES = FALSE)

  # Finalisation de la requête d'update avec les conditions WHERE
  update_query <- paste(
    update_query, paste(set_clauses, collapse = ", "),
    sprintf("WHERE %s = '%s'", unique_id_col, unique_id_val)
  )


  # Exécution de la requête d'update
  DBI::dbExecute(db_con, update_query)

  # Fermeture de la connexion à la base de données
  DBI::dbDisconnect(db_con)

  return(list(message = "Entry successfully updated", status = "success"))
}

# ==============================================================================
#' Supprimer une Entrée d'une Table
#'
#' Supprime une entrée spécifique d'une table dans la base de données.
#'
#' @param target_table Nom de la table de laquelle supprimer l'entrée.
#' @param unique_id_col Nom de la colonne contenant l'identifiant unique.
#' @param unique_id_val Valeur de l'identifiant unique de l'entrée à supprimer.
#' @export
delete_entry_in_table <- function(target_table = NULL, unique_id_col = NULL,
                                  unique_id_val = NULL) {
  # Vérification des paramètres requis
  if (is.null(target_table) || is.null(unique_id_col) || is.null(unique_id_val)) {
    stop("Missing required parameter")
  }

  # Connexion à la base de données
  db_con <- connect_to_database()

  # Construction de la requête SQL de suppression
  delete_query <- sprintf(
    "DELETE FROM %s WHERE %s = '%s'",
    target_table, unique_id_col, unique_id_val
  )

  # Exécution de la requête de suppression
  DBI::dbExecute(db_con, delete_query)

  # Fermeture de la connexion à la base de données
  DBI::dbDisconnect(db_con)

  return("Entry successfully deleted")
}

#' @export
supprimer_lignes_vides <- function(nom_table, nom_colonne) {
  conn <- connect_to_database()
  # Construire la requête SQL
  requete <- sprintf(
    "DELETE FROM %s WHERE %s IS NULL OR %s = ''",
    nom_table, nom_colonne, nom_colonne
  )

  # Exécuter la requête
  DBI::dbExecute(conn, requete)
  DBI::dbDisconnect(conn)
  # Retourner un message de succès (ou vous pouvez choisir de retourner le nombre de lignes affectées)
  paste("Les lignes avec des cellules vides dans la colonne", nom_colonne, "ont été supprimées.")
}

# ==============================================================================
#' Ajouter des Paramètres de Connexion à la Base de Données
#'
#' Ajoute des paramètres de connexion pour une source de données spécifique
#' dans la base de données.
#'
#' @param params_list Liste des paramètres de connexion à enregistrer.
#' @param meta Informations supplémentaires telles que le nom de la donnée,
#'             la source de données et le propriétaire.
#' @import sodium
#' @export
th2_add_db_connection_params <- function(params_list = list(), meta = list(
                                           data_name = "",
                                           data_source = "",
                                           owner = ""
                                         )) {
  # Préparation des valeurs de paramètres
  param_values <- setNames(
    object = lapply(1:length(params_list), function(x) if (is.null(params_list[[x]])) NA else params_list[[x]]),
    nm = paste0("PARAM", 1:length(params_list))
  )

  # Construction du data frame avec des informations supplémentaires
  data_connection_other_infos <- data.frame(
    TABLE_ID = meta$data_name,
    DATA_SOURCE = meta$data_source,
    OWNER = meta$owner,
    CREATED_AT = Sys.time(),
    UPDATED_AT = Sys.time()
  )

  # Combinaison des informations de connexion et des paramètres
  data_connection_meta <- cbind(data_connection_other_infos, t(param_values))

  # Chiffrement des colonnes spécifiées
  cols_to_encrypt <- grep("^PARAM", names(data_connection_meta), value = TRUE)
  data_connection_meta[cols_to_encrypt] <- lapply(
    data_connection_meta[cols_to_encrypt],
    th2dbm::encrypt_column
  )

  # Connexion à la base de données et vérification de l'existence de l'entrée
  db_con <- th2dbm::connect_to_database()
  query_statement <- glue::glue(
    "SELECT * FROM data_connection_params WHERE OWNER = '{meta$owner}' AND ",
    "TABLE_ID = '{meta$data_name}'",
    ifelse(meta$data_name == "git_configuration", " AND PARAM2 = '{data_connection_meta$PARAM2}'", "")
  )
  query_res <- DBI::dbSendQuery(db_con, statement = query_statement)
  config_params <- DBI::dbFetch(query_res)
  DBI::dbDisconnect(db_con)

  # Mise à jour ou ajout de l'entrée dans la table
  if (nrow(config_params) == 0) {
    add_entry_to_table(new_entry = data_connection_meta, target_table = "data_connection_params")
  } else {
    update_entry_in_table(
      updated_entry = data_connection_meta,
      target_table = "data_connection_params",
      unique_id_col = "TABLE_ID",
      unique_id_val = meta$data_name
    )
  }
}

# ==============================================================================
#' Récupérer les Données depuis la Base de Données
#'
#' Cette fonction récupère des données d'une table spécifique dans la base de données. Elle permet une sélection optionnelle de colonnes et un filtrage.
#'
#' @param table Une chaîne spécifiant le nom de la table à partir de laquelle les données seront récupérées.
#' @param cols Un vecteur de caractères spécifiant les noms des colonnes à récupérer. Si vide, toutes les colonnes sont récupérées.
#' @param filter Une liste nommée où les noms sont les noms des colonnes et les valeurs sont les critères de filtrage.
#'
#' @return Un dataframe contenant les données récupérées.
#' @export
#' @examples
#' fetch_data_from_db(table = "ma_table", cols = c("id", "nom"), filter = list(id = 1))
fetch_data_from_db <- function(table = NULL,
                               cols = c(),
                               filter = list()) {
  # Build the SELECT clause
  if (!length(cols)) {
    select_clause <- "SELECT *"
  } else {
    select_clause <- paste("SELECT", paste(cols, collapse = ", "))
  }

  # Build the FROM clause
  if (is.null(table)) {
    stop("Table name is required")
  }
  from_clause <- glue::glue(" FROM {table} ")

  # Build the WHERE clause (if filters are provided)
  where_clause <- ""
  if (length(filter)) {
    filter_string <-
      paste(names(filter), "=", sapply(filter, function(x) {
        if (class(x) == "character") {
          paste0("'", x, "'")
        } else {
          x
        }
      }), collapse = " AND ")
    where_clause <-
      paste(" WHERE ", filter_string)
  }

  # Combine the clauses
  sql <- glue::glue("{select_clause} {from_clause} {where_clause}")

  db_con <- th2dbm::connect_to_database()
  rs <- DBI::dbSendQuery(db_con, statement = sql)
  result <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)
  DBI::dbDisconnect(db_con)
  return(result)
}


# ==============================================================================
#' Récupérer les Données depuis la Base de Données
#'
#' Cette fonction récupère des données d'une table spécifique dans la base de données. Elle permet une sélection optionnelle de colonnes et un filtrage.
#'
#' @param sql
#'
#' @return Un dataframe contenant les données récupérées.
#' @export
fetch_data_from_db_by_sql <- function(sql = NULL) {
  db_con <- th2dbm::connect_to_database()
  rs <- DBI::dbSendQuery(db_con, statement = sql)
  result <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)
  DBI::dbDisconnect(db_con)
  return(result)
}

# ==============================================================================
#' Mettre à Jour les Données dans la Base de Données
#'
#' Cette fonction met à jour les enregistrements dans une table spécifiée de la base de données. Elle nécessite le nom de la table, des mises à jour à appliquer et des filtres pour identifier les lignes à mettre à jour.
#'
#' @param table Le nom de la table dans laquelle effectuer la mise à jour.
#' @param updates Une liste nommée des champs à mettre à jour et leurs nouvelles valeurs.
#' @param filter Une liste nommée des critères pour filtrer les enregistrements à mettre à jour.
#'
#' @return Le nombre de lignes affectées par la mise à jour.
#' @export
#' @examples
#' update_data_in_db(table = "ma_table", updates = list(nom = "Dupont"), filter = list(id = 1))
update_data_in_db <- function(table, updates = list(), filter = list()) {
  if (is.null(table) || !length(updates)) {
    stop("Both table name and updates are required")
  }

  # Build the UPDATE clause
  update_string <- paste(names(updates), "=", sapply(updates, function(x) {
    if (is.character(x)) {
      paste0("'", x, "'")
    } else {
      as.character(x)
    }
  }), collapse = ", ")

  update_clause <- glue::glue("UPDATE {table} SET {update_string}")

  # Build the WHERE clause (if filters are provided)
  where_clause <- ""
  if (length(filter)) {
    filter_string <- paste(names(filter), "=", sapply(filter, function(x) {
      if (is.character(x)) {
        paste0("'", x, "'")
      } else {
        as.character(x)
      }
    }), collapse = " AND ")
    where_clause <- paste(" WHERE ", filter_string)
  }

  # Combine the clauses
  sql <- glue::glue("{update_clause} {where_clause}")

  # Execute the SQL command
  db_con <- th2dbm::connect_to_database()
  DBI::dbExecute(db_con, sql)
  DBI::dbDisconnect(db_con)
}

# ==============================================================================
#' Supprimer les Données de la Base de Données
#'
#' Cette fonction supprime des enregistrements d'une table spécifiée basée sur des critères de filtrage.
#'
#' @param table Le nom de la table de laquelle les données seront supprimées.
#' @param filter Une liste nommée des critères pour filtrer les enregistrements à supprimer.
#'
#' @return Le nombre de lignes supprimées.
#' @export
#' @examples
#' delete_data_from_db(table = "ma_table", filter = list(id = 1))
delete_data_from_db <- function(table, filter = list()) {
  if (is.null(table) || !length(filter)) {
    stop("Both table name and filter are required")
  }

  # Build the WHERE clause
  filter_string <- paste(names(filter), "=", sapply(filter, function(x) {
    if (is.character(x)) {
      paste0("'", x, "'")
    } else {
      as.character(x)
    }
  }), collapse = " AND ")

  where_clause <- paste(" WHERE ", filter_string)

  # Combine the clauses
  sql <- glue::glue("DELETE FROM {table} {where_clause}")

  # Execute the SQL command
  db_con <- th2dbm::connect_to_database()
  DBI::dbExecute(db_con, sql)
  DBI::dbDisconnect(db_con)
}




# ==============================================================================
#' Mettre à jour les Valeurs NULL dans la Base de Données
#'
#' Cette fonction met à jour les lignes d'une colonne spécifiée dans une table,
#' en remplaçant les valeurs NULL ou NA par une valeur donnée.
#'
#' @param table Le nom de la table à mettre à jour.
#' @param column Le nom de la colonne à mettre à jour.
#' @param new_value La nouvelle valeur à utiliser pour remplacer les valeurs NULL ou NA.
#'
#' @return Le nombre de lignes affectées.
#' @export
#' @examples
#' update_null_values_in_db(table = "ma_table", column = "ma_colonne", new_value = "valeur_par_defaut")
update_null_values_in_db <- function(table, column, new_value) {
  if (is.null(table) || is.null(column)) {
    stop("Le nom de la table et de la colonne sont requis")
  }

  # Déterminer la bonne représentation de la nouvelle valeur pour la requête SQL
  value_expression <- if (is.logical(new_value)) {
    if (new_value) "TRUE" else "FALSE"
  } else if (is.character(new_value)) {
    paste0("'", new_value, "'")
  } else {
    as.character(new_value)
  }

  # Construire la requête SQL pour mettre à jour les valeurs NULL sans référence à 'NA'
  sql <- sprintf("UPDATE %s SET %s = %s WHERE %s IS NULL", table, column, value_expression, column)

  # Exécuter la requête SQL
  db_con <- th2dbm::connect_to_database()
  result <- DBI::dbExecute(db_con, sql)
  DBI::dbDisconnect(db_con)

  return(result)
}
