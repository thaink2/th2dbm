#' Accorder la Permission Machine Learning
#'
#' Notifie les utilisateurs qui ont reçu l'accès à des modèles ou applications spécifiques et gère ces permissions dans une base de données.
#'
#' @param target_table Nom de la table cible dans la base de données où les permissions sont stockées.
#' @param meta Liste contenant les métadonnées pour l'accès accordé, telles que l'ID de l'objet, l'utilisateur courant, les utilisateurs autorisés, le type d'objet et le niveau de permission.
#' @param permission_action Action de permission à effectuer ("grant" pour accorder, "delete" pour supprimer, "get" pour obtenir les permissions existantes).
#' @description Cette fonction permet de gérer les permissions pour les modèles ou applications de machine learning au sein d'une base de données.
#' @export
grant_ml_permission_db <-
  function(target_table = "th2_ml_permissions",
           meta = list(
             OBJECT_ID = NULL,
             current_user = Sys.getenv("SHINYPROXY_USERNAME"),
             current_PERMITTED_USERS = NULL,
             CURRENT_OBJECT_TYPE = NULL,
             current_PERMISSION_LEVEL = NULL
           ),
           permission_action = "grant") {
    # Connexion à la base de données
    db_con <- connect_to_database()
    available_tables <- DBI::dbListTables(conn = db_con)
    if (!target_table %in% available_tables) {
      return(NULL)
    }
    # Récupération de la table actuelle
    query_statement <- glue::glue("SELECT * FROM {target_table}")
    query_res <- DBI::dbSendQuery(db_con, statement = query_statement)
    permission_db <- DBI::dbFetch(query_res)
    DBI::dbClearResult(query_res)
    colnames(permission_db) <- toupper(colnames(permission_db))
    DBI::dbDisconnect(db_con)

    if (permission_action == "get") {
      # Vérifie si la table des permissions est vide
      if (nrow(permission_db) == 0) {
        return(NULL)
      }

      if (is.null(meta$CURRENT_OBJECT_TYPE)) {
        # Vérifie si le current_user est OWNER de l'OBJECT_ID
        owner_row <- permission_db %>%
          dplyr::filter(
            OBJECT_ID == !!meta$OBJECT_ID &
              OBJECT_CREATOR == !!meta$current_user &
              PERMISSION_LEVEL == "Owner"
          )

        if (nrow(owner_row) > 0) {
          # Si l'utilisateur est OWNER, retourne la ligne correspondante
          return(owner_row)
        } else {
          # Sinon, vérifie si le current_user est dans les PERMITTED_USERS
          permitted_row <- permission_db %>%
            dplyr::filter(OBJECT_ID == !!meta$OBJECT_ID &
              PERMITTED_USERS == !!meta$current_user)
          if (nrow(permitted_row) > 0) {
            return(permitted_row)
          } else {
            print("Vous n'avez pas de permission")
            return(NULL)
          }
        }
      } else {
        permission_db <- permission_db %>%
          dplyr::filter(
            OBJECT_CREATOR == !!meta$current_user |
              (PERMITTED_USERS == !!meta$current_user)
          ) %>%
          dplyr::filter(OBJECT_TYPE == !!meta$CURRENT_OBJECT_TYPE) %>%
          dplyr::arrange(PERMISSION_TIME)
        # regrouper des données par object et son créateur, puis pour chaque groupe, vous voulez obtenir le PERMISSION_TIME le plus récent
        temp_data <- permission_db %>%
          dplyr::group_by(OBJECT_ID, OBJECT_CREATOR) %>%
          dplyr::reframe(PERMISSION_TIME = max(PERMISSION_TIME))

        # récupérer ceux qui ont une permission ou son propriétaire
        permission_db <- temp_data %>%
          dplyr::left_join(permission_db,
            by = c("OBJECT_ID", "OBJECT_CREATOR", "PERMISSION_TIME")
          ) %>%
          dplyr::filter(PERMISSION_LEVEL != "None" |
            OBJECT_CREATOR == !!meta$current_user)

        return(permission_db)
      }
    } else if (permission_action == "delete") {
      if (nrow(permission_db) == 0) {
        return(NULL)
      }
      temp <- 1:length(permission_db[["OBJECT_ID"]])
      lapply(temp, function(i) {
        if (permission_db[i, "OBJECT_ID"] == meta$OBJECT_ID) {
          permission_db <-
            delete_ml_permissions(
              target_table = "th2_ml_permissions",
              OBJECT_ID_to_del = meta$OBJECT_ID
            )
        }
      })
    } else if (permission_action == "grant") {
      if (nrow(permission_db) == 0) {
        return(NULL)
      }
      current_object <-
        permission_db %>% dplyr::filter(OBJECT_ID == !!meta$OBJECT_ID)

      # si on a pas de créateur d'objet actuel, on prend le créateur de l'objet
      if (is.null(meta$current_user)) {
        meta$current_user <- current_object %>%
          dplyr::pull(OBJECT_CREATOR) %>%
          head(1)
      }
      can_grant <- TRUE
      are_owners <- FALSE

      # si on retrouve l'objet dans la db,
      if (nrow(current_object) > 0) {
        # on verifie si l'utilisateur actuel à les droits
        can_grant <- current_object %>%
          dplyr::filter(
            PERMITTED_USERS == !!meta$current_user &
              PERMISSION_LEVEL %in% c("Manage", "Owner")
          ) %>%
          nrow() > 0

        are_owners <- current_object %>%
          dplyr::filter(
            PERMITTED_USERS %in% !!meta$current_PERMITTED_USERS &
              (PERMISSION_LEVEL %in% c("Owner"))
          ) %>%
          nrow() > 0
      }
      if ((can_grant == TRUE) & (are_owners == FALSE)) {
        new_users_perm <- data.frame(
          OBJECT_ID = meta$OBJECT_ID,
          OBJECT_CREATOR = Sys.getenv("SHINYPROXY_USERNAME"),
          OBJECT_TYPE = meta$CURRENT_OBJECT_TYPE,
          PERMITTED_USERS = meta$current_PERMITTED_USERS,
          PERMISSION_LEVEL = meta$current_PERMISSION_LEVEL,
          PERMISSION_TIME = paste(Sys.time()),
          COL_ID = generateID(prefix = "permission")
        )
        if (nrow(current_object) > 0) {
          current_object <- current_object %>%
            dplyr::anti_join(
              new_users_perm,
              by = c(
                "OBJECT_ID",
                "OBJECT_CREATOR",
                "PERMITTED_USERS",
                "OBJECT_TYPE"
              )
            ) %>%
            dplyr::bind_rows(new_users_perm)
        } else {
          current_object <- current_object %>%
            dplyr::bind_rows(new_users_perm)
        }
        c_object <<- new_users_perm

        create_update_ml_permissions(table_metadata = current_object, target_table = target_table)

        th2utils::send_email_notification(
          recipient = new_users_perm$PERMITTED_USERS,
          permission_level = new_users_perm$PERMISSION_LEVEL,
          permission_data = new_users_perm$OBJECT_ID,
          template_name = "permission_notification"
        )
        return(glue::glue("Permission à {meta$current_PERMITTED_USERS} accordé!"))
      } else {
        return(glue::glue(
          "Permission à {meta$current_PERMITTED_USERS} non accordé!"
        ))
      }
    }
  }

#' Créer ou Mettre à Jour les Permissions Machine Learning
#'
#' Met à jour ou crée des entrées de permissions pour l'accès à des modèles ou applications de machine learning dans une base de données spécifique.
#'
#' @param table_metadata Données de la table contenant les permissions à créer ou mettre à jour.
#' @param target_table La table cible où les permissions doivent être stockées ou mises à jour.
#' @export
create_update_ml_permissions <-
  function(table_metadata = NULL,
           target_table = "ml_permissions_db") {
    db_con <- connect_to_database()
    target_row <- table_metadata %>%
      dplyr::pull(OBJECT_ID) %>%
      unique()
    query_statement <-
      glue::glue("DELETE FROM {target_table} WHERE OBJECT_ID = '{target_row}'")
    query_res <- DBI::dbExecute(db_con, statement = query_statement)
    DBI::dbWriteTable(
      conn = db_con,
      name = target_table,
      value = table_metadata,
      append = TRUE
    )
    DBI::dbDisconnect(db_con)
    return("metadata updated")
  }

#' Supprimer les Permissions Machine Learning
#'
#' Supprime une permission spécifique liée à un modèle ou une application de machine learning dans la base de données.
#'
#' @param target_table Nom de la table cible dans la base de données d'où supprimer la permission.
#' @param OBJECT_ID_to_del ID de l'objet pour lequel la permission doit être supprimée.
#' @export
delete_ml_permissions <-
  function(target_table = NULL,
           OBJECT_ID_to_del = NULL) {
    db_con <- connect_to_database()
    query_statement <-
      glue::glue("DELETE FROM {target_table} WHERE OBJECT_ID = '{OBJECT_ID_to_del}'")
    query_res <- DBI::dbExecute(db_con, statement = query_statement)

    # metadata2 <<- table_metadata
    DBI::dbDisconnect(db_con)
    print("Permission deleted!")
    return(query_res)
  }



#' Build User Permission Workflow Profile
#'
#' This function generates a permission profile for a given user, based on their permissions in a specified target table.
#'
#' @param target_table The name of the target table containing permissions. Default is "th2_wf_permissions".
#' @param user The username for which the permission profile is being generated. Default is the username retrieved from the environment variable "SHINYPROXY_USERNAME".
#'
#' @return A list containing various permission flags and lists based on the user's permissions:
#' \itemize{
#'   \item \code{can_create_wf}: Indicates whether the user has permission to create workflows.
#'   \item \code{list_view}: A list of workflows the user has permission to view.
#'   \item \code{list_edit}: A list of workflows the user has permission to edit.
#'   \item \code{list_deploy}: A list of workflows the user has permission to deploy.
#'   \item \code{list_delete}: A list of workflows the user has permission to delete.
#'   \item \code{can_access_module}: Indicates whether the user has permission to access the module, based on their view permissions or ability to create workflows.
#' }
#'
#' @export
build_user_permission_wf_profile <-
  function(target_table = "th2_wf_permissions",
           user = Sys.getenv("SHINYPROXY_USERNAME"), object_type = "ml") {
    permission_profile <- list()
    list_all_permission <-
      get_user_permission(target_table = "th2_wf_permissions")
    print(list_all_permission)
    # check create permission
    permission_profile$can_create_wf <-
      can_create_wf(list_permission = list_all_permission)
    # permission_profile$can_create_wf <- TRUE
    permission_profile$list_view <-
      get_list_wf_base_on_filter(
        list_permission = list_all_permission,
        filter = list(PERMISSION_LEVEL = c("View", "Owner", "Deploy", "Edit", "Manage"), OBJECT_TYPE = object_type)
      )
    permission_profile$list_edit <-
      get_list_wf_base_on_filter(
        list_permission = list_all_permission,
        filter = list(PERMISSION_LEVEL = c("Owner", "Deploy", "Edit", "Manage"), OBJECT_TYPE = object_type)
      )
    permission_profile$list_deploy <-
      get_list_wf_base_on_filter(
        list_permission = list_all_permission,
        filter = list(PERMISSION_LEVEL = c("Owner", "Deploy", "Manage"), OBJECT_TYPE = object_type)
      )
    permission_profile$list_delete <-
      get_list_wf_base_on_filter(
        list_permission = list_all_permission,
        filter =
          list(PERMISSION_LEVEL = c("Owner", "Manage"), OBJECT_TYPE = object_type)
      )

    permission_profile$can_access_module <-
      length(permission_profile$list_view) > 0 ||
        permission_profile$can_create_wf
    return(permission_profile)
  }

#' @export
get_user_permission <- function(target_table = "th2_wf_permissions",
                                user = Sys.getenv("SHINYPROXY_USERNAME"), object_type = NULL ) {
  # Connexion à la base de données
  db_con <- th2product::connect_to_database()
  available_tables <- DBI::dbListTables(conn = db_con)
  if (!target_table %in% available_tables) {
    return(NULL)
  }
  condition <- "where 1=1"
  if(!is.null(user)){
    condition <- paste(condition, "and permitted_users ='{user}'")
  }
  if( !is.null(object_type)){
    condition <- paste(condition, "and object_type ='{object_type}'")
  }
  query_statement <-
    glue::glue(
      paste(
        "SELECT * FROM (
          SELECT permission_level,object_id,object_type
                ,ROW_NUMBER() OVER(PARTITION BY object_id ORDER BY permission_time DESC) AS rn
          FROM {target_table}",
        condition, ")AS subquery
      WHERE rn = 1")
    )

  query_res <- DBI::dbSendQuery(db_con, statement = query_statement)
  permission_db <- DBI::dbFetch(query_res)
  DBI::dbClearResult(query_res)
  colnames(permission_db) <- toupper(colnames(permission_db))
  DBI::dbDisconnect(db_con)
  return(permission_db)
}

filter_list_permission <-
  function(list_permission = NULL,
           filter = list()) {
    result <- list_permission
    if (is.null(list_permission) | is.null(filter)) {
      return(data.frame())
    }

    for (key in names(filter)) {
      print(key)
      print(filter[[key]])

      col_name <- rlang::sym(key)
      result <-
        result %>% dplyr::filter(.data[[col_name]] %in% filter[[key]])
    }
    return(result)
  }

#' @export
add_create_wf_permission <- function(user_mail = NULL, target_table = "th2_wf_permissions") {
  new_user_infos <- data.frame(object_creator = user_mail, permission_time = paste0(Sys.time()), OBJECT_TYPE = "access_workflow_module", object_id = "th2mageai module", permitted_users = user_mail, permission_level = "Owner")
  th2product::add_entry_to_table(new_entry = new_user_infos, target_table = target_table)
}


can_create_wf <- function(list_permission = NULL) {
  if (is.null(list_permission)) {
    return(FALSE)
  }

  filter <- list(OBJECT_TYPE = "access_workflow_module")
  access_permission <-
    filter_list_permission(list_permission, filter)

  if (nrow(access_permission) > 0) {
    return(TRUE)
  }

  return(FALSE)
}



get_list_wf_base_on_filter <-
  function(list_permission = NULL,
           filter = c()) {
    if (is.null(list_permission) || is.null(filter)) {
      return(list())
    }
    list_view_wf <-
      filter_list_permission(list_permission, filter) %>% .$OBJECT_ID

    return(list_view_wf)
  }
