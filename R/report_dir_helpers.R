#' Créer un Dossier pour les Rapports
#'
#' Cette fonction crée un nouveau dossier destiné à stocker les rapports. Si le dossier existe déjà, la fonction ne fait rien.
#'
#' @param dir_name Nom du dossier à créer. Par défaut, il s'agit de "reports".
#' @param subdir Booléen indiquant si la création doit inclure la création de sous-dossiers. Par défaut à FALSE.
#' @return Renvoie NULL de manière invisible. La fonction est utilisée pour son effet de côté (création d'un dossier).
#' @examples
#' create_dir("mes_rapports")
#' @export
create_dir <- function(dir_name = "reports", subdir = FALSE) {
  if (dir.exists(dir_name)) {
    cat("Le dossier spécifié existe déjà!\n")
    return(NULL)
  }
  dir.create(dir_name, recursive = subdir)
}

# ========================================================================================================
#' Générer des Fichiers HTML
#'
#' Cette fonction crée un nombre spécifié de fichiers HTML avec un contenu donné. Si aucun contenu n'est spécifié, un contenu par défaut est utilisé.
#'
#' @param file_nbr Nombre de fichiers HTML à générer.
#' @param file_base_name Préfixe du nom de fichier pour chaque fichier HTML généré.
#' @param file_path Chemin du dossier où les fichiers HTML seront créés.
#' @param file_ext Extension des fichiers (doit être '.html').
#' @param file_content Contenu HTML à écrire dans chaque fichier. Si vide, un contenu par défaut est utilisé.
#' @return Une liste contenant le chemin et le nom de chaque fichier généré.
#' @examples
#' generate_html_file(file_nbr = 2, file_content = "<html><body><p>Hello, world!</p></body></html>")
#' @export
generate_html_file <- function(file_nbr = 1, file_base_name = "report_", file_path = "reports/", file_ext = ".html", file_content = "") {
  if (!dir.exists(file_path)) {
    create_dir(dir_name = file_path)
  }

  generated_files <- list()

  for (i in 1:file_nbr) {
    file_name <- paste0(file_base_name, i, file_ext)
    full_path <- paste0(file_path, file_name)

    if (file_content == "") {
      content <- paste("<html><head><title>", "Report ", i, "</title></head>",
                       "<body><h1>", "Report ", i, "</h1></body></html>",
                       sep = ""
      )
    } else {
      content <- file_content
    }

    writeLines(content, con = full_path)

    generated_files[[i]] <- list("path" = full_path, "name" = file_name)
  }

  return(generated_files)
}


#' Lister les Détails des Fichiers et Dossiers dans un Répertoire
#'
#' Cette fonction liste les fichiers et dossiers dans le répertoire spécifié, incluant ou excluant les sous-dossiers selon le paramètre. Elle retourne une liste contenant le nombre total de fichiers, le nombre total de dossiers, et les détails de chaque fichier.
#'
#' @param directory_path Chemin du répertoire à inspecter.
#' @param include_subdirs Booléen indiquant si les sous-dossiers doivent être inclus.
#' @return Une liste avec le nombre total de fichiers, le nombre total de dossiers, et les détails de chaque fichier et dossier.
#' @examples
#' list_files_details(directory_path = ".", include_subdirs = TRUE)
#' @export
list_files_details <- function(directory_path = "reports", include_subdirs = FALSE) {
  if (!dir.exists(directory_path)) {
    cat("Le dossier spécifié n'existe pas.\n")
    return(NULL)
  }

  all_items <- list.files(directory_path, full.names = TRUE, recursive = include_subdirs)

  is_dir <- sapply(all_items, function(x) file.info(x)$isdir)
  files <- all_items[!is_dir]
  dirs <- all_items[is_dir]

  file_details <- list()
  total_files <- length(files)
  total_dirs <- length(dirs)

  for (file_path in files) {
    file_size <- file.info(file_path)$size
    file_details[[length(file_details) + 1]] <- list(path = file_path, name = basename(file_path), size = file_size)
  }

  dir_details <- vector("list", length(dirs))
  if (include_subdirs == FALSE) {
    for (i in seq_along(dirs)) {
      dir_details[[i]] <- list(path = dirs[i], name = basename(dirs[i]))
    }
  }

  return(list(total_files = total_files, total_dirs = total_dirs, file_details = file_details, dir_details = dir_details))
}
