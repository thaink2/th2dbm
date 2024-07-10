#' Thaink2 local packages
#' @description Récupérer les packages local de thaink2 à partir de l'API artifact
#' @param url Un string
#'
#' @return une list()
#' @export
#'
#' @examples
#' get_th2_local_phg("https://r-project.org") # Ne pas oublier de mettre un token si besoin
#'
get_th2_local_pkg <- function(package_name = "") {
  response <- get_API(package_name = package_name)

  donnees <- response %>%
    httr2::resp_body_html()

  texte_pre <- donnees %>%
    rvest::html_node("body") %>%
    rvest::html_text()

  packg <- strsplit(texte_pre, "\n")[[1]]

  packg <- unlist(strsplit(packg, " "))

  final_package <- grep("\\.tar\\.gz", packg, value = TRUE) %>%
    gsub("\\.tar\\.gz", "", .) %>%
    strsplit(., split = "_") %>%
    unlist()
  final_package_df <- data.frame()

  for (i in seq(1, length(final_package), 2)) {
    temp_df <- data.frame(package = final_package[i], version = final_package[i + 1])
    final_package_df <- dplyr::bind_rows(final_package_df, temp_df)
  }

  return(final_package_df)
}

#' @export
get_API <- function(package_name = "") {
  TOKEN <- Sys.getenv("ARTIFACTORY_TOKEN")
  if (nchar(package_name) != 0) {
    package_path <- glue::glue("/Archive/{package_name}")
  } else {
    package_path <- ""
  }

  api_url <- paste0(
    "https://",
    TOKEN,
    "@thaink2.jfrog.io/artifactory/thaink2r-cran-local/src/contrib",
    package_path
  )

  req <- httr2::request(api_url)
  response <- tryCatch(req %>%
    httr2::req_perform(), error = function(e) e)
  if ("error" %in% class(response)) {
    return(NULL)
  } # pas de version dans l'archive
  if (httr2::resp_status(response) != 200) {
    return(NULL)
  } else {
    return(response)
  }
}

#' @export
get_versions_by_package <- function(package_name = NULL) {
  response <- get_API(package_name = package_name)

  if (is.null(response)) {
    return(NULL)
  }

  donnees <- response %>%
    httr2::resp_body_html()

  texte_pre <- donnees %>%
    rvest::html_node("body") %>%
    rvest::html_text()

  packg <- strsplit(texte_pre, "\n")[[1]]
  packg <- unlist(strsplit(packg, " "))
  versions <- grep("\\b\\d+\\.\\d+\\.\\d+\\b", packg, value = TRUE)
  versions <- gsub("/", "", versions)

  pkg_version_df <- data.frame(package = package_name, version = versions)

  return(pkg_version_df)
}
