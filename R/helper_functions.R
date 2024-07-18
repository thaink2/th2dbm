#' th_shinyalert
#' @param title alert title
#' @param text text to be displayed under title
#' @param type info, success, fail
#' @param html whether tu use html  or not
#' @param imageUrl image url to be displayed in the alert popup
#' @param confirmButtonCol color of the confirm button
#' @export
th_shinyalert <-
  function(title,
                          text,
                          type = "info",
                          confirmButtonCol = "#013DFF",
                          html = FALSE,
                          imageUrl = NULL) {
  if(is.null(imageUrl)){
    imageUrl <- "https://raw.githubusercontent.com/thaink2/thaink2publicimages/main/thaink2_logo_circle.png"
  }
  shinyalert::shinyalert(
    title = title,
    text = text,
    type = type,
    imageUrl = imageUrl,
    html = html,
    confirmButtonCol = confirmButtonCol
  )
}


#' Préparer l'En-tête de l'Application
#'
#' Cette fonction crée un en-tête personnalisé pour une application Shiny, incluant le logo de l'application et des liens vers le site web principal.
#'
#' @return Un objet `dashboardHeader` de `bs4Dash` contenant l'en-tête personnalisé de l'application.
#' @export
prepare_app_header <- function() {
  app_logo <- "https://static.wixstatic.com/media/9aacb8_9886b3a143c2470d96ec76a181e67e49~mv2.png/v1/fill/w_195,h_46,al_c,q_85,usm_0.66_1.00_0.01,enc_auto/thaink2-logo-blue-big.png"
  bs4Dash::bs4DashNavbar(
    skin = "light",
    title = bs4Dash::bs4DashBrand(
      title = "th2dbm"
    ),
    rightUi = shiny::tagList(
      shiny::tags$li(
        class = "nav-item dropdown",
        shiny::tags$a(
          href = "http://supplai.thaink2.com/",
          target = "_blank",
          class = "nav-link",
          shiny::icon("power-off")
        )
      ),
      shiny::tags$li(
        class = "nav-item dropdown",
        shiny::tags$a(
          href = "https://www.thaink2.com/",
          target = "_blank",
          class = "nav-link",
          shiny::tags$img(src = app_logo, style = "height: 30px")
        )
      ),
      shiny::tags$li(
        class = "nav-item dropdown",
        shiny::actionButton(
          inputId = "user_button",
          label = NULL,
          icon = shiny::icon("user"),
          class = "nav-link"
        )
      )
    )
  )
}

#' Corriger la Date
#'
#' Convertit un timestamp numérique en date lisible ou retourne la date d'entrée si elle n'est pas numérique.
#'
#' @param current_date Date actuelle pouvant être un timestamp numérique ou une chaîne de caractères représentant la date.
#' @return La date corrigée en format lisible si numérique, sinon retourne la date d'entrée.
#' @export
correct_date <- function(current_date) {
  return(ifelse(is.na(as.numeric(current_date)),
    current_date,
    paste(as.POSIXct(as.numeric(current_date)))
  ))
}

#' Générer un Identifiant Unique
#'
#' Génère un identifiant unique en utilisant un préfixe spécifié et un UUID.
#'
#' @param prefix Préfixe à utiliser pour l'identifiant généré. Par défaut, "UserEntry".
#' @return Un identifiant unique sous forme de chaîne de caractères.
#' @export
generateID <- function(prefix = "UserEntry") {
  sprintf("%s_%s", prefix, uuid::UUIDgenerate())
}

#' Créer un Fichier Aide pour Rafraîchissement
#'
#' Crée un fichier aide pour le rafraîchissement d'un module dans une application Shiny, utilisé pour stocker le timestamp du dernier rafraîchissement.
#'
#' @param mod_id Identifiant du module pour lequel créer le fichier de rafraîchissement.
#' @param refresh_folder Dossier où stocker le fichier de rafraîchissement. Par défaut, "./refresh/".
#' @return Le chemin du fichier de rafraîchissement créé.
#' @export
create_refresh_helper_file <- function(mod_id = NULL, refresh_folder = "./refresh/") {
  if (!dir.exists(refresh_folder)) dir.create(refresh_folder, recursive = TRUE)
  mod_refresh_file <- paste0(refresh_folder, "refresher_", mod_id, ".rds")
  Sys.time() %>%
    saveRDS(mod_refresh_file)
  return(mod_refresh_file)
}

#' Supprimer les Entrées Shiny
#'
#' Supprime dynamiquement des entrées Shiny spécifiées par leur identifiant.
#'
#' @param id Identifiant des éléments d'entrée Shiny à supprimer.
#' @param .input Objet `.input` de Shiny contenant les entrées.
#' @export
remove_shiny_inputs <- function(id, .input) {
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
}


#' Vérifier le format d'un email et retourner le nom formaté
#'
#' Cette fonction prend un email en entrée et vérifie s'il est au format prénom.nom@domaine.
#' Si c'est le cas, elle retourne une chaîne de caractères au format "Prénom Nom" avec les
#' premières lettres en majuscule. Sinon, elle retourne l'email tel quel.
#'
#' @param email L'adresse email à vérifier.
#'
#' @return Une chaîne de caractères contenant "Prénom Nom" si l'email est au format
#' prénom.nom@domaine, sinon l'email original.
#'
#' @export
#'
#' @examples
#' verifier_format_email("prenom.nom@exemple.com")
#' verifier_format_email("emailnonconforme@domaine")
verifier_format_email <- function(email) {
  # Utilise une expression régulière pour vérifier le format général prenom.nom@domaine
  if (grepl("^[a-zA-Z]+\\.[a-zA-Z]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", email)) {
    # Extraire le prénom et le nom à partir de l'email
    parts <- strsplit(email, "[@.]")[[1]] # Sépare par '@' et '.'
    prenom <- parts[1]
    nom <- parts[2]
    formatted_name <- paste(
      paste0(toupper(substring(prenom, 1, 1)), tolower(substring(prenom, 2))),
      paste0(toupper(substring(nom, 1, 1)), tolower(substring(nom, 2)))
    )
    return(formatted_name)
  } else {
    # Retourne l'email original si le format ne correspond pas
    return(email)
  }
}

#' @export
get_current_url <- function(session = NULL, port = 3838) {
  protocol <- session$clientData$url_protocol
  hostname <- session$clientData$url_hostname
  pathname <- session$clientData$url_pathname
  search <- session$clientData$url_search

  # Construire l'URL complète
  current_url <- paste0(protocol, "//", hostname)

  if (port != "") {
    current_url <- paste0(current_url, ":", port)
  }

  current_url <- paste0(current_url, pathname, search)

  # Afficher l'URL complète
  return(current_url)
}
