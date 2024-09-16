#' Shiny Alert with Customization
#'
#' Displays a Shiny alert with an optional image, title, text, and customizable button color.
#'
#' @param title The title of the alert.
#' @param text The text content displayed within the alert.
#' @param type The type of alert ("info", "success", "warning", "error").
#' @param html Whether to render the `text` as HTML (`TRUE`) or plain text (`FALSE`).
#' @param imageUrl The URL of the image to display within the alert. Defaults to the package logo.
#' @param confirmButtonCol The color of the confirm button.
#'
#' @export
th_shinyalert <-
  function(title = "",
           text = "",
           type = "info",
           confirmButtonCol = "#013DFF",
           html = FALSE,
           imageUrl = NULL) {
    if (is.null(imageUrl)) {
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



#' Prepare the Application Header
#'
#' Creates a custom header for a Shiny application, including the application logo and links.
#'
#' @return A `dashboardHeader` object from `bs4Dash` containing the custom application header.
#'
#' @export
prepare_app_header <- function() {
  app_logo_circle <- "https://raw.githubusercontent.com/thaink2/thaink2publicimages/main/thaink2_logo_circle.png"
  bs4Dash::bs4DashNavbar(
    skin = "light",
    title = bs4Dash::bs4DashBrand(
      title = bs4Dash::bs4DashBrand(title = "Database Manager", image = app_logo_circle)
    ),
    rightUi = shiny::tagList(
      shiny::tags$li(
        class = "nav-item dropdown",
        shiny::tags$a(
          href = "https://github.com/thaink2/th2dbm.git",
          target = "_blank",
          class = "nav-link btn-primary",
          shiny::icon("github")
        )
      )
    )
  )
}

# Fonction pour obtenir la structure de la table
#'
#' @export
get_table_structure <- function(con, table_name) {
  DBI::dbGetQuery(con, paste0("SELECT column_name, data_type FROM information_schema.columns WHERE table_name = '", table_name, "'"))
}

# Fonction pour obtenir la clé primaire de la table
#'
#' @export
get_primary_key <- function(con, table_name) {
  result <- dbGetQuery(con, paste0("
    SELECT a.attname
    FROM   pg_index i
    JOIN   pg_attribute a ON a.attrelid = i.indrelid
                         AND a.attnum = ANY(i.indkey)
    WHERE  i.indrelid = '", table_name, "'::regclass
    AND    i.indisprimary;
  "))
  if (nrow(result) > 0) {
    return(result$attname[1])
  } else {
    return(NULL)
  }
}

#' Set User Default Configurations
#'
#' Sets default configurations for a user, including target language and time zone.
#' Saves the configurations to a JSON file.
#'
#' @param target_lang The target language for translations.
#' @param time_zone The user's time zone.
#' @param configs_dir The directory to store the configuration file.
#'
#' @return A list containing the user configurations.
#'
#' @export
set_user_default_configs <- function(target_lang = "en", time_zone = "CET", configs_dir = "../home/configs/") {
  user_configs <- list(target_lang = target_lang, time_zone = time_zone, update_time = Sys.time())
  if (dir.exists(configs_dir) == FALSE) {
    dir.create(configs_dir, recursive = TRUE)
  }
  user_name <- strsplit(Sys.getenv("SHINYPROXY_USERNAME"), "@") %>%
    unlist() %>%
    head(1) %>%
    gsub("\\.", "_", .)
  configs_file <- paste0(configs_dir, user_name, "_fairviewer_configs.json")
  user_configs %>%
    RJSONIO::toJSON() %>%
    write(file = configs_file)
  return(user_configs)
}


#' Load User Default Configurations
#'
#' Loads user default configurations from a JSON file. If the file doesn't exist, it creates one with default values.
#'
#' @param configs_dir The directory where the configuration file is stored.
#'
#' @return A list containing the loaded user configurations.
#'
#' @export
load_user_default_configs <- function(configs_dir = "../home/configs/") {
  user_name <- strsplit(Sys.getenv("SHINYPROXY_USERNAME"), "@") %>%
    unlist() %>%
    head(1) %>%
    gsub("\\.", "_", .)
  configs_file <- paste0(configs_dir, user_name, "_fairviewer_configs.json")
  if (file.exists(configs_file) == FALSE) set_user_default_configs()
  user_configs <- configs_file %>%
    RJSONIO::fromJSON()
  return(user_configs)
}

#' Load i18n Translator
#'
#' Loads the i18n translator for internationalization based on the user's default language configuration.
#'
#' @return An `i18n` translator object.
#'
#' @export
load_i18n_translator <- function() {
  # target_language <- golem::get_golem_options(which = "target_language" )
  target_language <- load_user_default_configs()$target_lang
  i18n <- shiny.i18n::Translator$new(translation_csvs_path = system.file("translator", package = "th2dbm"))
  i18n$set_translation_language(target_language)
  return(i18n)
}

#' Drop List Columns
#'
#' Removes columns of type 'list' from a data.table or data.frame.
#'
#' @param x The data.table or data.frame to process.
#'
#' @return The modified data.table or data.frame with list columns removed.
#'
#' @export
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

#' Correct the Date
#'
#' Converts a numeric timestamp to a readable date or returns the input date if it's not numeric.
#'
#' @param current_date The current date, which can be a numeric timestamp or a date string.
#'
#' @return The corrected date in a readable format if the input is numeric, otherwise the original input.
#'
#' @export
correct_date <- function(current_date) {
  return(ifelse(is.na(as.numeric(current_date)),
    current_date,
    paste(as.POSIXct(as.numeric(current_date)))
  ))
}

#' Generate a Unique Identifier
#'
#' Generates a unique identifier using a specified prefix and a UUID.
#'
#' @param prefix A prefix to add to the generated identifier. Defaults to "UserEntry".
#'
#' @return A unique identifier as a character string.
#'
#' @export
generateID <- function(prefix = "id") {
  sprintf("%s_%s", prefix, uuid::UUIDgenerate())
}

#' Create a Helper File for Refresh
#'
#' Creates a helper file to store the timestamp of the last refresh for a specific module.
#'
#' @param mod_id The identifier of the module.
#' @param refresh_folder The folder where the refresh file will be stored.
#'
#' @return The path to the created refresh file.
#'
#' @export
create_refresh_helper_file <- function(mod_id = NULL, refresh_folder = "./refresh/") {
  if (!dir.exists(refresh_folder)) dir.create(refresh_folder, recursive = TRUE)
  mod_refresh_file <- paste0(refresh_folder, "refresher_", mod_id, ".rds")
  Sys.time() %>%
    saveRDS(mod_refresh_file)
  return(mod_refresh_file)
}

#' Remove Shiny Inputs
#'
#' Dynamically removes specified Shiny input elements by their identifier.
#'
#' @param id The identifier of the Shiny input elements to remove.
#' @param .input The Shiny `.input` object containing the inputs.
#'
#' @export
remove_shiny_inputs <- function(id, .input) {
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
}


#' Check email format and return formatted name
#'
#' This function takes an email as input and checks if it's in the format firstname.lastname@domain.
#' If so, it returns a string in the format "Firstname Lastname" with the first letters capitalized.
#' Otherwise, it returns the original email.
#'
#' @param email The email address to check.
#'
#' @return A string containing "Firstname Lastname" if the email is in the format
#' firstname.lastname@domain, otherwise the original email.
#'
#' @export
verifier_format_email <- function(email) {
  # Utilise une expression reguliere pour verifier le format general prenom.nom@domaine
  if (grepl("^[a-zA-Z]+\\.[a-zA-Z]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", email)) {
    # Extraire le prenom et le nom a partir de l'email
    parts <- strsplit(email, "[@.]")[[1]] # Separe par '@' et '.'
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

#' Get the current URL of the Shiny application
#'
#' This function retrieves the current URL of the Shiny application, including the protocol, hostname,
#' pathname, and search parameters.
#'
#' @param session The Shiny session object (optional).
#' @param port The port number of the Shiny application (default: 3838).
#'
#' @return The complete URL of the Shiny application as a string.
#'
#' @export
get_current_url <- function(session = NULL, port = 3838) {
  protocol <- session$clientData$url_protocol
  hostname <- session$clientData$url_hostname
  pathname <- session$clientData$url_pathname
  search <- session$clientData$url_search

  # Construire l'URL complete
  current_url <- paste0(protocol, "//", hostname)

  if (port != "") {
    current_url <- paste0(current_url, ":", port)
  }

  current_url <- paste0(current_url, pathname, search)

  # Afficher l'URL complete
  return(current_url)
}

#' Read data from inputfile (CSV)
#' @author Farid Azouaou
#' @param input_file input file
#' @return tbl_df object

ghred_tisefka_csv <- function(input_file = NULL, delim = ",") {
  tisefka_csv <- readr::read_delim(
    file = input_file,
    # header = TRUE,
    delim = delim,
    na = c("", "NA", "ND", " ", "-", ".", "#")
  )
  tisefka_csv <- data.frame(tisefka_csv, check.names = FALSE)
  return(tisefka_csv)
}


#' Read data from inputfile (Excel)
#' @author Farid Azouaou
#' @param input_file inputfile object
#' @param tawriqt excel sheet name
#' @return tbl_df object

ghred_tisefka_excel <- function(input_file = NULL, tawriqt = NULL) {
  tisefka_excel <- readxl::read_excel(
    path = input_file,
    # header = TRUE,
    # delim = ",",
    sheet = tawriqt,
    na = c("", "NA", "ND", " ", "-", ".", "#")
  )
  tisefka_excel <- data.frame(tisefka_excel, check.names = FALSE)
  return(tisefka_excel)
}

#' Read data main function
#' @author Farid Azouaou
#' @param input_file input file
#' @param tala data source (CSV/Excel/Database)
#' @param tawriqt excel sheet name
#' @return tbl_df object
#' @export

ghred_tisefka_aqerru <- function(input_file = NULL, tala = NULL, tawriqt = NULL, delim = NULL) {
  if (tala == "csv") {
    tisefka <- ghred_tisefka_csv(input_file = input_file, delim = delim)
  }
  if (tala == "xlsx") {
    tisefka <- ghred_tisefka_excel(input_file = input_file, tawriqt = tawriqt)
  }
  if (tala == "txt") {
    tisefka <- read.table(file = input_file, header = TRUE, sep = "\t", quote = "")
  }

  tisefka <- tisefka %>%
    janitor::remove_empty(which = c("cols")) %>%
    janitor::remove_constant()
  return(tisefka)
}
