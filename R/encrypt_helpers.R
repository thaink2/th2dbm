#' Convertir des Chaînes Hexadécimales en Vecteurs Raw
#'
#' Cette fonction prend une chaîne hexadécimale et la convertit en un vecteur de type raw.
#'
#' @param hex Une chaîne de caractères représentant des données hexadécimales.
#' @return Un vecteur de type raw correspondant aux données hexadécimales entrées.
#' @export
hex2raw <- function(hex) {
  sapply(seq(from = 1, to = nchar(hex), by = 2), function(i) as.raw(strtoi(substr(hex, i, i + 1), base = 16)))
}

#' Chiffrer une Colonne
#'
#' Chiffre les valeurs d'une colonne en utilisant une clé de chiffrement spécifiée ou la clé par défaut de l'environnement.
#'
#' @param column Colonne de données à chiffrer.
#' @param key Clé de chiffrement hexadécimale utilisée pour le chiffrement. Si non spécifiée, utilise `ENCRYPT_PASS` de l'environnement.
#' @return Une colonne avec les valeurs chiffrées.
#' @export
encrypt_column <- function(column, key = Sys.getenv("ENCRYPT_PASS")) {
  raw_key <- hex2raw(key)
  sapply(column, function(x) {
    if (is.na(x)) {
      x
    } else {
      nonce <- sodium::random(24)
      cipher <- sodium::data_encrypt(charToRaw(x), raw_key, nonce)
      paste(sodium::bin2hex(nonce), sodium::bin2hex(cipher), sep = ".")
    }
  })
}

#' Déchiffrer une Colonne
#'
#' Déchiffre les valeurs d'une colonne en utilisant une clé de déchiffrement spécifiée ou la clé par défaut de l'environnement.
#'
#' @param column Colonne de données à déchiffrer.
#' @param key Clé de déchiffrement hexadécimale utilisée pour le déchiffrement. Si non spécifiée, utilise `ENCRYPT_PASS` de l'environnement.
#' @return Une colonne avec les valeurs déchiffrées.
#' @export
decrypt_column <- function(column, key = Sys.getenv("ENCRYPT_PASS")) {
  raw_key <- hex2raw(key)

  sapply(column, function(x) {
    if (is.na(x)) {
      x
    } else {
      parts <- strsplit(x, "\\.")[[1]]
      nonce <- sodium::hex2bin(parts[1])
      cipher <- sodium::hex2bin(parts[2])
      decrypted_raw <- sodium::data_decrypt(cipher, raw_key, nonce)
      rawToChar(decrypted_raw)
    }
  })
}
