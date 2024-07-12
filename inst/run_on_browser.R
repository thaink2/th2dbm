# system.file("app_starter", package="th2dbm") |>
#   fs::dir_copy("myapp", overwrite = TRUE)

# system.file("examples", "01_hello", package="shiny") |>
#   fs::dir_copy("myapp", overwrite = TRUE)
shinylive::export("myapp", "site")
httpuv::runStaticServer("site")
