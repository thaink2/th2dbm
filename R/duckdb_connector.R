#' connect_to_duck_db
#' @export
connect_to_duck_db <- function(db_dir = NULL, read_only = FALSE){
  if(is.null(db_dir))db_dir <- ":memory:"
  db_con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_dir, read_only = read_only)
  return(db_con)
}
