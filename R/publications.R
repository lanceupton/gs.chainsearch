
# FILESYSTEM --------------------------------------------------------------

# Marker file for active publication
pub_file <- function() {
  storage_path("publication")
}

# Update marker file
pub_write <- function(path) {
  file <- pub_file()
  writeLines(path, file)
}

# Read marker file
pub_read <- function() {
  file <- pub_file()
  if (file.exists(file)) readLines(file)
}

# Set active publication
pub_set <- function(id) {
  if (is.null(id)) return()
  LOG_MSG("pub_set: ", id)
  Sys.setenv(GS_CHAINSEARCH_PUBLICATION = id)
}

# Get active publication
pub_get <- function() {
  Sys.getenv("GS_CHAINSEARCH_PUBLICATION")
}

# List available publications
list_publications <- function() {
  list.dirs(storage_get(), full.names = FALSE)[-1]
}

# API ---------------------------------------------------------------------

publication_init <- function(pub_id) {
  if (pub_id %in% list_publications()) stop("Publication already exists!")
  LOG_MSG("publication_init: ", pub_id)
  dir.create(storage_path(pub_id))
}

publication_delete <- function(pub_id) {
  if (identical(pub_id, pub_get())) stop("You cannot delete the active publication!")
  unlink(storage_path(pub_id), recursive = TRUE)
}
