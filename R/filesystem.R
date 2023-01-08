
# USER-FACING -------------------------------------------------------------

#' Purge Package Directory.
#' 
#' Deletes all files in the package directory.
#'
#' @export
#'
purge_storage <- function() {
  unlink(package_storage(), recursive = TRUE)
}

#' Initialize a Working Session.
#' 
#' Restore the local environment to use methods from the package.
#'
#' @importFrom fs dir_create  
#' @export
#'
initialize_session <- function() {
  LOG_MSG("Initializing working session")
  # Create package storage
  dir_create(package_storage())
  # Restore active session
  session_set(session_read() %||% "default")
  # Create default config
  settings_init()
  # Restore proxy blacklist
  proxybl <- proxybl_read()
  proxybl_set(proxybl)
  # Refresh proxy list
  refresh_proxy_list()
  # Set new active proxy
  proxy_set()
  # Return invisible so no tags are included
  return(invisible())
}

activate_session <- function(session_id) {
  session_set(session_id)
  session_write(session_id)
}

#' @importFrom yaml read_yaml write_yaml
delete_session <- function(session_id) {
  file <- settings_file()
  tmp <- read_yaml(file)
  tmp <- tmp[-which(names(tmp) == session_id)]
  write_yaml(tmp, file)
}

create_session <- function(session_id) {
  settings_set(session_id = session_id)
  activate_session(session_id)
}

#' @importFrom yaml read_yaml
session_list <- function() {
  file <- settings_file()
  tmp <- read_yaml(file)
  names(tmp)
}

# TRACKING ----------------------------------------------------------------

# Name of active session
session_file <- function() {
  package_path("session")
}
session_read <- function() {
  file <- session_file()
  if (file.exists(file)) readLines(file)
}
session_write <- function(x) {
  file <- session_file()
  writeLines(x, file)
}
session_set <- function(session_id) {
  if (is.null(session_id)) return()
  Sys.setenv(GS_CHAINSEARCH_SESSION = session_id)
}
session_get <- function() {
  Sys.getenv("GS_CHAINSEARCH_SESSION", "default")
}

# Session settings
settings_file <- function() {
  package_path("settings.yaml")
}

settings_default <- function() {
  list(
    storage = package_storage(),
    url = "https://books.toscrape.com/"
  )
}

# Function to enforce that default config exists
# Merge in reverse in order to retain previous values
settings_init <- function() {
  file <- settings_file()
  if (file.exists(file)) {
    tmp <- read_yaml(file)
  } else {
    tmp <- NULL
  }
  lst <- merge(list(default = settings_default()), tmp)
  write_yaml(lst, file)
}

#' @importFrom config get
settings_get <- function(value = NULL) {
  get(
    value = value,
    config = session_get(),
    file = settings_file()
  )
}

#' @importFrom yaml read_yaml write_yaml
#' @importFrom config merge
settings_set <- function(..., session_id = session_get()) {
  file <- settings_file()
  tmp <- read_yaml(file)
  lst <- setNames(list(list(...)), session_id)
  lst2 <- merge(tmp, lst)
  write_yaml(lst2, file)
}

# UTILS -------------------------------------------------------------------

# List available fileystem volumes
#' @importFrom shinyFiles getVolumes
list_user_volumes <- function() {
  c(
    "Default Storage" = package_storage(),
    getVolumes()()
  )
}

# Default storage directory
#' @importFrom golem get_golem_name
#' @importFrom tools R_user_dir
package_storage <- function() {
  R_user_dir(package = get_golem_name(), which = "cache")
}

# Construct filepath within active storage
#' @importFrom fs path
package_path <- function(..., ext = "") {
  path(package_storage(), ..., ext = ext)
}
