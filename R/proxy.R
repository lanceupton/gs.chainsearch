
# USER-FACING -------------------------------------------------------------

#' Refresh the Proxy List.
#' 
#' Fetch a new proxy table via the API provided by Geonode (see
#'   https://geonode.com/free-proxy-list/) and update the proxy list used for
#'   scraping.
#' 
#' @param force (Default FALSE); This function will do nothing if the proxy
#'   table has been refreshed in the past hour, unless `force = TRUE`.
#'   
refresh_proxy_list <- function(force = FALSE) {
  if (isFALSE(force) && proxy_table_recent()) {
    LOG_MSG("Proxy table is fresh!", type = "success")
    tab <- proxytab_read()
    tab <- filter_proxy_table(tab)
    proxytab_set(tab)
    return()
  }
  LOG_MSG("Refreshing proxy table...")
  # Fetch new table
  tab <- fetch_proxy_table()
  # Remove blacklisted entries
  tab <- filter_proxy_table(tab)
  # Update active proxylist
  proxytab_set(tab)
  proxytab_write(tab)
}

# TRACKING ----------------------------------------------------------------

# Proxy table
proxytab_file <- function() {
  package_path("proxy_table.csv")
}
proxytab_read <- function() {
  file <- proxytab_file()
  if (file.exists(file)) read_csv(file, colClasses = list(character = "port"))
}
proxytab_write <- function(x) {
  file <- proxytab_file()
  write_csv(x, file)
}
#' @importFrom shiny shinyOptions
proxytab_set <- function(tab) {
  if (is.null(tab)) return()
  shinyOptions(GS_CHAINSEARCH_PROXYTAB = tab)
}
#' @importFrom shiny getShinyOption
proxytab_get <- function() {
  getShinyOption("GS_CHAINSEARCH_PROXYTAB") %||% proxytab_blank()
}

# Proxy blacklist
proxybl_file <- function() {
  package_path("proxy_blacklist.csv")
}
proxybl_read <- function() {
  file <- proxybl_file()
  if (file.exists(file)) read_csv(file, colClasses = list(character = "port"))
}
proxybl_write <- function(x) {
  file <- proxybl_file()
  write_csv(x, file)
}
#' @importFrom shiny shinyOptions
proxybl_set <- function(tab) {
  if (is.null(tab)) return()
  shinyOptions(GS_CHAINSEARCH_PROXYBL = tab)
}
#' @importFrom shiny getShinyOption
proxybl_get <- function() {
  getShinyOption("GS_CHAINSEARCH_PROXYBL") %||% proxybl_blank()
}
proxybl_add <- function(ip, port, method) {
  tmp <- proxybl_get()
  tmp <- rbind(tmp, data.frame(ip = ip, port = port, method = method))
  proxybl_set(tmp)
  proxybl_write(tmp)
}
proxybl_remove <- function(ip, port) {
  tmp <- proxybl_get()
  i1 <- which(tmp$ip %in% ip)
  i2 <- which(tmp$port %in% port)
  row <- intersect(i1, i2)
  tmp <- tmp[-row, ]
  proxybl_set(tmp)
  proxybl_write(tmp)
}

# Active proxy
proxy_set <- function() {
  tryCatch(
    expr = {
      tmp <- proxylist_sample()
      proxy <- paste0(tmp$ip, ":", tmp$port)
      LOG_MSG("proxy_set: ", proxy)
      Sys.setenv(GS_CHAINSEARCH_PROXY = proxy)
    },
    error = popup_error()
  )
}
proxy_get <- function() {
  Sys.getenv("GS_CHAINSEARCH_PROXY")
}

# API ---------------------------------------------------------------------

#' Blacklist a Proxy.
#' 
#' Blacklist an individual proxy.
#' 
blacklist_proxy <- function(ip, port, method = c("manual", "automatic")) {
  method <- match.arg(method)
  # Add proxy to blacklist
  proxybl_add(ip, port, method)
  # Filter proxy table
  tmp <- proxytab_get()
  tmp <- filter_proxy_table(tmp)
  proxytab_set(tmp)
  proxytab_write(tmp)
}

#' Whitelist a Proxy.
#' 
#' Whitelist an individual proxy.
#' 
whitelist_proxy <- function(ip, port) {
  # Remove proxy from blacklist
  proxybl_remove(ip, port)
}

# UTILS -------------------------------------------------------------------

# Check if the proxy table is fresh
# i.e., written in the past hour
proxy_table_recent <- function() {
  file <- proxytab_file()
  if (!file.exists(file)) return(FALSE)
  now <- Sys.time()
  then <- file.info(file)$mtime
  diff <- as.numeric(difftime(now, then, units = "hour"))
  return(diff < 1)
}

# Fetch the proxy table
#' @importFrom jsonlite fromJSON
fetch_proxy_table <- function() {
  # TODO: Loop through fast, medium, slow until limit is attained
  url <- proxy_table_url(100, 1)
  tmp <- fromJSON(url)
  # Wrangle into data frame
  tmp <- lapply(tmp$data, unlist)
  tmp <- data.frame(tmp)
  # Apply classes
  tmp$latency <- as.numeric(tmp$latency)
  tmp$upTime <- as.numeric(tmp$upTime)
  return(tmp)
}

#' @importFrom urltools param_set
proxy_table_url <- function(limit, page = 1, speed = c("fast", "medium", "slow")) {
  speed <- match.arg(speed)
  url <- "https://proxylist.geonode.com/api/proxy-list"
  param_set(url, "limit", limit) |>
    param_set("page", page) |>
    param_set("google", "true") |>
    param_set("speed", speed)
}

# Filter blacklisted entries from proxy table
filter_proxy_table <- function(tab) {
  blacklist <- proxybl_get()
  i1 <- which(tab$ip %in% blacklist$ip)
  i2 <- which(tab$port %in% blacklist$port)
  omit <- intersect(i1, i2)
  if (length(omit) > 0) {
    LOG_MSG("Omitting ", length(omit), " blacklisted proxies...", type = "warning")
    tab <- tab[-omit, ]
  }
  return(tab)
}

# Sample random proxy from proxy table
proxylist_sample <- function() {
  tmp <- proxytab_get()
  if (length(tmp) == 0) stop("Proxy list is empty.") 
  row <- sample(nrow(tmp), 1)
  list(
    ip = tmp$ip[row],
    port = tmp$port[row]
  )
}

# UI ----------------------------------------------------------------------

dt_proxytab <- function(tab) {
  dt_minimal(
    data = tab[,c("ip", "port", "city", "country", "latency", "upTime")],
    colnames = c("IP", "Port", "City", "Country", "Latency (ms)", "Uptime"),
    selection = "multiple"
  )
}

proxytab_blank <- function() {
  data.frame(
    ip = numeric(0),
    port = character(0),
    city = character(0),
    country = character(0),
    latency = numeric(0),
    upTime = numeric(0)
  )
}

dt_proxybl <- function(tab) {
  dt_minimal(
    data = tab[,c("ip", "port", "method")],
    colnames = c("IP", "Port", "Method"),
    selection = "multiple"
  )
}

proxybl_blank <- function() {
  data.frame(
    ip = numeric(0),
    port = character(0),
    method = character(0)
  )
}
