
page_file <- function(page_index) {
  name <- paste0("page", page_index, ".html")
  session_path(name)
}

scrape_base_page <- function() {
  LOG_MSG("scrape_base_page")
  scrape_page(settings_get("url"), 1)
}

#' @importFrom rvest html_attr html_elements read_html
#' @importFrom fs dir_create
scrape_page <- function(url, page_index) {
  page <- read_html(url)
  # Save page
  writeLines(as.character(page), session_path(paste0("page", page_index), ext = "html"))
  # The below works, but the content doesn't seem to load in the interface
  # when includeHTML is called
  # # Save links
  # links <- html_elements(page, "link")
  # for (href in html_attr(links, "href")) {
  #   copy_dependencies(href)
  # }
  # # Save scripts
  # scripts <- html_elements(page, "script")
  # for (href in html_attr(scripts, "src")) {
  #   copy_dependencies(href)
  # }
  # # Save images
  # imgs <- html_elements(page, "img")
  # for (href in html_attr(imgs, "src")) {
  #   copy_dependencies(href)
  # }
}

#' @importFrom urltools url_parse
is_url <- function(src) {
  tmp <- url_parse(src)
  !is.na(tmp$scheme)
}

#' @importFrom fs dir_create
#' @importFrom utils download.file
copy_dependencies <- function(href) {
  if (is.na(href) || is_url(href)) return()
  LOG_MSG("Downloading ", href)
  web_file <- paste0(url, href)
  local_file <- session_path(href)
  dir_create(dirname(local_file))
  download.file(web_file, local_file, quiet = TRUE)
}

#' @importFrom htmltools includeHTML
base_page_ui <- function() {
  file <- session_path(paste0("page1.html"))
  if (file.exists(file)) includeHTML(file)
}

dt_meta_inputs <- function(meta) {
  lst <- lapply(meta, rbind)
  df <- data.frame(do.call(rbind, lst), row.names = NULL)
  dt_minimal(
    data = df,
    editable = TRUE,
    selection = "multiple"
  )
}

meta_inputs <- function() {
  list(
    name = "",
    selector = "",
    type = "",
    attribute = ""
  )
}
