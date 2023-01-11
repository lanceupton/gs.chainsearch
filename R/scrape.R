
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
  # Save links
  links <- html_elements(page, "link")
  for (href in html_attr(links, "href")) {
    if (is.na(href) || is_url(href)) next
    LOG_MSG("Downloading ", href)
    web_file <- paste0(url, href)
    local_file <- session_path(href)
    dir_create(dirname(local_file))
    download.file(web_file, local_file, quiet = TRUE)
  }
  # Save scripts
  scripts <- html_elements(page, "script")
  for (href in html_attr(scripts, "src")) {
    if (is.na(href) || is_url(href)) next
    LOG_MSG("Downloading ", href)
    web_file <- paste0(url, href)
    local_file <- session_path(href)
    dir_create(dirname(local_file))
    download.file(web_file, local_file, quiet = TRUE)
  }
  # Save images
  imgs <- html_elements(page, "img")
  for (href in html_attr(imgs, "src")) {
    if (is.na(href) || is_url(href)) next
    LOG_MSG("Downloading ", href)
    web_file <- paste0(url, href)
    local_file <- session_path(href)
    dir_create(dirname(local_file))
    download.file(web_file, local_file, quiet = TRUE)
  }
  # Save page
  writeLines(as.character(page), session_path(paste0("page", page_index), ext = "html"))
}

#' @importFrom urltools url_parse
is_url <- function(src) {
  tmp <- url_parse(src)
  !is.na(tmp$scheme)
}
