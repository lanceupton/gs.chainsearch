
# Clean environment and load package
golem::detach_all_attached()
rm(list = ls(all.names = TRUE))
golem::document_and_reload()

# Initialize working session
initialize_session()

### CONFIG

# (OPTIONAL) Update storage
# update_storage("...")

# Set active publication
use_publication(10244671870114417611)

Sys.setenv(https_proxy = "163.172.37.158:9741"
Sys.setenv(http_proxy = "163.172.37.158:9741")


library(httr)



res <- GET(
  url = "https://scholar.google.com/scholar?cites=10244671870114417611", 
  # user_agent(agent = my_ua),
  use_proxy(url = "163.172.37.158", port = 9741),
  config(ssl_verifypeer = 0L),
  verbose()
)



# Set path to selenium jar file
# use_selenium("C:/Selenium/selenium-server-standalone-3.9.1.jar")

# Start selenium server
# start_selenium()
# 
# settings <- list(
#   storage = "C:\\Users\\lance\\AppData\\Local/R/cache/R/gs.chainsearch",
#   pub_id = 10244671870114417611,
#   proxy = list(
#     ip = "34.120.118.219",
#     port = 80L
#   ),
#   selenium = list(
#     host = "127.0.0.1",
#     port = 4444L,
#     browser = "firefox",
#     chrome_version = "108.0.5359.22"
#   )
# )

 profile <- makeFirefoxProfile(
#   list(
#     network.proxy.type = 1,
#     network.proxy.socks = "127.0.0.1",
#     network.proxy.socks_port = 9050,
#     network.proxy.socks_version = 5
#   )
# )

# remDr <- remoteDriver(
#   remoteServerAddr = "localhost",
#   port = 4444L,
#   browserName = "firefox",
# )

library(RSelenium)

driver <- rsDriver(
  browser = "chrome",
  port = 4444L,
  chromever  = "108.0.5359.125",
  extraCapabilities = list(
    chromeOptions = list(args = "--proxy=14.139.242.7:80")
  )
)

driver <- rsDriver(
  remoteServerAddr = "localhost",
  port = 4444L,
  browser = "firefox",
  # needs chrome version for some reason???
  chromever = "108.0.5359.22",
  extraCapabilities = makeFirefoxProfile(list(
    "network.proxy.socks" =  "34.64.4.104",
    "network.proxy.socks_port" = 80L,
    "network.proxy.type" = 1L
  ))
)

driver$client$navigate("https://ipinfo.io")
driver$client$screenshot(display = TRUE)



# gs_page_url <- function(pub_id = pub_get()) {
#   url <- "https://scholar.google.com/scholar"
#   
#   
#   
#   ?cites=10244671870114417611
#   # &as_sdt=5,26&sciodt=0,26&hl=en
#   
#   # search blank - 2019 second page (920 results)
#   "https://scholar.google.com/scholar?start=20&hl=en&as_sdt=5,45&sciodt=0,45&as_yhi=2019&cites=10244671870114417611&scipsc="
#   
#   # 2019 - blank second page (824 results)
#   "https://scholar.google.com/scholar?start=20&hl=en&as_sdt=5,45&sciodt=0,45&as_ylo=2019&cites=10244671870114417611&scipsc="
#   
#   
#   
#   
#   
#   
#   wp <- httr::GET(
#     url = url,
#     httr::user_agent(agent = ua),
#     httr::use_proxy(url = ip, port = port),
#     httr::verbose(),
#     # query = list(render_js = "1")
#     render_js = "1"
#   )
#   
#   if (wp$status_code == 200) {
#     file_nm <- paste0(unlist(strsplit(x = unlist(strsplit(x = url, split = "cites="))[2], split = "&scipsc=")), "-", as.numeric(unlist(regmatches(x = url, m = regexpr(pattern = "[[:digit:]]+", text = url)))), ".html")
#     slp <- runif(n = 1, min = 1, max = 3)
#     print(paste("sleep", slp, "- download_success", url))
#     cat(httr::content(x = wp, as = "text", encoding = encoding), file = paste0(dir, "/", file_nm))
#     Sys.sleep(time = slp)
#   }
#   else {
#     # message("Page Load Failure: Build a better function for handling this!")
#     print(paste("PROXY_FAILURE -", url))
#     break
#   }
# }


