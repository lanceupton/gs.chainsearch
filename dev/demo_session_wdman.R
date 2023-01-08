
library(wdman)
library(RSelenium)

remDr$closeall()
driver$stop()
rm(list = ls())

driver <- selenium(
  port = 4444L,
  chromever = NULL,
  geckover = "0.32.0",
  phantomver = NULL,
  check = FALSE
)

eCaps <- makeFirefoxProfile(
  list(
    network.proxy.socks = "62.109.31.192",
    network.proxy.socks_port = 20000L,
    network.proxy.type = 1L,
    network.proxy.socks_version = 5L,
    network.proxy.socks_remote_dns = TRUE
  )
)

driver2 <- rsDriver(
  port = 4444L,
  browser = "firefox",
  version = "latest",
  geckover = "latest",
  iedrver = NULL,
  phantomver = "2.1.1",
  verbose = TRUE,
  check = FALSE,
  extraCapabilities = eCaps
)

# remDr <- remoteDriver(
#   remoteServerAddr = "localhost",
#   port = 4444L,
#   browserName = "firefox",
#   extraCapabilities = eCaps 
# )

remDr$open()
remDr$navigate("http://ipinfo.io")
remDr$screenshot(display = TRUE)


http://localhost:4444/wd/hub/static/resource/hub.html


