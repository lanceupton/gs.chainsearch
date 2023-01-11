
# MAIN --------------------------------------------------------------------

mod_proxy_ui <- function(id) {
  ns <- NS(id)
  tagList(
    mod_proxy_header_ui(ns("header")),
    mod_proxy_table_ui(ns("table")),
    mod_proxy_blacklist_ui(ns("blacklist"))
  )
}

mod_proxy_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize global triggers
    trigger_init("proxy_blacklist")
    
    # Modules
    mod_proxy_header_server("header")
    mod_proxy_table_server("table")
    mod_proxy_blacklist_server("blacklist")
    
  })
}

# PROXY TABLE -------------------------------------------------------------

#' @importFrom bs4Dash bs4Callout
mod_proxy_header_ui <- function(id) {
  ns <- NS(id)
  bs4Callout(
    title = "Heads up!",
    status = "danger",
    width = NULL,
    id = ns("callout"),
    "Proxy usage is disabled for this session.",
    "It can be enabled in the 'Session Settings' tab."
  )
}

mod_proxy_header_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Toggle UI
    no_proxy <- reactiveVal(settings_get("no_proxy"))
    on_trigger("session_settings", no_proxy(settings_get("no_proxy")))
    bind_visibility("callout", no_proxy)
    
  })
}

# PROXY TABLE -------------------------------------------------------------

#' @importFrom DT DTOutput
#' @importFrom shinyjs disabled
mod_proxy_table_ui <- function(id) {
  ns <- NS(id)
  card_primary(
    title = "Proxy Table",
    icon = icon("table"),
    DTOutput(ns("dt_proxytab")),
    footer = tagList(
      btn_primary(
        inputId = ns("btn_reload"),
        label = "Reload Proxy Table",
        icon = icon("sync")
      ),
      disabled(btn_primary(
        inputId = ns("btn_blacklist"),
        label = "Blacklist Selected Proxies",
        icon = icon("cancel")
      ))
    )
  )
}

#' @importFrom DT renderDT
mod_proxy_table_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Proxy table
    rv_proxytab <- reactiveVal(proxytab_get())
    output$dt_proxytab <- renderDT(dt_proxytab(rv_proxytab()))
    selected_rows <- reactive(input$dt_proxytab_rows_selected)
    
    # Toggle UI
    bind_state("btn_blacklist", selected_rows)
    
    # Reload proxy table
    observeEvent(input$btn_reload, {
      tryCatch(
        expr = {
          popup_loading("Reloading proxy table...")
          refresh_proxy_list(force = TRUE)
          rv_proxytab(proxytab_get())
          popup_success("Proxy table updated!")
        },
        error = popup_error()
      )
    })
    
    # Blacklist proxy
    observeEvent(input$btn_blacklist, {
      tryCatch(
        expr = {
          popup_loading("Blacklisting proxies...")
          # Selected proxy
          df <- rv_proxytab()
          rows <- as.list(df[selected_rows(), ])
          # Blacklist proxy
          blacklist_proxy(rows$ip, rows$port)
          # Update table
          tmp <- proxytab_get()
          rv_proxytab(tmp)
          # Trigger blacklist
          trigger_press("proxy_blacklist")
          popup_success("Proxies blacklisted!")
        },
        error = popup_error()
      )
    })
    
  })
}

# PROXY BLACKLIST ---------------------------------------------------------

#' @importFrom DT DTOutput
#' @importFrom shinyjs disabled
mod_proxy_blacklist_ui <- function(id) {
  ns <- NS(id)
  card_primary(
    title = "Proxy Blacklist",
    icon = icon("cancel"),
    DTOutput(ns("dt_proxytab")),
    footer = tagList(
      disabled(btn_primary(
        inputId = ns("btn_whitelist"),
        label = "Whitelist Selected Proxies",
        icon = icon("check")
      ))
    )
  )
}

#' @importFrom DT renderDT
mod_proxy_blacklist_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Proxy table
    rv_proxytab <- reactiveVal(proxybl_get())
    output$dt_proxytab <- renderDT(dt_proxybl(rv_proxytab()))
    selected_rows <- reactive(input$dt_proxytab_rows_selected)
    
    # Blacklist callback
    on_trigger("proxy_blacklist", rv_proxytab(proxybl_get()))
    
    # Toggle UI
    bind_state("btn_whitelist", selected_rows)
    
    # Whitelist proxy
    observeEvent(input$btn_whitelist, {
      tryCatch(
        expr = {
          popup_loading("Whitelisting proxies...")
          # Selected proxy
          df <- rv_proxytab()
          row <- as.list(df[selected_rows(), ])
          # Whitelist proxy
          whitelist_proxy(row$ip, row$port)
          # Update table
          tmp <- proxybl_get()
          rv_proxytab(tmp)
          popup_success("Proxies whitelisted!")
        },
        error = popup_error()
      )
    })
    
  })
}
