
# MAIN --------------------------------------------------------------------

mod_scrape_ui <- function(id) {
  ns <- NS(id)
  tagList(
    mod_scrape_settings_ui(ns("settings")),
    mod_scrape_job_ui(ns("job"))
  )
}

mod_scrape_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Modules
    mod_scrape_settings_server("settings")
    mod_scrape_job_server("job")
    
  })
}

# SCRAPE SETTINGS ---------------------------------------------------------

mod_scrape_settings_ui <- function(id) {
  ns <- NS(id)
  card_primary(
    title = "Scrape Settings",
    icon = icon("magnifying-glass"),
    textInput(
      inputId = ns("url"),
      label = "Base URL:",
      settings_get("url")
    ),
    footer = tagList(
      btn_primary(
        inputId = ns("btn_save"),
        label = "Save Settings",
        icon = icon("save")
      )
    )
  )
}

mod_scrape_settings_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Session callback
    on("active_session", {
      updateTextInput(inputId = "url", value = settings_get("url"))
    })
    
    # Save settings
    observeEvent(input$btn_save, {
      tryCatch(
        expr = {
          popup_loading("Saving scrape settings...")
          settings_set(url = input$url)
          scrape_base_page()
          trigger_press("session_settings")
          popup_success("Scrape settings saved!")
        },
        error = popup_error()
      )
    })
    
  })
}

# SCRAPE JOB --------------------------------------------------------------

mod_scrape_job_ui <- function(id) {
  ns <- NS(id)
  card_primary(
    title = "Scrape Job",
    icon = icon("magnifying-glass"),
    uiOutput(ns("ui_page")),
    footer = tagList(
      btn_primary(
        inputId = ns("btn_save"),
        label = "Save Settings",
        icon = icon("save")
      )
    )
  )
}

mod_scrape_job_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$ui_page <- renderUI({
      trigger_watch("session_settings")
      tags$iframe(src = settings_get("url"), width = "100%", height = "500px")
    })
    
  })
}
