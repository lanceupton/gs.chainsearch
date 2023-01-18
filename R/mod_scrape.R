
# MAIN --------------------------------------------------------------------

mod_scrape_ui <- function(id) {
  ns <- NS(id)
  tagList(
    mod_scrape_settings_ui(ns("settings")),
    fluidRow(
      column(width = 6, mod_scrape_inputs_ui(ns("inputs"))),
      column(width = 6, mod_scrape_preview_ui(ns("preview")))
    )
  )
}

mod_scrape_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Modules
    mod_scrape_settings_server("settings")
    mod_scrape_inputs_server("inputs")
    mod_scrape_preview_server("preview")
    
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

# SCRAPE INPUTS -----------------------------------------------------------

#' @importFrom DT DTOutput
mod_scrape_inputs_ui <- function(id) {
  ns <- NS(id)
  card_primary(
    title = "Scrape Inputs",
    icon = icon("table"),
    textInput(
      inputId = ns("container_xpath"),
      label = "Container XPath:",
      value = settings_get("container_xpath")
    ),
    textInput(
      inputId = ns("item_sel"),
      label = "Item CSS Selector:",
      value = settings_get("item_sel")
    ),
    tags$h3("Meta Inputs"),
    DTOutput(ns("meta_inputs")),
    footer = tagList(
      btn_primary(
        inputId = ns("btn_add"),
        label = "Add Row",
        icon = icon("plus")
      ),
      btn_primary(
        inputId = ns("btn_delete"),
        label = "Remove Row(s)",
        icon = icon("trash")
      ),
      btn_primary(
        inputId = ns("btn_save"),
        label = "Save Settings",
        icon = icon("save")
      )
    )
  )
}

#' @importFrom DT renderDT
mod_scrape_inputs_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Item meta inputs
    item_meta <- reactiveVal(settings_get("item_meta"))
    on_trigger("session_settings", item_meta(settings_get("item_meta")))
    
    # Table of inputs
    output$meta_inputs <- renderDT({
      meta <- item_meta()
      dt_meta_inputs(meta)
    })
    
    # Handle edits
    observeEvent(input$meta_inputs_cell_edit, {
      tmp <- item_meta()
      row <- input$meta_inputs_cell_edit$row
      col <- input$meta_inputs_cell_edit$col + 1
      val <- input$meta_inputs_cell_edit$value
      tmp[[row]][col] <- val
      item_meta(tmp)
    })
    
    # Add row
    observeEvent(input$btn_add, {
      tmp <- append(item_meta(), list(meta_inputs()))
      item_meta(tmp)
    })
    
    # Delete row
    observeEvent(input$btn_delete, {
      tmp <- item_meta()
      rows <- input$meta_inputs_rows_selected
      tmp <- tmp[-rows]
      item_meta(tmp)
    })
    
    # Save settings
    observeEvent(input$btn_save, {
      tryCatch(
        expr = {
          popup_loading("Saving scrape settings...")
          settings_set(
            container_xpath = input$container_xpath,
            item_sel = input$item_sel,
            item_meta = item_meta()
          )
          trigger_press("session_settings")
          popup_success("Scrape settings saved!")
        },
        error = popup_error()
      )
    })
    
  })
}

# SCRAPE PREVIEW ----------------------------------------------------------

mod_scrape_preview_ui <- function(id) {
  ns <- NS(id)
  card_primary(
    title = "Scrape Preview",
    icon = icon("magnifying-glass"),
    uiOutput(ns("ui_page"))
  )
}

mod_scrape_preview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$ui_page <- renderUI({
      trigger_watch("session_settings")
      base_page_ui()
    })
    
  })
}
