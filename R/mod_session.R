
# MAIN --------------------------------------------------------------------

mod_session_ui <- function(id) {
  ns <- NS(id)
  tagList(
    mod_session_select_ui(ns("select")),
    mod_session_settings_ui(ns("settings"))
  )
}

mod_session_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize global triggers
    trigger_init("active_session")
    trigger_init("session_list")
    trigger_init("session_settings")
    
    # Modules
    mod_session_select_server("select")
    mod_session_settings_server("settings")
    
  })
}

# SESSION SELECT ----------------------------------------------------------

mod_session_select_ui <- function(id) {
  ns <- NS(id)
  card_primary(
    title = "Session Select",
    icon = icon("folder-open"),
    tags$strong("Active session:"),
    verbatimTextOutput(ns("active_session")),
    tags$hr(),
    selectInput(
      inputId = ns("session"),
      label = "Available sessions:",
      selected = session_get(),
      choices = session_list()
    ),
    footer = tagList(
      btn_primary(
        inputId = ns("btn_activate"),
        label = "Activate Session",
        icon = icon("check")
      ),
      btn_primary(
        inputId = ns("btn_delete"),
        label = "Delete Session",
        icon = icon("trash")
      ),
      btn_primary(
        inputId = ns("btn_create"),
        label = "Create New Session",
        icon = icon("plus")
      )
    )
  )
}

mod_session_select_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Active session label
    output$active_session <- renderText({
      trigger_watch("active_session")
      session_get()
    })
    
    on_trigger("active_session", {
      updateSelectInput(
        inputId = "session",
        selected = session_get()
      )
    })
    
    on_trigger("session_list", {
      updateSelectInput(
        inputId = "session",
        choices = session_list(),
        selected = session_get()
      )
    })
    
    # Activate session
    observeEvent(input$btn_activate, {
      tryCatch(
        expr = {
          popup_loading("Activating session...")
          activate_session(input$session)
          trigger_press("active_session")
          popup_success("Session activated!")
        },
        error = popup_error()
      )
    })
    
    # Delete session
    observeEvent(input$btn_delete, {
      tryCatch(
        expr = {
          popup_loading("Deleting session...")
          delete_session(input$session)
          trigger_press("session_list")
          popup_success("Session deleted!")
        },
        error = popup_error()
      )
    })
    
    # Create session
    observeEvent(input$btn_create, {
      popup_text(
        title = "Create Session",
        inputPlaceholder = "Session Name",
        confirmButtonText = "Create Session",
        callback = function(res) {
          if (isFALSE(res)) return()
          tryCatch(
            expr = {
              popup_loading("Creating session...")
              create_session(res)
              trigger_press("active_session")
              trigger_press("session_list")
              popup_success("Session created!")
            },
            error = popup_error()
          )
        }
      )
    })
    
  })
}

# SESSION SETTINGS --------------------------------------------------------

#' @importFrom shinyFiles shinyDirButton
mod_session_settings_ui <- function(id) {
  ns <- NS(id)
  card_primary(
    title = "Session Settings",
    icon = icon("folder-tree"),
    checkboxInput(
      inputId = ns("no_proxy"),
      label = "Disable Proxy?",
      value = settings_get("no_proxy")
    ),
    tags$hr(),
    shinyDirButton(
      id = ns("storage_dir"),
      label = "Select Storage Directory",
      title = "Select Storage Directory",
      icon = icon("folder-open")
    ),
    tags$br(), tags$br(),
    tags$strong("Current storage:"),
    verbatimTextOutput(ns("storage_dir")),
    footer = tagList(
      btn_primary(
        inputId = ns("btn_defaults"),
        label = "Use Default Settings",
        icon = icon("sync")
      ),
      btn_primary(
        inputId = ns("btn_save"),
        label = "Save Settings",
        icon = icon("save")
      )
    )
  )
}

#' @importFrom shinyFiles parseDirPath shinyDirChoose
mod_session_settings_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Update UI from triggers
    rv_path <- reactiveVal(settings_get("storage"))
    on_trigger("active_session", trigger_press("session_settings"))
    on_trigger("session_settings", {
      updateCheckboxInput(inputId = "no_proxy", value = settings_get("no_proxy"))
      rv_path(settings_get("storage"))
    })
    
    # Storage input backend
    shinyDirChoose(
      input = input,
      id = "storage_dir",
      roots = list_user_volumes()
    )
    
    # Handle storage input
    observeEvent(input$storage_dir, {
      req(is.list(input$storage_dir))
      path <- parseDirPath(roots = list_user_volumes(), selection = input$storage_dir)
      rv_path(path)
    })
    
    # Storage label
    output$storage_dir <- renderText(rv_path())
    
    # Apply default settings
    observeEvent(input$btn_defaults, {
      tryCatch(
        expr = {
          popup_loading("Applying default settings...")
          settings_set(no_proxy = TRUE, storage = package_storage())
          trigger_press("session_settings")
          popup_success("Default settings applied!")
        },
        error = popup_error()
      )
    })
    
    # Save settings
    observeEvent(input$btn_save, {
      tryCatch(
        expr = {
          popup_loading("Saving session settings...")
          settings_set(no_proxy = input$no_proxy, storage = rv_path())
          trigger_press("session_settings")
          popup_success("Session settings updated!")
        },
        error = popup_error()
      )
    })
    
  })
}
