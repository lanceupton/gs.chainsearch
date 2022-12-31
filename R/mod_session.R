
# MAIN --------------------------------------------------------------------

mod_session_ui <- function(id) {
  ns <- NS(id)
  tagList(
    mod_session_settings_ui(ns("settings")),
    mod_session_publications_ui(ns("publications"))
  )
}

mod_session_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize global triggers
    trigger_init("session_settings")
    trigger_init("publication_list")
    trigger_init("active_publication")
    
    # Modules
    mod_session_settings_server("settings")
    mod_session_publications_server("publications")
    
  })
}

# SESSION SETTINGS --------------------------------------------------------

#' @importFrom shinyFiles shinyDirButton
mod_session_settings_ui <- function(id) {
  ns <- NS(id)
  card_primary(
    title = "Session Settings",
    icon = icon("folder-tree"),
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
    
    rv_path <- reactiveVal(storage_get())
    on_trigger("session_settings", rv_path(storage_get()))
    
    shinyDirChoose(
      input = input,
      id = "storage_dir",
      roots = list_user_volumes()
    )
    
    observeEvent(input$storage_dir, {
      req(is.list(input$storage_dir))
      path <- parseDirPath(roots = list_user_volumes(), selection = input$storage_dir)
      rv_path(path)
    })
    
    output$storage_dir <- renderText(rv_path())
    
    # Revert to default settings
    observeEvent(input$btn_defaults, {
      tryCatch(
        expr = {
          popup_loading("Applying default settings...")
          storage_update(storage_default())
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
          storage_update(rv_path())
          trigger_press("session_settings")
          popup_success("Session settings updated!")
        },
        error = popup_error()
      )
    })
    
  })
}

# PUBLICATION SETTINGS ----------------------------------------------------

#' @importFrom shinyjs disabled
mod_session_publications_ui <- function(id) {
  ns <- NS(id)
  card_primary(
    title = "Publication Settings",
    icon = icon("book-open"),
    column(
      width = 4,
      selectInput(
        inputId = ns("pub_id"),
        label = "Available publications:",
        choices = list_publications(),
        selected = pub_get()
      )
    ),
    footer = tagList(
      btn_primary(
        inputId = ns("btn_create"),
        label = "Create New Publication",
        icon = icon("add")
      ),
      disabled(btn_primary(
        inputId = ns("btn_activate"),
        label = "Activate Selected Publication",
        icon = icon("check")
      )),
      disabled(btn_primary(
        inputId = ns("btn_delete"),
        label = "Delete Selected Publication",
        icon = icon("trash")
      ))
    )
  )
}

mod_session_publications_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Session settings callback
    on_trigger("session_settings", trigger_press("publication_list"))
    
    # Publication list callback
    on_trigger("publication_list", {
      updateSelectInput(
        inputId = "pub_id",
        choices = list_publications(),
        selected = pub_get()
      )
    })
    
    # Conduct selected publication
    selected_pub <- reactive(input$pub_id)
    
    # Toggle UI
    bind_state(c("btn_activate", "btn_delete"), selected_pub)
    
    # Create publication
    rv_callback <- reactiveVal()
    observeEvent(input$btn_create, {
      popup_text(
        title = "Create a Publication",
        inputValue = "", 
        inputPlaceholder = "publication ID",
        confirmButtonText = "Create Publication",
        callback = function(pub_id) {
          if (isFALSE(pub_id)) return()
          tryCatch(
            expr = {
              popup_loading("Creating publication...")
              publication_init(pub_id)
              pub_set(pub_id)
              pub_write(pub_id)
              trigger_press("publication_list")
              popup_success("Publication created!")
            },
            error = popup_error()
          )
        }
      )
    })
    
    # Activate publication
    observeEvent(input$btn_activate, {
      popup_loading("Activating publication...")
      pub_set(selected_pub())
      pub_write(selected_pub())
      trigger_press("active_publication")
      popup_success("Publication activated!")
    })
    
    # Delete publication
    observeEvent(input$btn_delete, {
      popup_confirm(
        title = "Delete Publication?",
        text = paste0("Publication ID: ", selected_pub()),
        callback = function(confirm) {
          if (isFALSE(confirm)) return()
          tryCatch(
            expr = {
              popup_loading("Deleting publication...")
              publication_delete(selected_pub())
              trigger_press("publication_list")
              popup_success("Publication deleted!")
            },
            error = popup_error()
          )
        }
      )
    })
    
  })
}
