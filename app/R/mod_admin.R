# R/mod_admin.R - Admin panel module
# Admin-only data management with full edit/delete capabilities

# ---------------------------
# UI
# ---------------------------

adminUI <- function(id) {

  ns <- NS(id)

  nav_panel(
    title = "Admin",
    icon = icon("shield-halved"),
    uiOutput(ns("panel_ui"))
  )
}

# ---------------------------
# Server
# ---------------------------

adminServer <- function(id, pool, is_admin, current_user, data_changed) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Admin panel UI - shows lock screen for non-admins
    output$panel_ui <- renderUI({
      if (!is_admin()) {
        return(
          div(class = "text-center py-5",
              icon("lock", class = "fa-3x text-muted mb-3"),
              h4("Admin Access Required"),
              p(class = "text-muted", "This section is only available to administrators."))
        )
      }

      # Admin panel content
      tagList(
        layout_columns(
          col_widths = c(12),

          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              span(icon("shield-halved"), " Admin Data Management"),
              span(class = "badge bg-danger", "Admin Only")
            ),
            card_body(
              p(class = "text-muted",
                "As an admin, you can edit or delete any entry. Use the edit/delete buttons ",
                "in the Actions column, or use the buttons below for bulk operations."),
              hr(),
              h5("All Database Entries"),
              DTOutput(ns("all_entries")),
              hr(),
              h5("Bulk Operations"),
              layout_column_wrap(
                width = 1/3,
                downloadButton(ns("export"), "Export All Data", class = "btn-outline-primary"),
                actionButton(ns("refresh"), "Refresh Table", class = "btn-outline-secondary", icon = icon("refresh"))
              )
            )
          )
        )
      )
    })

    # Admin: all entries table with full edit/delete capabilities
    output$all_entries <- renderDT({
      input$refresh  # React to refresh button
      data_changed()

      if (!is_admin()) return(NULL)

      dat <- db_get_all_samples()
      if (nrow(dat) == 0) return(NULL)

      display <- dat %>%
        select(id, species, created_by, ph, organic_matter, texture_class, date, created_at) %>%
        mutate(date = as.character(date),
               created_at = as.character(created_at))

      # Add action buttons - these trigger global edit_entry/delete_entry inputs
      display$actions <- sapply(display$id, function(entry_id) {
        sprintf(
          "<button class=\"btn btn-sm btn-outline-primary me-1\" title=\"Edit entry %d\" aria-label=\"Edit entry %d\" onclick=\"Shiny.setInputValue('edit_entry', %d, {priority: 'event'})\"><i class=\"fa fa-edit\"></i></button><button class=\"btn btn-sm btn-outline-danger\" title=\"Delete entry %d\" aria-label=\"Delete entry %d\" onclick=\"Shiny.setInputValue('delete_entry', %d, {priority: 'event'})\"><i class=\"fa fa-trash\"></i></button>",
          entry_id, entry_id, entry_id, entry_id, entry_id, entry_id
        )
      })

      datatable(display,
                options = list(pageLength = 25, scrollX = TRUE, order = list(list(0, 'desc'))),
                rownames = FALSE,
                escape = FALSE,
                colnames = c("ID", "Species", "Created By", "pH", "OM %", "Texture", "Date", "Created At", "Actions"))
    })

    # Admin export
    output$export <- downloadHandler(
      filename = function() paste0("admin_export_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
      content = function(file) {
        if (is_admin()) {
          data <- db_get_all_samples()
          write.csv(data, file, row.names = FALSE)
          u <- current_user()
          if (!is.null(u)) {
            db_audit_log("admin_export", "soil_samples", NULL, u$user_uid, sprintf("Admin export: %d records", nrow(data)))
          }
        }
      }
    )
  })
}
