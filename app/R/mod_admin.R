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
                "As an admin, you can edit, delete, or flag any entry. Use the action buttons ",
                "in the Actions column, or use the buttons below for bulk operations."),
              hr(),
              # Filter controls
              div(class = "d-flex align-items-center gap-3 mb-3",
                  h5("All Database Entries", class = "mb-0"),
                  checkboxInput(ns("filter_flagged"),
                                span(icon("flag"), " Show flagged only"),
                                value = FALSE)
              ),
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

    # Admin: all entries table with full edit/delete/flag capabilities
    output$all_entries <- renderDT({
      input$refresh  # React to refresh button
      data_changed()

      if (!is_admin()) return(NULL)

      dat <- db_get_all_samples()
      if (nrow(dat) == 0) return(NULL)

      # Ensure flagged columns exist (for databases not yet migrated)
      if (!"flagged" %in% names(dat)) dat$flagged <- FALSE
      if (!"flag_reason" %in% names(dat)) dat$flag_reason <- NA_character_

      # Apply flagged filter if checked
      show_flagged_only <- isTRUE(input$filter_flagged)
      if (show_flagged_only) {
        dat <- dat[!is.na(dat$flagged) & dat$flagged == TRUE, ]
        if (nrow(dat) == 0) return(NULL)
      }

      display <- dat %>%
        select(id, species, created_by, ph, organic_matter, texture_class, date, created_at, flagged, flag_reason) %>%
        mutate(date = as.character(date),
               created_at = as.character(created_at))

      # Sanitize user-provided text fields to prevent XSS
      text_cols <- c("species", "created_by", "texture_class", "flag_reason")
      for (col in intersect(text_cols, names(display))) {
        display[[col]] <- vapply(display[[col]], function(v) {
          if (is.na(v) || v == "") v else htmltools::htmlEscape(v)
        }, character(1), USE.NAMES = FALSE)
      }

      # Add flag indicator to species column
      display$species <- mapply(function(sp, is_flagged, reason) {
        if (isTRUE(is_flagged)) {
          reason_text <- if (!is.na(reason) && nzchar(reason)) htmltools::htmlEscape(reason) else "No reason given"
          paste0('<span class="badge bg-danger me-1" title="', reason_text, '"><i class="fa fa-flag"></i></span>', sp)
        } else {
          sp
        }
      }, display$species, display$flagged, display$flag_reason, USE.NAMES = FALSE)

      # Add action buttons - edit, delete, flag/unflag
      display$actions <- mapply(function(entry_id, is_flagged) {
        flag_btn <- if (isTRUE(is_flagged)) {
          sprintf(
            "<button class=\"btn btn-sm btn-warning me-1\" title=\"Unflag entry %d\" aria-label=\"Unflag entry %d\" onclick=\"Shiny.setInputValue('%s', %d, {priority: 'event'})\"><i class=\"fa fa-flag\"></i></button>",
            entry_id, entry_id, ns("unflag_entry"), entry_id
          )
        } else {
          sprintf(
            "<button class=\"btn btn-sm btn-outline-warning me-1\" title=\"Flag entry %d\" aria-label=\"Flag entry %d\" onclick=\"Shiny.setInputValue('%s', %d, {priority: 'event'})\"><i class=\"fa fa-flag\"></i></button>",
            entry_id, entry_id, ns("flag_entry"), entry_id
          )
        }
        sprintf(
          "%s<button class=\"btn btn-sm btn-outline-primary me-1\" title=\"Edit entry %d\" aria-label=\"Edit entry %d\" onclick=\"Shiny.setInputValue('edit_entry', %d, {priority: 'event'})\"><i class=\"fa fa-edit\"></i></button><button class=\"btn btn-sm btn-outline-danger\" title=\"Delete entry %d\" aria-label=\"Delete entry %d\" onclick=\"Shiny.setInputValue('delete_entry', %d, {priority: 'event'})\"><i class=\"fa fa-trash\"></i></button>",
          flag_btn, entry_id, entry_id, entry_id, entry_id, entry_id, entry_id
        )
      }, display$id, display$flagged, USE.NAMES = FALSE)

      # Remove raw flagged/flag_reason columns from display (shown via badge instead)
      display$flagged <- NULL
      display$flag_reason <- NULL

      datatable(display,
                options = list(pageLength = 25, scrollX = TRUE, order = list(list(0, 'desc'))),
                rownames = FALSE,
                escape = FALSE,
                colnames = c("ID", "Species", "Created By", "pH", "OM %", "Texture", "Date", "Created At", "Actions"))
    })

    # Reactive to hold the entry ID being flagged
    pending_flag_id <- reactiveVal(NULL)

    # --- Flag entry ---
    observeEvent(input$flag_entry, {
      if (!is_admin()) return()
      entry_id <- input$flag_entry
      pending_flag_id(entry_id)

      showModal(modalDialog(
        title = span(icon("flag"), " Flag Entry"),
        size = "s",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_flag"), "Flag", class = "btn-warning")
        ),
        p(sprintf("Flag entry #%d for review?", entry_id)),
        textAreaInput(ns("flag_reason_input"), "Reason (optional)",
                      placeholder = "e.g., Suspiciously high pH value",
                      height = "80px")
      ))
    })

    observeEvent(input$confirm_flag, {
      entry_id <- pending_flag_id()
      if (is.null(entry_id)) return()
      reason <- input$flag_reason_input
      if (is.null(reason) || !nzchar(trimws(reason))) reason <- "Flagged by admin"

      success <- db_flag_sample(entry_id, reason, pool)
      removeModal()
      pending_flag_id(NULL)

      if (success) {
        u <- current_user()
        db_audit_log("flag", "soil_samples", entry_id,
                     if (!is.null(u)) u$user_uid else NULL,
                     sprintf("Flagged: %s", reason))
        showNotification(sprintf("Entry #%d flagged", entry_id), type = "message")
        data_changed(data_changed() + 1)
      } else {
        showNotification("Error flagging entry", type = "error")
      }
    })

    # --- Unflag entry ---
    observeEvent(input$unflag_entry, {
      if (!is_admin()) return()
      entry_id <- input$unflag_entry

      success <- db_unflag_sample(entry_id, pool)
      if (success) {
        u <- current_user()
        db_audit_log("unflag", "soil_samples", entry_id,
                     if (!is.null(u)) u$user_uid else NULL,
                     "Unflagged")
        showNotification(sprintf("Entry #%d unflagged", entry_id), type = "message")
        data_changed(data_changed() + 1)
      } else {
        showNotification("Error unflagging entry", type = "error")
      }
    })

    # Admin export with rate limiting
    output$export <- downloadHandler(
      filename = function() paste0("admin_export_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
      content = function(file) {
        if (is_admin()) {
          u <- current_user()

          # Export rate limiting (admins get same limit)
          if (!is.null(u) && !db_check_export_rate(u$user_uid, pool)) {
            showNotification(
              "You've reached the daily export limit (10 per day). Please try again tomorrow.",
              type = "error", duration = 8)
            writeLines("Export rate limit exceeded", file)
            return()
          }

          data <- db_get_all_samples()
          data$created_by <- NULL  # Strip user IDs from export
          write.csv(data, file, row.names = FALSE)
          if (!is.null(u)) {
            db_audit_log("export", "soil_samples", NULL, u$user_uid, sprintf("Admin export: %d records", nrow(data)))
          }
        }
      }
    )
  })
}
