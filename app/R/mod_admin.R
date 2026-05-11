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
          ),

          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              span(icon("users-slash"), " User Moderation"),
              span(class = "badge bg-danger", "Admin Only")
            ),
            card_body(
              p(class = "text-muted",
                "List of contributors. Use ", tags$strong("Bulk-flag"), " to mark every sample from a user as flagged, ",
                "or ", tags$strong("Disable"), " to ban the user from new submissions, exports, and PDF extractions. ",
                "Disabled users can be re-enabled later — disabling does not delete their data."),
              DTOutput(ns("users_table"))
            )
          )
        )
      )
    })

    # --- Per-user moderation table ---
    output$users_table <- renderDT({
      input$refresh
      data_changed()
      if (!is_admin()) return(NULL)

      users <- tryCatch({
        dbGetQuery(pool, "
          SELECT s.created_by AS user_id,
                 COUNT(*)::int AS n_samples,
                 SUM(CASE WHEN s.flagged THEN 1 ELSE 0 END)::int AS n_flagged,
                 MIN(s.created_at)::text AS first_seen,
                 MAX(s.created_at)::text AS last_seen,
                 (CASE WHEN d.user_id IS NOT NULL THEN TRUE ELSE FALSE END) AS is_disabled,
                 d.reason AS disable_reason
          FROM soil_samples s
          LEFT JOIN disabled_users d ON d.user_id = s.created_by
          WHERE s.created_by IS NOT NULL
          GROUP BY s.created_by, d.user_id, d.reason
          ORDER BY MAX(s.created_at) DESC")
      }, error = function(e) {
        message("users_table query error: ", e$message)
        data.frame()
      })

      if (nrow(users) == 0) return(NULL)

      # Sanitize
      users$user_id <- vapply(users$user_id, htmltools::htmlEscape, character(1), USE.NAMES = FALSE)
      users$disable_reason <- vapply(users$disable_reason %||% rep(NA_character_, nrow(users)),
                                     function(v) if (is.na(v) || !nzchar(v)) "" else htmltools::htmlEscape(v),
                                     character(1), USE.NAMES = FALSE)

      users$status <- ifelse(users$is_disabled,
                             '<span class="badge bg-danger">Disabled</span>',
                             '<span class="badge bg-success">Active</span>')

      users$actions <- mapply(function(uid, disabled) {
        # Hidden Shiny input wiring via Shiny.setInputValue
        bulk_btn <- sprintf(
          "<button class=\"btn btn-sm btn-outline-warning me-1\" title=\"Bulk-flag all samples from this user\" onclick=\"Shiny.setInputValue('%s', '%s', {priority: 'event'})\"><i class=\"fa fa-flag\"></i> Bulk-flag</button>",
          ns("bulk_flag_user"), uid)
        toggle_btn <- if (isTRUE(disabled)) {
          sprintf(
            "<button class=\"btn btn-sm btn-success\" title=\"Re-enable this user\" onclick=\"Shiny.setInputValue('%s', '%s', {priority: 'event'})\"><i class=\"fa fa-user-check\"></i> Enable</button>",
            ns("enable_user"), uid)
        } else {
          sprintf(
            "<button class=\"btn btn-sm btn-danger\" title=\"Disable this user\" onclick=\"Shiny.setInputValue('%s', '%s', {priority: 'event'})\"><i class=\"fa fa-user-slash\"></i> Disable</button>",
            ns("disable_user"), uid)
        }
        paste0(bulk_btn, toggle_btn)
      }, users$user_id, users$is_disabled, USE.NAMES = FALSE)

      display <- users[, c("user_id", "status", "n_samples", "n_flagged", "first_seen", "last_seen", "disable_reason", "actions")]
      datatable(display,
                options = list(pageLength = 10, scrollX = TRUE, order = list(list(2, 'desc'))),
                rownames = FALSE,
                escape = FALSE,
                colnames = c("User ID", "Status", "Samples", "Flagged", "First seen", "Last seen", "Disable reason", "Actions"))
    })

    # --- Bulk-flag all samples from user ---
    pending_bulk_user <- reactiveVal(NULL)
    observeEvent(input$bulk_flag_user, {
      if (!is_admin()) return()
      uid <- input$bulk_flag_user
      if (is.null(uid) || !nzchar(uid)) return()
      pending_bulk_user(uid)
      showModal(modalDialog(
        title = span(icon("flag"), " Bulk-flag user samples"),
        size = "s",
        easyClose = TRUE,
        footer = tagList(modalButton("Cancel"),
                         actionButton(ns("confirm_bulk_flag"), "Bulk-flag", class = "btn-warning")),
        p("Flag every sample from this user as suspicious?"),
        tags$code(uid),
        textAreaInput(ns("bulk_flag_reason"), "Reason",
                      value = "Bulk flagged by admin",
                      height = "80px")
      ))
    })
    observeEvent(input$confirm_bulk_flag, {
      uid <- pending_bulk_user()
      if (is.null(uid)) return()
      reason <- input$bulk_flag_reason
      if (is.null(reason) || !nzchar(trimws(reason))) reason <- "Bulk flagged by admin"

      n <- db_flag_samples_by_user(uid, reason, pool)
      removeModal()
      pending_bulk_user(NULL)

      u <- current_user()
      db_audit_log("bulk_flag", "soil_samples", NULL,
                   if (!is.null(u)) u$user_uid else NULL,
                   sprintf("Bulk-flagged %d samples from user %s: %s", n, uid, reason))
      showNotification(sprintf("Flagged %d sample%s from user", n, if (n == 1) "" else "s"),
                       type = "message")
      data_changed(data_changed() + 1)
    })

    # --- Disable user ---
    pending_disable_user <- reactiveVal(NULL)
    observeEvent(input$disable_user, {
      if (!is_admin()) return()
      uid <- input$disable_user
      if (is.null(uid) || !nzchar(uid)) return()
      pending_disable_user(uid)
      showModal(modalDialog(
        title = span(icon("user-slash"), " Disable user"),
        size = "s",
        easyClose = TRUE,
        footer = tagList(modalButton("Cancel"),
                         actionButton(ns("confirm_disable_user"), "Disable", class = "btn-danger")),
        p("Block this user from new submissions, exports, and PDF extractions?"),
        tags$code(uid),
        p(class = "text-muted small mt-2",
          "Existing samples are NOT auto-removed. Use Bulk-flag separately if you also want to hide their submissions."),
        textAreaInput(ns("disable_reason"), "Reason",
                      placeholder = "e.g., Submitting spam / abusive content",
                      height = "80px")
      ))
    })
    observeEvent(input$confirm_disable_user, {
      uid <- pending_disable_user()
      if (is.null(uid)) return()
      u <- current_user()
      ok <- db_disable_user(uid, input$disable_reason %||% "Disabled by admin",
                            if (!is.null(u)) u$user_uid else NULL, pool)
      removeModal()
      pending_disable_user(NULL)
      if (ok) {
        db_audit_log("disable_user", "disabled_users", NULL,
                     if (!is.null(u)) u$user_uid else NULL,
                     sprintf("Disabled user %s: %s", uid, input$disable_reason %||% ""))
        showNotification("User disabled", type = "message")
        data_changed(data_changed() + 1)
      } else {
        showNotification("Error disabling user", type = "error")
      }
    })

    # --- Re-enable user ---
    observeEvent(input$enable_user, {
      if (!is_admin()) return()
      uid <- input$enable_user
      if (is.null(uid) || !nzchar(uid)) return()
      ok <- db_enable_user(uid, pool)
      if (ok) {
        u <- current_user()
        db_audit_log("enable_user", "disabled_users", NULL,
                     if (!is.null(u)) u$user_uid else NULL,
                     sprintf("Re-enabled user %s", uid))
        showNotification("User re-enabled", type = "message")
        data_changed(data_changed() + 1)
      } else {
        showNotification("Error re-enabling user", type = "error")
      }
    })

    # Admin: all entries table with full edit/delete/flag capabilities
    output$all_entries <- renderDT({
      input$refresh  # React to refresh button
      data_changed()

      if (!is_admin()) return(NULL)

      dat <- db_get_all_samples(include_flagged = TRUE)
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

          data <- db_get_all_samples(include_flagged = TRUE)
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
