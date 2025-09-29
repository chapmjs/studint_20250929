# Student Interactions Tracking System
# Shiny Application

# WebSocket configuration for RStudio Server
options(shiny.port = NULL)
options(shiny.host = "0.0.0.0")

library(shiny)
library(bslib)
library(DT)
library(DBI)
library(RMySQL)
library(dplyr)
library(lubridate)

# Database connection function
get_db_connection <- function() {
  tryCatch({
    # Debug: print connection info (without password)
    cat("Attempting to connect to database...\n")
    cat("Host:", Sys.getenv("DB_HOST"), "\n")
    cat("Database:", Sys.getenv("DB_NAME"), "\n")
    cat("User:", Sys.getenv("DB_USER"), "\n")
    cat("Port:", Sys.getenv("DB_PORT", "3306"), "\n")

    conn <- dbConnect(
      MySQL(),
      host = Sys.getenv("DB_HOST"),
      dbname = Sys.getenv("DB_NAME"),
      user = Sys.getenv("DB_USER"),
      password = Sys.getenv("DB_PASSWORD"),
      port = as.integer(Sys.getenv("DB_PORT", "3306"))
    )

    cat("Database connection successful!\n")
    return(conn)
  }, error = function(e) {
    cat("Database connection ERROR:", e$message, "\n")
    stop(paste("Database connection failed:", e$message))
  })
}

# Test database connection on startup
cat("\n=== Testing Database Connection ===\n")
test_conn <- tryCatch({
  get_db_connection()
}, error = function(e) {
  cat("STARTUP ERROR: Cannot connect to database\n")
  cat("Error message:", e$message, "\n")
  cat("\nPlease check:\n")
  cat("1. .Renviron file exists in project directory\n")
  cat("2. Database credentials are correct\n")
  cat("3. Database server is running and accessible\n")
  NULL
})

if (!is.null(test_conn)) {
  dbDisconnect(test_conn)
  cat("Database connection test: SUCCESS\n")
} else {
  cat("Database connection test: FAILED\n")
}
cat("===================================\n\n")

# UI
ui <- page_navbar(
  title = "Student Interactions",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2C3E50",
    base_font = font_google("Open Sans")
  ),

  # Students Tab
  nav_panel(
    "Students",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        textInput("student_search", "Search Students", placeholder = "Name, major, or hometown"),
        selectInput("grad_year_filter", "Graduation Year", choices = c("All" = "")),
        actionButton("add_student_btn", "Add New Student", class = "btn-primary w-100")
      ),
      card(
        card_header("Students"),
        DTOutput("students_table")
      )
    )
  ),

  # Record Interaction Tab
  nav_panel(
    "Record Interaction",
    layout_columns(
      col_widths = c(12, 12, 12),
      card(
        card_header("New Interaction"),
        selectInput("interaction_student", "Select Student", choices = NULL),
        dateInput("interaction_date", "Date", value = Sys.Date()),
        textInput("interaction_time", "Time (HH:MM)",
                  value = format(Sys.time(), "%H:%M"),
                  placeholder = "14:30"),
        textInput("interaction_location", "Location", placeholder = "e.g., Office, Hallway, Zoom"),
        textAreaInput("interaction_notes", "Notes", rows = 6, placeholder = "What did you discuss?"),
        checkboxInput("add_commitment_check", "Add commitment from this interaction", FALSE),
        conditionalPanel(
          condition = "input.add_commitment_check == true",
          selectInput("commitment_type_new", "Who will act?",
                      choices = c("Student Action", "My Action")),
          textInput("commitment_action_new", "Action Description"),
          dateInput("commitment_due_new", "Due Date", value = Sys.Date() + 7)
        ),
        actionButton("save_interaction_btn", "Save Interaction", class = "btn-success")
      )
    )
  ),

  # Commitments Tab
  nav_panel(
    "Commitments",
    layout_sidebar(
      sidebar = sidebar(
        width = 250,
        radioButtons("commitment_filter", "Filter",
                     choices = c("All", "Active Only", "Overdue", "Completed",
                                 "Student Actions", "My Actions"),
                     selected = "Active Only"),
        actionButton("add_commitment_btn", "New Commitment", class = "btn-primary w-100")
      ),
      card(
        card_header("Commitments"),
        DTOutput("commitments_table")
      )
    )
  ),

  # Dashboard Tab
  nav_panel(
    "Dashboard",
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        card_header("Total Students"),
        div(style = "font-size: 48px; text-align: center; padding: 20px;",
            textOutput("total_students"))
      ),
      card(
        card_header("Interactions This Month"),
        div(style = "font-size: 48px; text-align: center; padding: 20px;",
            textOutput("interactions_month"))
      ),
      card(
        card_header("Pending Commitments"),
        div(style = "font-size: 48px; text-align: center; padding: 20px;",
            textOutput("pending_commitments"))
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Upcoming Commitments (Next 7 Days)"),
        DTOutput("upcoming_commitments")
      ),
      card(
        card_header("Recent Interactions"),
        DTOutput("recent_interactions")
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Reactive values
  rv <- reactiveValues(refresh = 0)

  # Load students
  students_data <- reactive({
    rv$refresh

    tryCatch({
      conn <- get_db_connection()
      on.exit(dbDisconnect(conn))

      query <- "SELECT student_id, first_name, last_name, email, major,
                graduation_month, graduation_year, hometown, phone,
                linkedin_url, social_media FROM students ORDER BY last_name, first_name"
      students <- dbGetQuery(conn, query)

      # Apply filters
      if (nzchar(input$student_search)) {
        search_term <- tolower(input$student_search)
        students <- students %>%
          filter(
            grepl(search_term, tolower(first_name)) |
              grepl(search_term, tolower(last_name)) |
              grepl(search_term, tolower(major)) |
              grepl(search_term, tolower(hometown))
          )
      }

      if (nzchar(input$grad_year_filter)) {
        students <- students %>% filter(graduation_year == as.integer(input$grad_year_filter))
      }

      students
    }, error = function(e) {
      showNotification(
        paste("Database Error:", e$message),
        type = "error",
        duration = NULL
      )
      data.frame()  # Return empty data frame on error
    })
  })

  # Update graduation year filter choices
  observe({
    tryCatch({
      conn <- get_db_connection()
      on.exit(dbDisconnect(conn))
      years <- dbGetQuery(conn, "SELECT DISTINCT graduation_year FROM students WHERE graduation_year IS NOT NULL ORDER BY graduation_year")
      choices <- c("All" = "", setNames(years$graduation_year, years$graduation_year))
      updateSelectInput(session, "grad_year_filter", choices = choices)
    }, error = function(e) {
      cat("Error updating grad year filter:", e$message, "\n")
    })
  })

  # Update student dropdown for interactions
  observe({
    tryCatch({
      students <- students_data()
      if (nrow(students) > 0) {
        choices <- setNames(students$student_id, paste(students$first_name, students$last_name))
        updateSelectInput(session, "interaction_student", choices = choices)
      } else {
        updateSelectInput(session, "interaction_student", choices = c("No students yet" = ""))
      }
    }, error = function(e) {
      cat("Error updating student dropdown:", e$message, "\n")
    })
  })

  # Display students table
  output$students_table <- renderDT({
    students <- students_data()
    display_data <- students %>%
      select(first_name, last_name, major, graduation_year, email, hometown) %>%
      rename(
        "First Name" = first_name,
        "Last Name" = last_name,
        "Major" = major,
        "Grad Year" = graduation_year,
        "Email" = email,
        "Hometown" = hometown
      )

    datatable(
      display_data,
      options = list(pageLength = 10, dom = 'tip'),
      selection = 'single',
      rownames = FALSE
    )
  })

  # Add student modal
  observeEvent(input$add_student_btn, {
    showModal(modalDialog(
      title = "Add New Student",
      textInput("new_first_name", "First Name*", placeholder = "Required"),
      textInput("new_last_name", "Last Name*", placeholder = "Required"),
      textInput("new_email", "Email"),
      textInput("new_phone", "Phone"),
      selectInput("new_grad_month", "Graduation Month",
                  choices = c("", month.name)),
      numericInput("new_grad_year", "Graduation Year", value = year(Sys.Date()),
                   min = year(Sys.Date()), max = year(Sys.Date()) + 10),
      textInput("new_hometown", "Hometown"),
      textInput("new_major", "Major"),
      textInput("new_linkedin", "LinkedIn URL"),
      textInput("new_social", "Social Media"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_student_btn", "Save Student", class = "btn-primary")
      )
    ))
  })

  # Save new student
  observeEvent(input$save_student_btn, {
    req(input$new_first_name, input$new_last_name)

    conn <- get_db_connection()
    on.exit(dbDisconnect(conn))

    query <- "INSERT INTO students (first_name, last_name, email, phone,
              graduation_month, graduation_year, hometown, major, linkedin_url, social_media)
              VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

    dbExecute(conn, query, params = list(
      input$new_first_name,
      input$new_last_name,
      input$new_email,
      input$new_phone,
      input$new_grad_month,
      input$new_grad_year,
      input$new_hometown,
      input$new_major,
      input$new_linkedin,
      input$new_social
    ))

    rv$refresh <- rv$refresh + 1
    removeModal()
    showNotification("Student added successfully!", type = "message")
  })

  # Save interaction
  observeEvent(input$save_interaction_btn, {
    req(input$interaction_student, input$interaction_date)

    conn <- get_db_connection()
    on.exit(dbDisconnect(conn))

    # Insert interaction
    query <- "INSERT INTO interactions (student_id, interaction_date, interaction_time, location, notes)
              VALUES (?, ?, ?, ?, ?)"

    # Parse time input (HH:MM format)
    time_value <- if (nzchar(input$interaction_time)) {
      paste0(input$interaction_time, ":00")  # Add seconds
    } else {
      NULL
    }

    dbExecute(conn, query, params = list(
      as.integer(input$interaction_student),
      input$interaction_date,
      time_value,
      input$interaction_location,
      input$interaction_notes
    ))

    # Get the interaction ID
    interaction_id <- dbGetQuery(conn, "SELECT LAST_INSERT_ID() as id")$id

    # Add commitment if checked
    if (input$add_commitment_check && nzchar(input$commitment_action_new)) {
      commit_query <- "INSERT INTO commitments (student_id, interaction_id, commitment_type,
                       action_description, due_date) VALUES (?, ?, ?, ?, ?)"

      dbExecute(conn, commit_query, params = list(
        as.integer(input$interaction_student),
        interaction_id,
        input$commitment_type_new,
        input$commitment_action_new,
        input$commitment_due_new
      ))
    }

    rv$refresh <- rv$refresh + 1

    # Reset form
    updateTextInput(session, "interaction_location", value = "")
    updateTextAreaInput(session, "interaction_notes", value = "")
    updateCheckboxInput(session, "add_commitment_check", value = FALSE)

    showNotification("Interaction recorded successfully!", type = "message")
  })

  # Load commitments
  commitments_data <- reactive({
    rv$refresh

    tryCatch({
      conn <- get_db_connection()
      on.exit(dbDisconnect(conn))

      query <- "SELECT c.commitment_id, c.student_id,
                CONCAT(s.first_name, ' ', s.last_name) as student_name,
                c.commitment_type, c.action_description, c.due_date,
                c.is_complete, c.completed_date, c.notes
                FROM commitments c
                JOIN students s ON c.student_id = s.student_id
                ORDER BY c.due_date"

      commitments <- dbGetQuery(conn, query)

      if (nrow(commitments) == 0) {
        return(data.frame(
          commitment_id = integer(),
          student_id = integer(),
          student_name = character(),
          commitment_type = character(),
          action_description = character(),
          due_date = as.Date(character()),
          is_complete = integer(),
          completed_date = as.Date(character()),
          notes = character(),
          stringsAsFactors = FALSE
        ))
      }

      commitments$due_date <- as.Date(commitments$due_date)
      if (!is.null(commitments$completed_date) && any(!is.na(commitments$completed_date))) {
        commitments$completed_date <- as.Date(commitments$completed_date)
      }

      # Apply filters
      filtered <- commitments

      if (input$commitment_filter == "Active Only") {
        filtered <- filtered %>% filter(is_complete == 0)
      } else if (input$commitment_filter == "Overdue") {
        filtered <- filtered %>% filter(is_complete == 0 & due_date < Sys.Date())
      } else if (input$commitment_filter == "Completed") {
        filtered <- filtered %>% filter(is_complete == 1)
      } else if (input$commitment_filter == "Student Actions") {
        filtered <- filtered %>% filter(commitment_type == "Student Action")
      } else if (input$commitment_filter == "My Actions") {
        filtered <- filtered %>% filter(commitment_type == "My Action")
      }

      filtered
    }, error = function(e) {
      showNotification(
        paste("Error loading commitments:", e$message),
        type = "error",
        duration = NULL
      )
      data.frame()
    })
  })

  # Display commitments table
  output$commitments_table <- renderDT({
    commitments <- commitments_data()

    if (nrow(commitments) == 0) {
      return(datatable(
        data.frame(Message = "No commitments found"),
        options = list(dom = 't', ordering = FALSE),
        rownames = FALSE
      ))
    }

    display_data <- commitments %>%
      mutate(
        Status = ifelse(is_complete == 1, "✓ Complete",
                        ifelse(due_date < Sys.Date(), "⚠ Overdue", "Pending")),
        due_date = format(due_date, "%Y-%m-%d")
      ) %>%
      select(student_name, commitment_type, action_description, due_date, Status) %>%
      rename(
        "Student" = student_name,
        "Type" = commitment_type,
        "Action" = action_description,
        "Due Date" = due_date
      )

    datatable(
      display_data,
      options = list(pageLength = 15, dom = 'tip'),
      selection = 'single',
      rownames = FALSE
    ) %>%
      formatStyle(
        'Status',
        backgroundColor = styleEqual(
          c('✓ Complete', '⚠ Overdue', 'Pending'),
          c('#d4edda', '#f8d7da', '#fff3cd')
        )
      )
  })

  # Dashboard metrics
  output$total_students <- renderText({
    conn <- get_db_connection()
    on.exit(dbDisconnect(conn))
    count <- dbGetQuery(conn, "SELECT COUNT(*) as count FROM students")$count
    as.character(count)
  })

  output$interactions_month <- renderText({
    conn <- get_db_connection()
    on.exit(dbDisconnect(conn))
    query <- "SELECT COUNT(*) as count FROM interactions
              WHERE MONTH(interaction_date) = MONTH(CURRENT_DATE())
              AND YEAR(interaction_date) = YEAR(CURRENT_DATE())"
    count <- dbGetQuery(conn, query)$count
    as.character(count)
  })

  output$pending_commitments <- renderText({
    conn <- get_db_connection()
    on.exit(dbDisconnect(conn))
    count <- dbGetQuery(conn, "SELECT COUNT(*) as count FROM commitments WHERE is_complete = 0")$count
    as.character(count)
  })

  # Upcoming commitments
  output$upcoming_commitments <- renderDT({
    conn <- get_db_connection()
    on.exit(dbDisconnect(conn))

    query <- "SELECT CONCAT(s.first_name, ' ', s.last_name) as student_name,
              c.action_description, c.due_date, c.commitment_type
              FROM commitments c
              JOIN students s ON c.student_id = s.student_id
              WHERE c.is_complete = 0 AND c.due_date <= DATE_ADD(CURRENT_DATE(), INTERVAL 7 DAY)
              ORDER BY c.due_date"

    upcoming <- dbGetQuery(conn, query)
    if (nrow(upcoming) > 0) {
      upcoming %>%
        rename("Student" = student_name, "Action" = action_description,
               "Due" = due_date, "Type" = commitment_type) %>%
        datatable(options = list(pageLength = 5, dom = 't'), rownames = FALSE)
    } else {
      datatable(data.frame(Message = "No upcoming commitments"),
                options = list(dom = 't'), rownames = FALSE)
    }
  })

  # Recent interactions
  output$recent_interactions <- renderDT({
    conn <- get_db_connection()
    on.exit(dbDisconnect(conn))

    query <- "SELECT CONCAT(s.first_name, ' ', s.last_name) as student_name,
              i.interaction_date, i.location, LEFT(i.notes, 50) as notes_preview
              FROM interactions i
              JOIN students s ON i.student_id = s.student_id
              ORDER BY i.interaction_date DESC, i.created_at DESC
              LIMIT 5"

    recent <- dbGetQuery(conn, query)
    if (nrow(recent) > 0) {
      recent %>%
        mutate(notes_preview = paste0(notes_preview, "...")) %>%
        rename("Student" = student_name, "Date" = interaction_date,
               "Location" = location, "Notes" = notes_preview) %>%
        datatable(options = list(pageLength = 5, dom = 't'), rownames = FALSE)
    } else {
      datatable(data.frame(Message = "No recent interactions"),
                options = list(dom = 't'), rownames = FALSE)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
