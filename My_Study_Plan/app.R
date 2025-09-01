# ========================
# Graduate Study Planner
# ========================

# ---- Packages ----
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(jsonlite)
library(officer)
library(flextable)
library(stringr)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- UI ----
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Graduate Study Planner", titleWidth = 280),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("Course Planning", tabName = "planning", icon = icon("calendar")),
      menuItem("Progress Analytics", tabName = "analytics", icon = icon("chart-bar")),
      menuItem("Export & Timeline", tabName = "export", icon = icon("file-export")),
      menuItem("How to Use & Contact", tabName = "help", icon = icon("circle-question")),
      br(),
      div(
        style = "padding: 16px 16px 8px; border-top: 1px solid rgba(255,255,255,0.1);",
        fileInput("upload_data", "Import Data",
                  accept = c(".csv"),
                  buttonLabel = "Browse",
                  placeholder = "No file selected"),
        # Sidebar: Template button only (removed broken Export CSV here)
        div(style="display:grid; grid-template-columns:1fr; margin-top:8px;",
            downloadButton("download_template", "Template", class = "btn btn-outline-light btn-sm")
        )
      )
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML('
        /* Modern minimalist design */
        :root {
          --primary: #2c3e50;
          --secondary: #34495e;
          --accent: #3498db;
          --success: #27ae60;
          --warning: #f39c12;
          --danger: #e74c3c;
          --light: #ecf0f1;
          --dark: #2c3e50;
          --text: #2c3e50;
          --text-muted: #7f8c8d;
          --border: #bdc3c7;
          --shadow: rgba(0,0,0,0.08);
        }
        .content-wrapper { 
          background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
          min-height: 100vh;
        }
        .main-header .logo, .main-header .navbar {
          background: var(--primary) !important;
          border-bottom: 3px solid var(--accent);
        }
        .main-header .logo { 
          font-weight: 600; 
          letter-spacing: 0.5px;
          font-size: 18px;
        }
        .main-sidebar { background: var(--secondary) !important; }
        .sidebar-menu > li > a {
          color: rgba(255,255,255,0.8) !important;
          border-left: 3px solid transparent;
          transition: all 0.3s ease;
        }
        .sidebar-menu > li > a:hover,
        .sidebar-menu > li.active > a {
          background: rgba(255,255,255,0.1) !important;
          border-left-color: var(--accent);
          color: white !important;
        }
        .box {
          border: none !important;
          border-radius: 12px !important;
          box-shadow: 0 4px 6px var(--shadow) !important;
          margin-bottom: 20px;
          background: white;
        }
        .box-header { border-bottom: 1px solid #f4f4f4; padding: 20px 20px 15px; }
        .box-title { font-size: 18px; font-weight: 600; color: var(--text); margin: 0; }
        .box-body { padding: 20px; }
        .small-box {
          border-radius: 12px !important;
          box-shadow: 0 4px 6px var(--shadow) !important;
          border: none !important;
          overflow: hidden;
          transition: transform 0.2s ease, box-shadow 0.2s ease;
        }
        .small-box:hover { transform: translateY(-2px); box-shadow: 0 8px 15px rgba(0,0,0,0.12) !important; }
        .small-box .inner h3 {
          font-size: 2.2rem;
          font-weight: 700;
          margin-bottom: 5px;
          color: white !important;
        }
        .small-box .inner p { font-size: 14px; font-weight: 500; opacity: 0.9; }
        .small-box .icon { opacity: 0.15; font-size: 70px; }
        .form-control {
          border: 1px solid var(--border);
          border-radius: 6px;
          padding: 8px 12px;
          font-size: 14px;
          transition: border-color 0.2s ease;
        }
        .form-control:focus { border-color: var(--accent); box-shadow: 0 0 0 2px rgba(52, 152, 219, 0.1); }
        .btn { border-radius: 6px; font-weight: 500; padding: 8px 16px; font-size: 14px; border: none; transition: all 0.2s ease; }
        .btn-primary { background: var(--accent); color: white; }
        .btn-primary:hover { background: #2980b9; transform: translateY(-1px); }
        .btn-success { background: var(--success); }
        .btn-warning { background: var(--warning); }
        .btn-danger { background: var(--danger); }
        .dataTables_wrapper { font-size: 14px; }
        table.dataTable { border-collapse: separate; border-spacing: 0; border-radius: 8px; overflow: hidden; }
        table.dataTable thead th {
          background: var(--light); border-bottom: 2px solid var(--border); font-weight: 600; color: var(--text); padding: 12px 8px;
        }
        table.dataTable tbody td { padding: 10px 8px; border-bottom: 1px solid #f8f9fa; }
        table.dataTable tbody tr:hover { background-color: rgba(52, 152, 219, 0.05); }
        .progress-card {
          text-align: center; padding: 20px; border-radius: 12px; background: white; box-shadow: 0 2px 4px var(--shadow); margin-bottom: 20px;
        }
        .progress-number { font-size: 2rem; font-weight: 700; color: var(--accent); margin-bottom: 5px; }
        .progress-label { font-size: 14px; color: var(--text-muted); font-weight: 500; }
        .plot-container { background: white; border-radius: 8px; padding: 15px; margin: 10px 0; }
        h1, h2, h3, h4 { color: var(--text); font-weight: 600; }
        .text-muted { color: var(--text-muted) !important; }
        .dashboard-row { margin-bottom: 25px; }
        .content { padding: 25px; }
        @media (max-width: 768px) {
          .content { padding: 15px; }
          .box-body { padding: 15px; }
        }
      '))
    ),
    # --- JS for localStorage autosave/restore ---
    tags$script(HTML("
      Shiny.addCustomMessageHandler('saveState', function(payload_json){
        try { localStorage.setItem('gradStudyPlannerState', payload_json); }
        catch(e) { Shiny.setInputValue('storage_error', true, {priority:'event'}); }
      });
      document.addEventListener('DOMContentLoaded', function(){
        try {
          var saved = localStorage.getItem('gradStudyPlannerState');
          if (saved) { Shiny.setInputValue('restore_state', saved, {priority: 'event'}); }
        } catch(e) { Shiny.setInputValue('storage_error', true, {priority:'event'}); }
      });
    ")),
    
    tabItems(
      # ---- Dashboard Overview ----
      tabItem(
        tabName = "dashboard",
        div(class = "dashboard-row",
            fluidRow(
              valueBoxOutput("total_credits_box", width = 3),
              valueBoxOutput("completed_credits_box", width = 3),
              valueBoxOutput("pending_credits_box", width = 3),
              valueBoxOutput("gpa_box", width = 3)
            )
        ),
        div(class = "dashboard-row",
            fluidRow(
              box(
                width = 6, title = "Academic Progress Overview", status = "primary",
                plotOutput("progress_overview", height = "300px")
              ),
              box(
                width = 6, title = "Credit Distribution by Category", status = "primary",
                plotOutput("credit_distribution", height = "300px")
              )
            )
        ),
        div(class = "dashboard-row",
            fluidRow(
              box(
                width = 12, title = "Recent Activity & Upcoming Courses", status = "info",
                DTOutput("recent_activity")
              )
            )
        ),
        fluidRow(
          box(width = 12, status = "danger", title = "Danger Zone",
              actionButton("reset_all", "Reset All Data", class = "btn-danger",
                           icon = icon("bomb"))
          )
        )
      ),
      
      # ---- Course Planning ----
      tabItem(
        tabName = "planning",
        fluidRow(
          box(
            width = 4, title = "Add / Edit Course", status = "primary",
            
            numericInput("calendar_year", "Academic Year:",
                         value = as.numeric(format(Sys.Date(), "%Y")), min = 2000, max = 2050),
            selectInput("semester", "Semester:", choices = c("Spring","Summer","Fall")),
            textInput("course_name", "Course Name:", placeholder = "e.g., Research Methods"),
            textInput("course_code", "Course Code:", placeholder = "e.g., GRAD601"),
            numericInput("course_credit", "Credits:", value = 3, min = 0, max = 30, step = .5),
            
            selectizeInput(
              "course_tags", "Categories",
              choices = NULL, multiple = TRUE,
              options = list(create = TRUE, persist = FALSE,
                             placeholder = 'Required, Elective, Core, Methods, etc.')
            ),
            
            prettyCheckbox("course_done", "Completed", value = FALSE, status = "success", shape = "curve"),
            conditionalPanel(
              condition = "input.course_done == true",
              selectInput("course_grade", "Grade:",
                          choices = c("A","A-","B+","B","B-","C+","C","C-","F"))
            ),
            
            br(),
            div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px;",
                actionButton("add_course", "Add Course", class = "btn-primary", icon = icon("plus")),
                actionButton("edit_course", "Edit Selected", class = "btn-warning", icon = icon("edit"))
            ),
            div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin-top: 10px;",
                actionButton("clear_inputs", "Clear", class = "btn-default", icon = icon("eraser")),
                actionButton("remove_selected_from_form", "Remove", class = "btn-danger", icon = icon("trash"))
            )
          ),
          
          box(
            width = 8, title = "Course Schedule", status = "primary",
            DTOutput("course_schedule"),
            br(),
            actionButton("remove_selected_from_table", "Remove Selected",
                         class = "btn-danger btn-sm", icon = icon("trash"))
          )
        )
      ),
      
      # ---- Enhanced Analytics ----
      tabItem(
        tabName = "analytics",
        fluidRow(
          box(
            width = 6, title = "Academic Timeline", status = "primary",
            plotOutput("academic_timeline", height = "350px")
          ),
          box(
            width = 6, title = "Performance Trends", status = "primary",
            plotOutput("performance_trends", height = "350px")
          )
        ),
        fluidRow(
          box(
            width = 8, title = "Category Credits (Completed vs Planned)", status = "primary",
            plotOutput("category_bars", height = "400px")
          ),
          box(
            width = 4, title = "Study Analytics", status = "info",
            div(class = "progress-card",
                div(class = "progress-number", textOutput("completion_rate")),
                div(class = "progress-label", "Completion Rate")
            ),
            div(class = "progress-card",
                div(class = "progress-number", textOutput("avg_credits_per_semester")),
                div(class = "progress-label", "Avg Credits/Semester")
            ),
            div(class = "progress-card",
                div(class = "progress-number", textOutput("projected_graduation")),
                div(class = "progress-label", "Est. Graduation")
            )
          )
        ),
        fluidRow(
          box(
            width = 12, title = "Detailed Performance Analysis", status = "primary",
            DTOutput("performance_analysis")
          )
        )
      ),
      
      # ---- Export & Timeline ----
      tabItem(
        tabName = "export",
        fluidRow(
          box(
            width = 6, title = "Academic Timeline", status = "primary",
            helpText("Configure your academic milestones"),
            div(style="display:grid; grid-template-columns:1fr 1fr; gap:15px;",
                textInput("tt_admit", "Date Admitted", placeholder = "Spring 2023"),
                textInput("tt_hours", "Hours Completed", placeholder = "auto-calculated"),
                textInput("tt_coursework", "Coursework Complete", placeholder = "Spring 2025"),
                textInput("tt_comps", "Comprehensive Exam", placeholder = "Fall 2025"),
                textInput("tt_proposal", "Dissertation Proposal", placeholder = "Spring 2026"),
                textInput("tt_residency", "Residency Semesters", placeholder = "4"),
                textInput("tt_dissertation", "Dissertation Complete", placeholder = "Fall 2027"),
                textAreaInput("tt_notes", "Additional Notes", rows = 3, placeholder = "Optional notes...")
            ),
            div(style="display:flex; gap:10px; margin-top:15px;",
                actionButton("tt_infer", "Auto-Fill", class = "btn-primary", icon = icon("magic")),
                actionButton("tt_clear", "Clear All", class = "btn-default", icon = icon("eraser"))
            )
          ),
          box(
            width = 6, title = "Export Options", status = "success",
            h4("Plan of Study Document"),
            p("Generate a comprehensive academic plan organized by course categories."),
            downloadButton("download_docx", "Export to Word", class = "btn-success btn-lg", icon = icon("file-word")),
            br(), br(),
            h4("Data Export"),
            p("Export your course data for backup or external analysis."),
            downloadButton("download_data", "Export CSV", class = "btn-primary", icon = icon("file-csv"))
          )
        )
      ),
      
      # ---- Help ----
      tabItem(
        tabName = "help",
        box(width = 12, title = "How to Use This Planner", status = "primary",
            HTML("
              <ol style='line-height:1.8'>
                <li><b>Add courses:</b> Go to <i>Course Planning</i>. Choose the <b>Academic Year</b>, <b>Semester</b> (Spring, Summer, Fall),
                    enter <b>Course Name</b>, <b>Code</b>, <b>Credits</b>, and optional <b>Categories</b> (e.g., Core, Methods, Elective).
                    Click <b>Add Course</b>.</li>
                <li><b>Mark completion & grades:</b> Toggle <b>Completed</b> and select a <b>Grade</b>. 
                    The <b>GPA</b> and <b>Completed/Pending credits</b> update instantly on the Dashboard.</li>
                <li><b>Edit or remove:</b> Select a row in <i>Course Schedule</i> then click <b>Edit Selected</b> or <b>Remove</b>.
                    Inline edits are allowed for <i>Credits</i>, <i>Completed</i>, and <i>Grade</i>.</li>
                <li><b>Categories (Tags):</b> Use categories to group courses for analytics and exports.
                    You can type new categories on the fly; they automatically appear for future selection.</li>
                <li><b>Import data:</b> Click <b>Import Data</b> (CSV with columns:
                    Semester, CalendarYear, Course, Code, Credit, Grade, Tags). 
                    <i>Completed</i> is <u>inferred</u> from <b>Grade</b> (any valid grade means completed).
                    Tags can be separated by commas or semicolons.</li>
                <li><b>Export:</b> In <i>Export & Timeline</i>, download your data as CSV or a Word <b>Plan of Study</b>, grouped by category.</li>
                <li><b>Timeline & milestones:</b> Fill fields or click <b>Auto-Fill</b> to infer milestones (admit, coursework completion, dissertation, etc.) from your data. Adjust as needed.</li>
                <li><b>Analytics:</b>
                  <ul>
                    <li><i>Academic Progress Overview</i> shows completed vs planned credits by <b>semester</b>.</li>
                    <li><i>Performance Trends</i> shows semester GPA with a smooth trend line (bubble size = credits taken).</li>
                    <li><i>Category Credits</i> stacks completed and planned credits per category.</li>
                  </ul>
                </li>
                <li><b>Autosave & reset:</b> The app autosaves to your browser. Use the <b>Reset All Data</b> button on the Dashboard to clear everything.</li>
                <li><b>Best practices:</b> Keep categories consistent; enter grades promptly so GPA and projections stay accurate; export regularly.</li>
              </ol>
            ")
        ),
        box(width = 12, title = "Contact", status = "info",
            HTML("<p>Questions or feature requests? Email <a href='mailto:hoadegoke@uncg.edu'>hoadegoke@uncg.edu</a>.</p>")
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # ---- Semester ordering (Spring before Fall; Summer in the middle) ----
  SEM_ORDER <- c("Spring","Summer","Fall")
  sem_order_index <- c("Spring"=1, "Summer"=2, "Fall"=3)
  
  normalize_sem <- function(x) {
    factor(as.character(x), levels = SEM_ORDER, ordered = TRUE)
  }
  sem_key <- function(sem, yr) {
    s <- unname(sem_order_index[as.character(sem)] %||% 4L)
    as.numeric(yr) * 10 + as.numeric(s)
  }
  
  # ---- Data store ----
  course_data <- reactiveVal(
    data.frame(
      Semester = character(),
      CalendarYear = integer(),
      Course = character(),
      Code = character(),
      Credit = numeric(),
      Completed = logical(),
      Grade = character(),
      Tags = I(list()),
      stringsAsFactors = FALSE
    )
  )
  
  has_restored <- reactiveVal(FALSE)
  
  # ---- Helper functions ----
  calculate_gpa <- function(data) {
    if (nrow(data) == 0 || !any(data$Completed, na.rm = TRUE)) return(0)
    pts <- c("A"=4.0,"A-"=3.7,"B+"=3.3,"B"=3.0,"B-"=2.7,"C+"=2.3,"C"=2.0,"C-"=1.7,"F"=0.0)
    completed <- data %>% filter(Completed == TRUE, !is.na(Grade), Grade %in% names(pts))
    if (nrow(completed) == 0) return(0)
    credits <- as.numeric(completed$Credit)
    grade_vals <- as.numeric(pts[completed$Grade])
    total_points <- sum(credits * grade_vals, na.rm = TRUE)
    total_credits <- sum(credits, na.rm = TRUE)
    if (total_credits == 0) return(0)
    round(total_points / total_credits, 2)
  }
  
  expand_by_tag <- function(df) {
    if (!nrow(df)) return(df[0, ])
    df %>%
      mutate(Tags = ifelse(lengths(Tags) == 0, list(NA_character_), Tags)) %>%
      tidyr::unnest(Tags)
  }
  
  # ---- Enhanced Analytics Functions ----
  get_completion_rate <- function(data) {
    if (nrow(data) == 0) return("0%")
    completed <- sum(data$Completed, na.rm = TRUE)
    total <- nrow(data)
    paste0(round(completed/total * 100), "%")
  }
  
  get_avg_credits_per_semester <- function(data) {
    if (nrow(data) == 0) return("0")
    sems <- data %>%
      group_by(CalendarYear, Semester) %>%
      summarise(total_credits = sum(Credit, na.rm = TRUE), .groups = "drop")
    round(mean(sems$total_credits, na.rm = TRUE), 1)
  }
  
  estimate_graduation <- function(data) {
    if (nrow(data) == 0) return("TBD")
    max_year <- max(data$CalendarYear, na.rm = TRUE)
    as.character(max_year)
  }
  
  # ---- Course Management ----
  observeEvent(input$add_course, {
    req(input$course_name, input$course_code)
    if (nchar(input$course_name) < 3 || nchar(input$course_code) < 2) {
      showNotification("Please enter valid course name and code", type = "error")
      return()
    }
    
    d <- course_data()
    if (nrow(d) && any(d$Semester==input$semester & d$CalendarYear==input$calendar_year & d$Code==input$course_code)) {
      showNotification("This course already exists for that semester.", type = "warning")
      return()
    }
    
    new_entry <- data.frame(
      Semester = input$semester,
      CalendarYear = as.integer(input$calendar_year),
      Course = input$course_name,
      Code = input$course_code,
      Credit = as.numeric(input$course_credit),
      Completed = isTRUE(input$course_done),
      Grade = if (isTRUE(input$course_done)) input$course_grade else NA_character_,
      Tags  = I(list(input$course_tags %||% character(0))),
      stringsAsFactors = FALSE
    )
    
    course_data(bind_rows(course_data(), new_entry))
    showNotification("Course added successfully", type = "message")
    
    # Clear inputs
    updateTextInput(session, "course_name", value = "")
    updateTextInput(session, "course_code", value = "")
    updateSelectizeInput(session, "course_tags", selected = character(0))
  })
  
  # Edit course functionality
  observeEvent(input$edit_course, {
    sel <- input$course_schedule_rows_selected
    d <- course_data()
    req(length(sel) == 1, nrow(d) >= sel)
    
    updateNumericInput(session, "calendar_year", value = d$CalendarYear[sel])
    updateSelectInput(session,  "semester",      selected = d$Semester[sel])
    updateTextInput(session,    "course_name",   value = d$Course[sel])
    updateTextInput(session,    "course_code",   value = d$Code[sel])
    updateNumericInput(session, "course_credit", value = d$Credit[sel])
    shinyWidgets::updatePrettyCheckbox(session, "course_done", value = isTRUE(d$Completed[sel]))
    if (!is.na(d$Grade[sel])) updateSelectInput(session, "course_grade", selected = d$Grade[sel])
    
    updateSelectizeInput(session, "course_tags",
                         choices = sort(unique(unlist(d$Tags))),
                         selected = d$Tags[[sel]])
    
    course_data(d[-sel, ])
  })
  
  # Keep tag choices in sync with data
  observe({
    d <- course_data()
    tag_choices <- sort(unique(unlist(d$Tags)))
    updateSelectizeInput(session, "course_tags", choices = tag_choices, server = TRUE)
  })
  
  # Clear form
  observeEvent(input$clear_inputs, {
    updateNumericInput(session, "calendar_year", value = as.numeric(format(Sys.Date(), "%Y")))
    updateSelectInput(session, "semester", selected = "Spring")
    updateTextInput(session, "course_name", value = "")
    updateTextInput(session, "course_code", value = "")
    updateNumericInput(session, "course_credit", value = 3)
    shinyWidgets::updatePrettyCheckbox(session, "course_done", value = FALSE)
    updateSelectInput(session, "course_grade", selected = "A")
    updateSelectizeInput(session, "course_tags", selected = character(0))
  })
  
  # Remove selected course
  remove_selected <- function() {
    sel <- input$course_schedule_rows_selected
    d <- course_data()
    if (length(sel) == 1 && nrow(d) >= sel) {
      course_data(d[-sel, ])
      showNotification("Course removed", type = "message")
    } else {
      showNotification("Please select a course to remove", type = "warning")
    }
  }
  observeEvent(input$remove_selected_from_table, remove_selected())
  observeEvent(input$remove_selected_from_form,  remove_selected())
  
  # ---- Value Boxes ----
  output$total_credits_box <- renderValueBox({
    d <- course_data()
    valueBox(
      value = sum(as.numeric(d$Credit), na.rm = TRUE),
      subtitle = "Total Credits",
      icon = icon("graduation-cap"),
      color = "blue"
    )
  })
  
  output$completed_credits_box <- renderValueBox({
    d <- course_data()
    valueBox(
      value = sum(as.numeric(d$Credit[d$Completed %in% TRUE]), na.rm = TRUE),
      subtitle = "Completed Credits",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$pending_credits_box <- renderValueBox({
    d <- course_data()
    valueBox(
      value = sum(as.numeric(d$Credit[d$Completed %in% FALSE]), na.rm = TRUE),
      subtitle = "Pending Credits",
      icon = icon("clock"),
      color = "yellow"
    )
  })
  
  output$gpa_box <- renderValueBox({
    d <- course_data()
    gpa <- calculate_gpa(d)
    valueBox(
      value = sprintf("%.2f", gpa),
      subtitle = "Current GPA",
      icon = icon("star"),
      color = if(gpa >= 3.5) "green" else if(gpa >= 3.0) "yellow" else "red"
    )
  })
  
  # ---- Enhanced Analytics Outputs ----
  output$completion_rate <- renderText({
    get_completion_rate(course_data())
  })
  
  output$avg_credits_per_semester <- renderText({
    get_avg_credits_per_semester(course_data())
  })
  
  output$projected_graduation <- renderText({
    estimate_graduation(course_data())
  })
  
  # ---- Enhanced Visualizations ----
  output$progress_overview <- renderPlot({
    d <- course_data()
    if (nrow(d) == 0) return(NULL)
    
    yrs <- seq(min(d$CalendarYear), max(d$CalendarYear))
    complete_semesters <- expand.grid(CalendarYear = yrs, Semester = SEM_ORDER) %>%
      arrange(CalendarYear, match(Semester, SEM_ORDER)) %>%
      mutate(SemesterLabel = paste(Semester, CalendarYear))
    
    progress_data <- d %>%
      group_by(CalendarYear, Semester) %>%
      summarise(Completed = sum(Credit[Completed], na.rm=TRUE),
                Pending   = sum(Credit[!Completed], na.rm=TRUE), .groups="drop") %>%
      right_join(complete_semesters, by = c("CalendarYear","Semester")) %>%
      mutate(across(c(Completed, Pending), ~replace_na(.x, 0))) %>%
      pivot_longer(c(Completed, Pending), names_to="Status", values_to="Credits") %>%
      filter(Credits > 0) %>%
      mutate(SemesterLabel = factor(SemesterLabel, levels = complete_semesters$SemesterLabel))
    
    if (!nrow(progress_data)) return(NULL)
    
    ggplot(progress_data, aes(x = SemesterLabel, y = Credits, fill = Status)) +
      geom_col() +
      scale_fill_manual(values = c(Completed="#27ae60", Pending="#f39c12")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle=45, hjust=1),
            legend.position = "top",
            panel.grid.minor = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank())
  })
  
  output$credit_distribution <- renderPlot({
    d <- course_data()
    if (nrow(d) == 0) return(NULL)
    
    expanded <- expand_by_tag(d) %>%
      filter(!is.na(Tags)) %>%
      group_by(Tags) %>%
      summarise(Credits = sum(Credit, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Credits))
    
    if (nrow(expanded) == 0) return(NULL)
    
    ggplot(expanded, aes(x = reorder(Tags, Credits), y = Credits)) +
      geom_bar(stat = "identity", fill = "#3498db", alpha = 0.8) +
      coord_flip() +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      labs(x = "", y = "Credits", title = "")
  })
  
  output$academic_timeline <- renderPlot({
    d <- course_data()
    if (nrow(d) == 0) return(NULL)
    
    timeline_data <- d %>%
      mutate(sem_key = sem_key(Semester, CalendarYear)) %>%
      arrange(CalendarYear, match(Semester, SEM_ORDER)) %>%
      mutate(
        cumulative_credits = cumsum(ifelse(Completed, Credit, 0)),
        total_planned = cumsum(Credit),
        SemesterLabel = paste(Semester, CalendarYear)
      )
    
    yrs <- seq(min(timeline_data$CalendarYear), max(timeline_data$CalendarYear))
    all_semesters <- as.vector(sapply(yrs, function(y) paste(SEM_ORDER, y)))
    timeline_data$SemesterLabel <- factor(timeline_data$SemesterLabel, levels = all_semesters)
    
    ggplot(timeline_data, aes(x = SemesterLabel)) +
      geom_line(aes(y = cumulative_credits, color = "Completed"), linewidth = 1.2, group=1) +
      geom_line(aes(y = total_planned, color = "Planned"), linewidth = 1, linetype = "dashed", group=1) +
      scale_color_manual(values = c("Completed"="#27ae60", "Planned"="#95a5a6")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle=45, hjust=1),
            legend.position = "top",
            panel.grid.minor = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank())
  })
  
  output$performance_trends <- renderPlot({
    d <- course_data()
    if (nrow(d)==0 || !any(d$Completed, na.rm=TRUE)) return(NULL)
    
    pts <- c("A"=4.0,"A-"=3.7,"B+"=3.3,"B"=3.0,"B-"=2.7,"C+"=2.3,"C"=2.0,"C-"=1.7,"F"=0.0)
    
    perf <- d %>%
      filter(Completed, !is.na(Grade), Grade %in% names(pts)) %>%
      mutate(grade_val = pts[Grade],
             skey = sem_key(Semester, CalendarYear)) %>%
      arrange(CalendarYear, match(Semester, SEM_ORDER)) %>%
      group_by(CalendarYear, Semester) %>%
      summarise(semester_gpa = weighted.mean(grade_val, Credit, na.rm=TRUE),
                credits = sum(Credit, na.rm=TRUE),
                .groups="drop") %>%
      mutate(SemesterLabel = paste(Semester, CalendarYear))
    
    if (!nrow(perf)) return(NULL)
    
    yrs <- seq(min(perf$CalendarYear), max(perf$CalendarYear))
    all_semesters <- as.vector(sapply(yrs, function(y) paste(SEM_ORDER, y)))
    perf$SemesterLabel <- factor(perf$SemesterLabel, levels = all_semesters)
    
    ggplot(perf, aes(x = SemesterLabel, y = semester_gpa, group = 1)) +
      geom_hline(yintercept = 3.0, linetype = "dashed", color = "#e74c3c", alpha = 0.5) +
      geom_hline(yintercept = 3.5, linetype = "dashed", color = "#27ae60", alpha = 0.5) +
      geom_point(aes(size = credits), alpha = 0.85) +
      geom_line(linewidth = 1.1, alpha = 0.7) +
      geom_smooth(method = "loess", se = FALSE, linewidth = 1, alpha=0.6) +
      scale_size_continuous(range = c(3, 8), name = "Credits") +
      coord_cartesian(ylim = c(0, 4)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle=45, hjust=1),
            legend.position = "top",
            panel.grid.minor = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank())
  })
  
  output$category_bars <- renderPlot({
    d <- course_data(); if (!nrow(d)) return(NULL)
    
    expanded <- expand_by_tag(d) %>%
      mutate(Status = ifelse(Completed, "Completed", "Planned")) %>%
      filter(!is.na(Tags)) %>%
      group_by(Tags, Status) %>%
      summarise(Credits = sum(Credit, na.rm=TRUE), .groups="drop")
    
    if (!nrow(expanded)) return(NULL)
    
    order_df <- expanded %>%
      group_by(Tags) %>%
      summarise(Total = sum(Credits), .groups="drop") %>%
      arrange(Total)
    expanded$Tags <- factor(expanded$Tags, levels = order_df$Tags)
    
    ggplot(expanded, aes(x = Credits, y = Tags, fill = Status)) +
      geom_col(position = "stack") +
      scale_fill_manual(values = c(Completed="#27ae60", Planned="#f39c12")) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(),
            legend.position = "top",
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      labs(fill = "")
  })
  
  # ---- DataTable Outputs ----
  output$course_schedule <- DT::renderDataTable({
    d <- course_data()
    if (nrow(d) == 0) {
      return(DT::datatable(d, selection = "single"))
    }
    
    disp <- d %>%
      mutate(sem_key = sem_key(Semester, CalendarYear)) %>%
      arrange(desc(sem_key)) %>%  # Most recent first by new semester order
      select(-sem_key)
    
    disp$TagsTxt <- vapply(disp$Tags, function(x) paste(x, collapse = ", "), "")
    disp$Credit  <- as.numeric(disp$Credit)
    disp$Completed <- as.logical(disp$Completed)
    
    DT::datatable(
      disp[, c("Semester","CalendarYear","Course","Code","Credit","Completed","Grade","TagsTxt")],
      selection = "single",
      options   = list(
        pageLength = 10,
        order = list(),
        dom = 'frtip',
        scrollX = TRUE
      ),
      rownames  = FALSE,
      class     = 'cell-border stripe hover',
      colnames  = c("Semester","Year","Course Name","Code","Credits","Completed","Grade","Categories"),
      editable  = list(target = "cell", disable = list(columns = c(0,1,2,3,7)))
    ) %>%
      DT::formatStyle(
        'Completed',
        backgroundColor = DT::styleEqual(c(TRUE, FALSE), c('#d5edda', '#f8d7da'))
      ) %>%
      DT::formatStyle(
        'Grade',
        backgroundColor = DT::styleEqual(
          c('A', 'A-', 'B+', 'B', 'B-', 'C+', 'C', 'C-', 'F'),
          c('#d4edda', '#d4edda', '#fff3cd', '#fff3cd', '#fff3cd', '#f8d7da', '#f8d7da', '#f8d7da', '#f5c6cb')
        )
      )
  })
  
  next_semester_keys <- function(k, n=3) {
    # Return the next n semester keys after key k
    out <- integer(n)
    year <- floor(k / 10); semi <- k %% 10
    for (i in seq_len(n)) {
      semi <- semi + 1
      if (semi > 3) { semi <- 1; year <- year + 1 }
      out[i] <- year*10 + semi
    }
    out
  }
  
  output$recent_activity <- DT::renderDataTable({
    d <- course_data()
    if (!nrow(d)) {
      empty_df <- data.frame(Status=character(),Course=character(),Credits=numeric(),Semester=character(), stringsAsFactors = FALSE)
      return(DT::datatable(empty_df, options = list(dom = 't'), rownames = FALSE))
    }
    
    d <- d %>%
      mutate(sem_key = sem_key(Semester, CalendarYear),
             SemesterText = paste(Semester, CalendarYear))
    
    last_completed <- d %>%
      filter(Completed) %>%
      arrange(desc(sem_key)) %>%
      slice_head(n=1)
    
    if (nrow(last_completed)) {
      nxt_keys <- next_semester_keys(last_completed$sem_key[1], n=3)  # show next three semesters
      recent <- d %>%
        filter(!Completed, sem_key %in% nxt_keys) %>%
        arrange(sem_key, desc(Credit)) %>%
        mutate(Status = "⏳ Upcoming") %>%
        select(Status, Course, Credit, Semester = SemesterText)
      
      if (!nrow(recent)) {
        recent <- data.frame(Status=character(), Course=character(), Credit=numeric(), Semester=character(), stringsAsFactors = FALSE)
      }
    } else {
      keys <- sort(unique(d$sem_key))
      target <- head(keys, 3)
      recent <- d %>%
        filter(!Completed, sem_key %in% target) %>%
        arrange(sem_key, desc(Credit)) %>%
        mutate(Status = "⏳ Upcoming") %>%
        select(Status, Course, Credit, Semester = SemesterText)
    }
    
    DT::datatable(
      recent,
      options = list(
        dom = 't',
        pageLength = 8,
        scrollX = TRUE
      ),
      rownames = FALSE,
      colnames = c("Status", "Course", "Credits", "Semester")
    ) %>%
      DT::formatStyle(
        'Status',
        color = DT::styleEqual(c('⏳ Upcoming'), c('#f39c12'))
      )
  })
  
  output$performance_analysis <- DT::renderDataTable({
    d <- course_data()
    if (nrow(d) == 0 || !any(d$Completed, na.rm = TRUE)) {
      empty_df <- data.frame(
        Category = character(),
        Courses = integer(),
        Credits = numeric(),
        Avg_GPA = numeric(),
        Status = character(),
        stringsAsFactors = FALSE
      )
      return(DT::datatable(empty_df, options = list(dom = 't'), rownames = FALSE))
    }
    
    pts <- c("A"=4.0,"A-"=3.7,"B+"=3.3,"B"=3.0,"B-"=2.7,"C+"=2.3,"C"=2.0,"C-"=1.7,"F"=0.0)
    
    analysis <- expand_by_tag(d) %>%
      filter(!is.na(Tags)) %>%
      group_by(Tags) %>%
      summarise(
        Courses = n(),
        Credits = sum(Credit, na.rm = TRUE),
        Completed = sum(Completed, na.rm = TRUE),
        Avg_GPA = ifelse(
          sum(Completed & !is.na(Grade) & Grade %in% names(pts)) > 0,
          round(weighted.mean(pts[Grade[Completed & !is.na(Grade) & Grade %in% names(pts)]],
                              Credit[Completed & !is.na(Grade) & Grade %in% names(pts)], na.rm = TRUE), 2),
          NA_real_
        ),
        .groups = "drop"
      ) %>%
      mutate(
        Status = paste0(Completed, "/", Courses, " completed"),
        Avg_GPA = ifelse(is.na(Avg_GPA), "-", as.character(Avg_GPA))
      ) %>%
      select(Category = Tags, Courses, Credits, Avg_GPA, Status)
    
    DT::datatable(
      analysis,
      options = list(
        pageLength = 10,
        dom = 'frtip',
        scrollX = TRUE
      ),
      rownames = FALSE,
      colnames = c("Category", "Total Courses", "Total Credits", "Avg GPA", "Completion Status")
    ) %>%
      DT::formatRound(c("Credits"), digits = 1)
  })
  
  # Handle inline edits safely
  observeEvent(input$course_schedule_cell_edit, {
    info <- input$course_schedule_cell_edit
    d <- course_data()
    if (nrow(d) == 0) return()
    
    row <- info$row
    col <- info$col + 1  # convert to 1-based
    val <- info$value
    
    # Editable columns: 5 Credit, 6 Completed, 7 Grade
    allowed <- c(5, 6, 7)
    
    if (!(col %in% allowed)) {
      showNotification("This column is read-only.", type = "warning")
      return()
    }
    
    if (col == 5) { # Credit
      num <- suppressWarnings(as.numeric(val))
      if (is.na(num) || num < 0) {
        showNotification("Credits must be a non-negative number.", type = "error")
        return()
      }
      if (num > 30) showNotification("Credits seem high. Please verify.", type = "warning")
      d$Credit[row] <- num
    } else if (col == 6) { # Completed
      v <- tolower(trimws(as.character(val)))
      tf <- v %in% c("true","t","1","yes","y")
      ff <- v %in% c("false","f","0","no","n","")
      if (!(tf || ff)) {
        showNotification("Completed must be TRUE/FALSE (or yes/no).", type = "error")
        return()
      }
      d$Completed[row] <- tf
      if (!tf) d$Grade[row] <- NA_character_
    } else if (col == 7) { # Grade
      allowed_grades <- c("A","A-","B+","B","B-","C+","C","C-","F")
      vv <- toupper(trimws(val))
      if (!nzchar(vv)) {
        d$Grade[row] <- NA_character_
      } else if (!(vv %in% allowed_grades)) {
        showNotification(paste0("Grade must be: ", paste(allowed_grades, collapse = ", ")), type = "error")
        return()
      } else {
        d$Grade[row] <- vv
        d$Completed[row] <- TRUE
      }
    }
    
    course_data(d)
  })
  
  # ---- File Upload (Completed inferred from Grade; Completed column not required) ----
  observeEvent(input$upload_data, {
    req(input$upload_data)
    tryCatch({
      up <- read.csv(input$upload_data$datapath, stringsAsFactors = FALSE)
      
      required <- c("Semester","CalendarYear","Course","Code","Credit","Grade","Tags")
      missing <- setdiff(required, names(up))
      if (length(missing) > 0) stop("Missing columns: ", paste(missing, collapse = ", "))
      
      split_tags <- function(x) {
        if (is.na(x) || !nzchar(x)) return(character(0))
        parts <- unlist(strsplit(x, "[;,]"))
        trimws(parts[nzchar(parts)])
      }
      valid_grades <- c("A","A-","B+","B","B-","C+","C","C-","F")
      
      up <- up %>%
        mutate(
          Semester     = as.character(Semester),
          CalendarYear = as.integer(CalendarYear),
          Course       = as.character(Course),
          Code         = as.character(Code),
          Credit       = suppressWarnings(as.numeric(Credit)),
          Grade        = ifelse(is.na(Grade) | Grade=="", NA, toupper(as.character(Grade))),
          Completed    = !is.na(Grade) & Grade %in% valid_grades,
          Tags         = lapply(Tags, split_tags)
        )
      
      if (any(is.na(up$Credit))) stop("Some Credit values are not numeric.")
      up$Tags <- I(up$Tags)
      
      course_data(up[, c("Semester","CalendarYear","Course","Code","Credit","Completed","Grade","Tags")])
      showNotification("Data imported successfully!", type = "message")
    }, error = function(e) {
      msg <- paste("Import failed:", conditionMessage(e))
      showNotification(msg, type = "error")
    })
  })
  
  # ---- Timetable Functionality ----
  infer_timetable <- function(d) {
    if (!nrow(d)) {
      return(list(
        admit = "", coursework = "", comps = "", proposal = "",
        residency = "", dissertation = "", hours = "0"
      ))
    }
    
    dt <- d %>% mutate(
      sem_key = sem_key(Semester, CalendarYear),
      has_diss = vapply(Tags, function(x) any(str_detect(tolower(x), "diss|dissertation|thesis")), FALSE)
    )
    
    # Date Admitted (earliest semester)
    min_year <- min(dt$CalendarYear, na.rm = TRUE)
    dt_min_year <- dt %>% filter(CalendarYear == min_year)
    if (nrow(dt_min_year)) {
      sem_min <- dt_min_year$Semester[which.min(sem_key(dt_min_year$Semester, dt_min_year$CalendarYear))]
      admit <- paste(sem_min, min_year)
    } else admit <- as.character(min_year)
    
    # Coursework completion (last non-dissertation course)
    diss_rows <- dt %>% filter(has_diss)
    first_diss_key <- if (nrow(diss_rows)) min(diss_rows$sem_key, na.rm = TRUE) else Inf
    pre_diss <- dt %>% filter(sem_key < first_diss_key)
    if (nrow(pre_diss)) {
      i <- which.max(pre_diss$sem_key)
      coursework <- paste(pre_diss$Semester[i], pre_diss$CalendarYear[i])
    } else {
      ref <- dt %>% filter(!has_diss)
      if (nrow(ref)) {
        i <- which.max(ref$sem_key)
        coursework <- paste(ref$Semester[i], ref$CalendarYear[i])
      } else {
        coursework <- admit
      }
    }
    
    # Dissertation completion (last dissertation course)
    if (nrow(diss_rows)) {
      i <- which.max(diss_rows$sem_key)
      dissertation <- paste(diss_rows$Semester[i], diss_rows$CalendarYear[i])
    } else {
      i <- which.max(dt$sem_key)
      dissertation <- paste(dt$Semester[i], dt$CalendarYear[i])
    }
    
    # Residency: count semesters chronologically
    current_semesters <- dt %>%
      select(CalendarYear, Semester) %>%
      distinct() %>%
      arrange(CalendarYear, match(Semester, SEM_ORDER))
    residency <- if (nrow(current_semesters) > 0) nrow(current_semesters) else ""
    
    hours_completed <- sum(as.numeric(dt$Credit[dt$Completed %in% TRUE]), na.rm = TRUE)
    
    list(
      admit = admit,
      coursework = coursework,
      comps = "",
      proposal = "",
      residency = as.character(residency),
      dissertation = dissertation,
      hours = as.character(hours_completed)
    )
  }
  
  observeEvent(input$tt_infer, {
    d <- course_data()
    vals <- infer_timetable(d)
    updateTextInput(session, "tt_admit", value = vals$admit)
    updateTextInput(session, "tt_hours", value = vals$hours)
    updateTextInput(session, "tt_coursework", value = vals$coursework)
    updateTextInput(session, "tt_comps", value = vals$comps)
    updateTextInput(session, "tt_proposal", value = vals$proposal)
    updateTextInput(session, "tt_residency", value = vals$residency)
    updateTextInput(session, "tt_dissertation", value = vals$dissertation)
  })
  
  observeEvent(input$tt_clear, {
    updateTextInput(session, "tt_admit", value = "")
    updateTextInput(session, "tt_hours", value = "")
    updateTextInput(session, "tt_coursework", value = "")
    updateTextInput(session, "tt_comps", value = "")
    updateTextInput(session, "tt_proposal", value = "")
    updateTextInput(session, "tt_residency", value = "")
    updateTextInput(session, "tt_dissertation", value = "")
    updateTextAreaInput(session, "tt_notes", value = "")
  })
  
  # Auto-update hours when data changes
  observe({
    d <- course_data()
    isolate({
      hrs <- sum(as.numeric(d$Credit[d$Completed %in% TRUE]), na.rm = TRUE)
      updateTextInput(session, "tt_hours", value = as.character(hrs))
    })
  })
  
  # ---- Download Handlers ----
  output$download_template <- downloadHandler(
    filename = function() paste0("course_template_", Sys.Date(), ".csv"),
    content = function(file) {
      template <- data.frame(
        Semester = character(),
        CalendarYear = integer(),
        Course = character(),
        Code = character(),
        Credit = numeric(),
        Grade = character(),   # Completed inferred from Grade on import
        Tags = character(),
        stringsAsFactors = FALSE
      )
      write.csv(template, file, row.names = FALSE)
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() paste0("graduate_study_plan_", Sys.Date(), ".csv"),
    content = function(file) {
      out <- course_data()
      out$Tags <- vapply(out$Tags, function(x) {
        x <- gsub(";", "/", x)  # Avoid semicolons in CSV
        paste(x, collapse = ";")
      }, "")
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  # ---- Word Export ----
  make_section_table <- function(df_section) {
    if (!nrow(df_section)) {
      return(flextable(data.frame(
        COURSE = character(),
        TITLE = character(),
        CREDITS = numeric(),
        GRADE = character(),
        DATE = character(),
        check.names = FALSE
      )))
    }
    
    tab <- df_section %>%
      transmute(
        COURSE = Code,
        TITLE = Course,
        CREDITS = as.numeric(Credit),
        GRADE = ifelse(is.na(Grade), "-", Grade),
        DATE = paste(Semester, CalendarYear)
      )
    
    ft <- flextable(tab)
    ft <- autofit(ft)
    ft <- align(ft, align = "left", part = "all")
    ft <- theme_vanilla(ft)
    ft
  }
  
  output$download_docx <- downloadHandler(
    filename = function() paste0("Plan_of_Study_", Sys.Date(), ".docx"),
    content = function(file) {
      d <- course_data()
      if (!nrow(d)) {
        showNotification("No data to export.", type = "warning")
        stop("No data")
      }
      
      exp <- expand_by_tag(d)
      tags_all <- unique(exp$Tags)
      has_untagged <- any(is.na(tags_all))
      tags_clean <- sort(unique(na.omit(tags_all)))
      if (has_untagged) tags_clean <- c(tags_clean, "Untagged")
      
      doc <- read_docx()
      doc <- body_add_par(doc, "GRADUATE PLAN OF STUDY", style = "heading 1")
      doc <- body_add_par(doc, paste("Generated:", format(Sys.Date(), "%B %d, %Y")), style = "Normal")
      doc <- body_add_par(doc, "", style = "Normal")
      
      total_overall <- 0
      summary_rows <- list()
      
      for (tg in tags_clean) {
        if (tg == "Untagged") {
          df_tag <- d %>% filter(lengths(Tags) == 0)
        } else {
          df_tag <- exp %>% filter(Tags == tg) %>% select(-Tags)
        }
        
        if (nrow(df_tag) > 0) {
          total_tag <- sum(as.numeric(df_tag$Credit), na.rm = TRUE)
          header <- sprintf("%s (%s credits)", tg, total_tag)
          doc <- body_add_par(doc, header, style = "heading 2")
          doc <- body_add_flextable(doc, make_section_table(df_tag))
          doc <- body_add_par(doc, "", style = "Normal")
          
          total_overall <- total_overall + total_tag
          summary_rows[[length(summary_rows) + 1]] <- data.frame(
            Category = tg,
            Credits = total_tag,
            check.names = FALSE
          )
        }
      }
      
      doc <- body_add_par(doc, "SUMMARY", style = "heading 2")
      summary_df <- if (length(summary_rows)) bind_rows(summary_rows) else data.frame(Category=character(), Credits=numeric())
      summary_df <- bind_rows(summary_df, data.frame(Category = "TOTAL", Credits = total_overall))
      doc <- body_add_flextable(doc, theme_vanilla(autofit(flextable(summary_df))))
      doc <- body_add_par(doc, "", style = "Normal")
      
      hours_completed <- sum(as.numeric(d$Credit[d$Completed %in% TRUE]), na.rm = TRUE)
      tt <- data.frame(
        Milestone = c("Date Admitted:", "Hours Completed to Date", "Completion of Coursework",
                      "Comprehensive Exam", "Dissertation Proposal", "Semesters of Residency",
                      "Completion of Dissertation", "Notes"),
        Details = c(input$tt_admit %||% "",
                    hours_completed,
                    input$tt_coursework %||% "",
                    input$tt_comps %||% "",
                    input$tt_proposal %||% "",
                    input$tt_residency %||% "",
                    input$tt_dissertation %||% "",
                    input$tt_notes %||% ""),
        check.names = FALSE
      )
      doc <- body_add_par(doc, "PROJECTED TIMELINE", style = "heading 2")
      doc <- body_add_flextable(doc, theme_vanilla(autofit(flextable(tt))))
      
      print(doc, target = file)
    }
  )
  
  # ---- Auto-save functionality ----
  observe({
    d <- course_data()
    courses_json <- toJSON(d, dataframe = "rows", null = "null", auto_unbox = TRUE)
    payload <- toJSON(list(schema = "v1", courses = fromJSON(courses_json)),
                      auto_unbox = TRUE, null = "null")
    session$sendCustomMessage("saveState", payload)
  })
  
  observeEvent(input$storage_error, {
    showNotification("Auto-save unavailable. Use CSV export to backup your work.", type = "warning")
  })
  
  # ---- Restore from localStorage ----
  observeEvent(input$restore_state, {
    if (isTRUE(has_restored())) return()
    json <- input$restore_state
    if (!nzchar(json)) return()
    
    ok <- TRUE
    res <- tryCatch(fromJSON(json), error = function(e){ ok <<- FALSE; NULL })
    if (!ok || is.null(res$courses)) return()
    
    cdf <- as.data.frame(res$courses, stringsAsFactors = FALSE)
    need_cols <- c("Semester","CalendarYear","Course","Code","Credit","Completed","Grade","Tags")
    miss <- setdiff(need_cols, names(cdf))
    if (length(miss)) return()
    
    cdf$Semester <- as.character(cdf$Semester)
    cdf$CalendarYear <- as.integer(cdf$CalendarYear)
    cdf$Course <- as.character(cdf$Course)
    cdf$Code <- as.character(cdf$Code)
    cdf$Credit <- as.numeric(cdf$Credit)
    cdf$Completed <- as.logical(cdf$Completed)
    cdf$Grade <- ifelse(is.na(cdf$Grade) | cdf$Grade=="", NA, as.character(cdf$Grade))
    if (!is.list(cdf$Tags)) cdf$Tags <- lapply(cdf$Tags, function(x) as.character(x %||% character(0)))
    cdf$Tags <- I(cdf$Tags)
    
    course_data(cdf)
    has_restored(TRUE)
    showNotification("Previous session restored", type = "message")
  }, ignoreInit = TRUE, once = TRUE)
  
  # ---- Reset all data (Danger Zone) ----
  observeEvent(input$reset_all, {
    showModal(modalDialog(
      title = "Confirm Reset",
      "This will clear all courses from this browser. Continue?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reset", "Yes, reset", class = "btn-danger")
      )
    ))
  })
  observeEvent(input$confirm_reset, {
    removeModal()
    course_data(course_data()[0,])  # empty but same cols
    session$sendCustomMessage("saveState", jsonlite::toJSON(list(schema="v1", courses=list()), auto_unbox=TRUE))
    showNotification("All data cleared.", type = "message")
  })
}

# ---- Run Application ----
shinyApp(ui = ui, server = server)
