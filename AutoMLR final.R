library(shiny)
library(bslib)
library(shinyjs)
library(ggplot2)
library(htmltools)

ui <- page_navbar(
  title = "autoMLR",
  theme = bs_theme(version = 5, bootswatch = "minty"),

  header = tags$head(
    shinyjs::useShinyjs(),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    tags$style(HTML("
      .search-container { position: relative; width: 250px; }
      .search-container i { position: absolute; left: 12px; top: 50%; transform: translateY(-50%); color: #aaa; z-index: 10; }
      .search-container input { padding-left: 32px !important; }
      .source-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(180px, 1fr)); gap: 12px; max-height: 400px; overflow-y: auto; padding: 5px; }
      .source-item-btn { border: 1px solid #e2e8f0; border-radius: 6px; padding: 15px; text-align: center; background: #fff; width: 100%; display: block; transition: all 0.2s; cursor: pointer; text-decoration: none !important; }
      .source-item-btn:hover { border-color: #10b981; background: #f0fdf4; transform: translateY(-2px); }
      .source-item-btn.selected-item { border: 2px solid #10b981 !important; background: #e6fbf1 !important; }
      .source-item-btn i { font-size: 2rem; margin-bottom: 8px; display: block; }
      .icon-excel { color: #107c41; }
      .icon-json { color: #e15729; }
      .icon-access { color: #a4373a; }
      .icon-sql { color: #cc292b; }
      .icon-snowflake { color: #29b5e8; }
      .icon-salesforce { color: #00a1e0; }
      .icon-google { color: #4285f4; }
      .icon-azure { color: #0078d4; }
      .icon-python { color: #3776ab; }
      .icon-r { color: #276dc3; }
      .source-ext-label { display: block; font-size: 0.75rem; color: #94a3b8; margin-top: 2px; }
      .pbi-table-container { overflow-x: auto; max-height: 400px; overflow-y: auto; border: 1px solid #dee2e6; background: #fff; }
      .pbi-table { width: 100%; border-collapse: collapse; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; font-size: 0.82rem; }
      .pbi-table th { background: #f8f9fa; border: 1px solid #dee2e6; padding: 6px; font-weight: normal; vertical-align: top; min-width: 160px; position: sticky; top: 0; z-index: 5; cursor: pointer; user-select: none; }
      .pbi-table th:hover { background: #f1f5f9; }
      .pbi-table td { border: 1px solid #e2e8f0; padding: 4px 8px; white-space: nowrap; max-width: 220px; overflow: hidden; text-overflow: ellipsis; color: #334155; }
      .pbi-table th.selected-col-header { background: #e2e8f0 !important; border-bottom: 3px solid #0284c7 !important; font-weight: 600; }
      .pbi-table td.selected-col-cell { background-color: #f1f5f9 !important; }
      .header-top-row { display: flex; justify-content: space-between; align-items: center; margin-bottom: 4px; pointer-events: none; }
      .header-title-text { font-weight: 600; color: #1e293b; overflow: hidden; text-overflow: ellipsis; }
      .header-type-label { font-weight: 700; color: #64748b; font-size: 0.7rem; background: #e2e8f0; padding: 1px 4px; border-radius: 3px; }
      .quality-bar-wrapper { margin-top: 4px; margin-bottom: 4px; pointer-events: none; }
      .quality-bar { display: flex; height: 6px; border-radius: 2px; overflow: hidden; background: #e2e8f0; }
      .q-valid { background-color: #10b981; }
      .q-empty { background-color: #94a3b8; }
      .q-error { background-color: #ef4444; }
      .quality-text-legend { font-size: 0.72rem; color: #475569; display: flex; justify-content: space-between; margin-top: 1px; }
      .mini-dist-container { margin-top: 6px; border-top: 1px dashed #cbd5e1; padding-top: 4px; pointer-events: none; }
      .mini-bar-chart { display: flex; align-items: flex-end; height: 32px; gap: 2px; margin-bottom: 3px; background: #fafafa; padding: 2px; border-radius: 2px; }
      .mini-bar { background-color: #0ea5e9; flex-grow: 1; min-width: 3px; border-radius: 1px 1px 0 0; }
      .mini-dist-counts { font-size: 0.72rem; color: #64748b; font-style: italic; text-align: left; }
    "))
  ),

  nav_panel(
    title = "Data Cleaner",
    layout_sidebar(
      sidebar = sidebar(
        title = "DATA SOURCE",
        width = 300,

        fileInput("file_input", "Drop file here",
                  accept = c(".csv"),
                  buttonLabel = "Browse...",
                  placeholder = "CSV • XLSX • JSON • Parquet"),

        actionButton("btn_clear_file", "❌ Clear Current File",
                     class = "btn-outline-danger w-100 btn-sm",
                     style = "margin-top: -10px; margin-bottom: 15px;"),

        tags$hr(),
        tags$h5("Connections"),

        actionButton("btn_open_hub", "⚡ Connect with Source Data",
                     class = "btn-outline-secondary w-100 text-start",
                     style = "border-color: #ced4da;"),

        tags$hr(),

        tags$h5("Loaded dataset"),
        strong(textOutput("dataset_name_ui")),
        span(textOutput("dataset_dims_ui"), style = "color: #666; font-size: 0.9rem;"),

        br(), br(), br(),

        actionButton("btn_export", "↓ Export clean data", class = "btn-outline-dark w-100")
      ),

      navset_pill(
        id = "main_tabs",
        nav_panel("Overview",
                  br(),
                  layout_columns(
                    value_box(title = "Total rows", value = textOutput("total_rows"), theme = "success"),
                    value_box(title = "Columns", value = textOutput("total_cols"), theme = "light"),
                    value_box(title = "Missing values", value = textOutput("missing_count"), theme = "light"),
                    value_box(title = "Columns with issues", value = textOutput("columns_issues"), theme = "danger"),
                    col_widths = c(3, 3, 3, 3)
                  ),
                  br(),
                  div(
                    style = "display: flex; justify-content: space-between; align-items: center;",
                    tags$h4("Column health overview"),
                    span("Scroll to review all", class = "badge bg-light text-dark", style = "border: 1px solid #ddd;")
                  ),
                  br(),
                  uiOutput("column_cards_container"),
                  br(),
                  div(
                    style = "display: flex; justify-content: space-between; align-items: center; border-top: 1px solid #ddd; padding-top: 15px;",
                    span(textOutput("pending_actions_count_ui"), style = "color: #666; font-size: 0.9rem;"),
                    div(
                      actionButton("btn_review", "Review actions", class = "btn-outline-secondary me-2"),
                      actionButton("btn_apply", "Apply cleaning ↗", class = "btn-success")
                    )
                  )
        ),

        nav_panel("Columns",
                  br(),
                  card(
                    card_header(
                      div(style = "display: flex; gap: 25px; align-items: center; flex-wrap: wrap;",
                          strong("Data View Structure Options:", style = "font-size: 0.95rem; color:#475569;"),
                          checkboxInput("pbi_quality", "Column quality", value = TRUE),
                          checkboxInput("pbi_dist", "Column distribution", value = FALSE)
                      )
                    ),
                    div(class = "pbi-table-container",
                        uiOutput("power_bi_table")
                    )
                  ),

                  br(),
                  layout_columns(
                    col_widths = c(4, 4, 4),
                    card(
                      card_header(strong("Column statistics")),
                      uiOutput("column_stats_table_ui")
                    ),
                    card(
                      card_header(strong("Smart Cleaning Toolkit")),
                      uiOutput("column_transform_ui")
                    ),
                    card(
                      card_header(strong("Value distribution")),
                      plotOutput("profile_histogram_plot", height = "260px")
                    )
                  )
        ),

        nav_panel("Preview",
                  br(),
                  card(
                    card_header(
                      div(style = "display: flex; justify-content: space-between; align-items: center;",
                          strong("Dataset Preview Panel (First 500 rows)"),
                          radioButtons("preview_view_type", "Filter rows:",
                                       choices = c("All Data" = "all", "Nulls Only" = "nulls"),
                                       selected = "all", inline = TRUE)
                      )
                    ),
                    div(style = "overflow: auto; max-height: 450px; padding: 10px;",
                        tableOutput("preview_data_table")
                    )
                  )
        ),
        nav_panel("Pending actions",
                  br(),
                  card(
                    card_header(strong("Recorded Cleaning Operations & History Log")),
                    div(style = "overflow: auto; max-height: 450px; padding: 10px;",
                        tableOutput("pending_actions_table")
                    )
                  )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  file_reset_trigger <- reactiveVal(FALSE)
  selected_source <- reactiveVal(NULL)
  active_profile_col <- reactiveVal(NULL)
  connected_data <- reactiveVal(NULL)
  connected_data_name <- reactiveVal(NULL)

  # Tracks dataset updates dynamically
  modified_data <- reactiveVal(NULL)

  # Tracks action rows
  actions_log <- reactiveVal(data.frame(
    Time = character(),
    Column = character(),
    Action = character(),
    Details = character(),
    stringsAsFactors = FALSE
  ))

  # Resets variables and text boxes on clear button interaction
  observeEvent(input$btn_clear_file, {
    shinyjs::runjs("
      $('#file_input').val('');
      $('#file_input_placeholder').val('');
      $('.progress-bar').css('width', '0%').text('');
      $('.progress').hide();
      $('.shiny-input-container .input-group .form-control').val('');
    ")
    file_reset_trigger(TRUE)
    connected_data(NULL)
    connected_data_name(NULL)
    active_profile_col(NULL)
    modified_data(NULL)
    actions_log(data.frame(Time=character(), Column=character(), Action=character(), Details=character(), stringsAsFactors=FALSE))
    showNotification("Dataset cleared successfully.", type = "warning")
  })

  observeEvent(input$file_input, {
    file_reset_trigger(FALSE)
  })

  # Reactive dataframe file loader
  uploaded_data <- reactive({
    if (!is.null(connected_data())) {
      return(connected_data())
    }
    if (file_reset_trigger()) return(NULL)
    req(input$file_input)

    df <- read.csv(input$file_input$datapath, stringsAsFactors = FALSE)
    if(ncol(df) > 0 && is.null(active_profile_col())) {
      active_profile_col(colnames(df)[1])
    }
    df
  })

  observeEvent(uploaded_data(), {
    req(uploaded_data())
    modified_data(uploaded_data())
    actions_log(data.frame(Time=character(), Column=character(), Action=character(), Details=character(), stringsAsFactors=FALSE))
  })

  current_working_data <- reactive({
    if (!is.null(modified_data())) {
      return(modified_data())
    }
    uploaded_data()
  })

  # Syncs custom JS table click actions directly into active tracking variable
  observeEvent(input$pbi_col_clicked, {
    active_profile_col(input$pbi_col_clicked)
  })

  # --- HUBS / POPUPS MANAGER ---
  observeEvent(input$btn_open_hub, {
    selected_source(NULL)
    showModal(modalDialog(
      title = div(style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
                  tags$span(strong("Get Data"), style = "font-size: 1.3rem;"),
                  div(class = "search-container",
                      tags$i(class = "fa fa-search"),
                      textInput("search_source", NULL, placeholder = "Search data sources...", width = "100%")
                  )
      ),
      size = "l",
      easyClose = TRUE,
      uiOutput("filtered_sources_ui"),
      footer = tagList(modalButton("Cancel"), uiOutput("connect_btn_ui"))
    ))
  })

  all_source_ids <- c("excel", "json", "access", "sqldb", "snow", "sfobj", "sfrep", "gbq", "databr", "py", "rscript")
  lapply(all_source_ids, function(id) {
    observeEvent(input[[paste0("select_", id)]], { selected_source(id) })
  })

  output$connect_btn_ui <- renderUI({
    if (is.null(selected_source())) {
      actionButton("btn_modal_connect", "Connect", class = "btn btn-success", disabled = TRUE)
    } else {
      actionButton("btn_modal_connect", "Connect", class = "btn btn-success")
    }
  })

  # Launches unique connection configurations matching Power BI setups
  observeEvent(input$btn_modal_connect, {
    req(selected_source())
    src <- selected_source()
    removeModal()

    form_fields <- switch(src,
                          "excel" = tagList(
                            fileInput("pbi_src_file", "File path / Browse", accept = c(".xlsx", ".xls", ".xlsm")),
                            checkboxInput("pbi_first_row", "Use first row as headers", value = TRUE)
                          ),
                          "json" = tagList(
                            fileInput("pbi_src_file", "File path / Browse", accept = c(".json")),
                            textInput("pbi_json_root", "JSON Root Element Path (Optional)")
                          ),
                          "access" = tagList(
                            fileInput("pbi_src_file", "Database filename path", accept = c(".accdb", ".mdb")),
                            passwordInput("pbi_db_pass", "Database Password (if encrypted)")
                          ),
                          "sqldb" = tagList(
                            textInput("pbi_srv", "Server Address", placeholder = "e.g., localhost\\\\SQLEXPRESS or sql.domain.com"),
                            textInput("pbi_db", "Database (Optional)"),
                            radioButtons("pbi_mode", "Data Connectivity Mode", choices = c("Import", "DirectQuery"), selected = "Import"),
                            tags$hr(),
                            textAreaInput("pbi_sql_statement", "SQL Statement / Query (Optional)", rows = 5, placeholder = "SELECT * FROM my_table", width = "100%")
                          ),
                          "snow" = tagList(
                            textInput("pbi_snow_url", "Server URL", placeholder = "e.g., xy12345.east-us-2.azure.snowflakecomputing.com"),
                            textInput("pbi_wh", "Warehouse"),
                            textInput("pbi_db", "Database (Optional)"),
                            tags$hr(),
                            textAreaInput("pbi_snow_statement", "SQL Statement / Query (Optional)", rows = 5, placeholder = "SELECT * FROM my_table", width = "100%")
                          ),
                          "sfobj" = tagList(
                            radioButtons("pbi_sf_env_obj", "Salesforce Environment", choices = c("Production", "Custom/Sandbox")),
                            conditionalPanel(
                              condition = "input.pbi_sf_env_obj == 'Custom/Sandbox'",
                              textInput("pbi_sf_url_obj", "Custom Login URL", placeholder = "e.g., https://my-company.sandbox.my.salesforce.com")
                            ),
                            checkboxInput("pbi_sf_rel", "Include relationship columns", value = TRUE)
                          ),
                          "sfrep" = tagList(
                            radioButtons("pbi_sf_env_rep", "Salesforce Environment", choices = c("Production", "Custom/Sandbox")),
                            conditionalPanel(
                              condition = "input.pbi_sf_env_rep == 'Custom/Sandbox'",
                              textInput("pbi_sf_url_rep", "Custom Login URL", placeholder = "e.g., https://my-company.sandbox.my.salesforce.com")
                            ),
                            textInput("pbi_sf_rep_id", "Specific Report Unique ID")
                          ),
                          "gbq" = tagList(
                            textInput("pbi_gbq_proj", "Project ID", placeholder = "e.g., my-google-project"),
                            tags$div(style = "margin-top: 15px; margin-bottom: 10px;",
                                     checkboxInput("pbi_gbq_adv_toggle", strong("Advanced options"), value = FALSE)
                            ),
                            conditionalPanel(
                              condition = "input.pbi_gbq_adv_toggle == true",
                              tags$div(style = "padding-left: 15px; border-left: 2px solid #cbd5e1; margin-bottom: 15px;",
                                       textAreaInput("pbi_gbq_sql", "SQL statement", rows = 10, placeholder = "SELECT * FROM `project.dataset.table`", width = "100%"),
                                       textInput("pbi_gbq_billing", "Billing project ID (Optional)"),
                                       numericInput("pbi_gbq_limit", "Row limit (Optional)", value = NA)
                              )
                            ),
                            span("Connection will utilize your browser context for OAuth2 validation.", style = "font-size:0.8rem; color:gray;")
                          ),
                          "databr" = tagList(
                            textInput("pbi_srv", "Server Hostname URL"),
                            textInput("pbi_db", "HTTP Path"),
                            passwordInput("pbi_pass", "Personal Access Token (PAT)"),
                            tags$hr(),
                            textAreaInput("pbi_db_statement", "SQL Statement / Query (Optional)", rows = 5, placeholder = "SELECT * FROM my_table", width = "100%")
                          ),
                          "py" = tagList(
                            textAreaInput("pbi_script_area_py", "Python Script Execution Body", rows = 15, placeholder = "import pandas as pd\\ndf = pd.read_csv('...')", width = "100%"),
                            span("Execution is processed locally within sandbox instance memory blocks.", style = "font-size:0.8rem; color:gray;")
                          ),
                          "rscript" = tagList(
                            textAreaInput("pbi_script_area_r", "R Script Execution Body", rows = 15, placeholder = "df <- read.csv('...')", width = "100%"),
                            span("Execution is processed locally within sandbox instance memory blocks.", style = "font-size:0.8rem; color:gray;")
                          )
    )

    showModal(modalDialog(
      title = paste("Connect to", switch(src, "excel"="Excel Workbook", "json"="JSON File", "access"="Access Database", "sqldb"="SQL Server Database", "snow"="Snowflake Storage Engine", "sfobj"="Salesforce Objects Matrix", "sfrep"="Salesforce Analytical Report", "gbq"="Google BigQuery Warehouse", "databr"="Azure Databricks Link", "py"="Python Script Platform", "rscript"="R Script Platform")),
      size = "m",
      easyClose = FALSE,
      form_fields,
      footer = tagList(
        actionButton("btn_back_to_hub", "← Back", class = "btn btn-outline-secondary"),
        modalButton("Cancel"),
        actionButton("btn_finalize_connect", "Connect", class = "btn btn-success")
      )
    ))
  })

  observeEvent(input$btn_back_to_hub, {
    removeModal()
    click("btn_open_hub")
  })

  output$filtered_sources_ui <- renderUI({
    search_term <- if (!is.null(input$search_source)) tolower(input$search_source) else ""
    all_sources <- list(
      list(id = "excel",   name = "Excel workbook",        ext = ".xlsx, .xls, .xlsm",    icon = "fa-file-excel icon-excel"),
      list(id = "json",    name = "JSON file",             ext = ".json, .txt",           icon = "fa-code icon-json"),
      list(id = "access",  name = "Access database",       ext = ".accdb, .mdb",          icon = "fa-database icon-access"),
      list(id = "sqldb",   name = "SQL Server database",   ext = "SQL Engine Tables",     icon = "fa-database icon-sql"),
      list(id = "snow",    name = "Snowflake",             ext = "Snowflake Cloud Wh",    icon = "fa-snowflake icon-snowflake"),
      list(id = "sfobj",   name = "Salesforce Objects",    ext = "Cloud CRM Tables",      icon = "fa-cloud icon-salesforce"),
      list(id = "sfrep",   name = "Salesforce Reports",    ext = "CRM Tabular Reports",   icon = "fa-chart-bar icon-salesforce"),
      list(id = "gbq",     name = "Google BigQuery",       ext = "BigQuery Analytics",    icon = "fa-cloud-meatball icon-google"),
      list(id = "databr",  name = "Azure Databricks",      ext = "Spark Cluster Lake",    icon = "fa-fire icon-azure"),
      list(id = "py",      name = "Python script",         ext = "Local script run",      icon = "fa-brands fa-python icon-python"),
      list(id = "rscript", name = "R script",              ext = "Local script run",      icon = "fa-brands fa-r-project icon-r")
    )
    matched <- Filter(function(s) search_term == "" || grepl(search_term, tolower(s$name)) || grepl(search_term, tolower(s$ext)), all_sources)
    if (length(matched) == 0) {
      return(div(style = "text-align: center; padding: 40px; color: #94a3b8;",
                 tags$i(class = "fa fa-search-minus", style = "font-size: 3rem; margin-bottom: 10px;"),
                 p("No matching data sources found.")))
    }
    div(class = "source-grid",
        lapply(matched, function(s) {
          is_selected <- !is.null(selected_source()) && selected_source() == s$id
          class_string <- if (is_selected) "source-item-btn selected-item" else "source-item-btn"
          actionLink(paste0("select_", s$id),
                     label = div(class = class_string,
                                 tags$i(class = paste("fa", s$icon)),
                                 tags$span(s$name, style = "color: #334155; font-weight: 500; font-size: 0.9rem; display:block;"),
                                 tags$span(s$ext, class = "source-ext-label")
                     )
          )
        })
    )
  })

  # Basic overview textual stats
  output$dataset_name_ui <- renderText({
    if (!is.null(connected_data_name())) return(connected_data_name())
    if (is.null(current_working_data())) "No file loaded" else input$file_input$name
  })
  output$dataset_dims_ui <- renderText({ if (is.null(current_working_data())) "0 rows • 0 columns" else paste(nrow(current_working_data()), "rows •", ncol(current_working_data()), "columns") })
  output$total_rows     <- renderText({ if (is.null(current_working_data())) "0" else nrow(current_working_data()) })
  output$total_cols     <- renderText({ if (is.null(current_working_data())) "0" else ncol(current_working_data()) })
  output$missing_count  <- renderText({ if (is.null(current_working_data())) "0" else sum(is.na(current_working_data()) | as.character(current_working_data()) == "") })
  output$columns_issues <- renderText({ if (is.null(current_working_data())) "0" else sum(sapply(current_working_data(), function(col) any(is.na(col) | as.character(col) == ""))) })

  # --- OVERVIEW DATA DISPLAY CARDS ---
  output$column_cards_container <- renderUI({
    df <- current_working_data()
    if (is.null(df)) return(p("Please upload a dataset file to generate column evaluations.", style = "color: gray; font-style: italic;"))

    col_names <- colnames(df)
    card_list <- lapply(col_names, function(name) {
      column_data <- df[[name]]
      na_count <- sum(is.na(column_data) | as.character(column_data) == "")
      na_pct <- round((na_count / length(column_data)) * 100, 1)
      dist_text <- "N/A (Categorical)"
      outlier_text <- "No outliers"
      outlier_badge <- "bg-success"

      if (is.numeric(column_data)) {
        clean_data <- na.omit(column_data)
        if (length(clean_data) > 3) {
          m3 <- mean((clean_data - mean(clean_data))^3); s3 <- sd(clean_data)^3; skewness <- if (s3 > 0) m3 / s3 else 0
          dist_text <- if (abs(skewness) < 0.5) "Normal" else if (skewness >= 0.5) "Right-Skewed" else "Left-Skewed"
          q25 <- quantile(clean_data, 0.25); q75 <- quantile(clean_data, 0.75); iqr <- q75 - q25
          outliers <- clean_data[clean_data < (q25 - 1.5*iqr) | clean_data > (q75 + 1.5*iqr)]
          outlier_pct <- round((length(outliers) / length(clean_data)) * 100, 1)
          if (length(outliers) > 0) {
            outlier_text <- paste0(if(outlier_pct > 5) "Big" else "Minor", " Outliers (", outlier_pct, "%)")
            outlier_badge <- if(outlier_pct > 5) "bg-danger" else "bg-warning text-dark"
          }
        }
      }

      status_class <- if (na_pct > 0) "badge bg-warning text-dark" else "badge bg-success"
      fill_val <- 100 - na_pct

      card(
        style = "margin-bottom: 15px; box-shadow: 0 1px 3px rgba(0,0,0,0.05); border-left: 4px solid #10b981;",
        card_body(
          div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px;",
              tags$strong(name, style = "font-size: 1.1rem; color: #2c3e50;"),
              span(if (na_pct > 0) "Review" else "Clean", class = status_class)
          ),
          div(style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 10px; font-size: 0.85rem; margin-bottom: 10px;",
              div(tags$span("Nulls: "), tags$strong(paste0(na_pct, "%"))),
              div(tags$span("Distribution: "), tags$strong(dist_text)),
              div(tags$span("Outliers: "), span(outlier_text, class = paste("badge", outlier_badge)))
          ),
          div(class = "progress", style = "height: 8px;",
              div(class = paste("progress-bar", if(na_pct > 0) "bg-warning" else "bg-success"), style = paste0("width: ", fill_val, "%;"))
          )
        )
      )
    })
    do.call(tagList, card_list)
  })

  output$pending_actions_count_ui <- renderText({
    n <- nrow(actions_log())
    paste(n, "transformations completed & logged. View history on the Pending Actions page.")
  })

  observeEvent(input$btn_review, {
    updateNavsetPill(session, "main_tabs", selected = "Pending actions")
  })

  observeEvent(input$btn_apply, {
    showNotification("All current cleaning logic passes successfully locked into state!", type = "success")
  })

  # --- CUSTOM POWER QUERY DATA TABLE GENERATOR ---
  output$power_bi_table <- renderUI({
    df <- current_working_data()
    if (is.null(df)) return(p("No data loaded. Go to Overview to upload a file.", style = "padding: 20px; font-style: italic; color: #64748b;"))

    preview_df <- head(df, 1000)
    col_names <- colnames(preview_df)
    selected_col <- active_profile_col()

    headers <- lapply(col_names, function(col) {
      col_vec <- df[[col]]
      total_items <- length(col_vec)

      ui_type <- if(is.numeric(col_vec)) "1.2" else "A B C"

      na_count <- sum(is.na(col_vec) | as.character(col_vec) == "")
      empty_pct <- round((na_count / total_items) * 100)
      valid_pct <- 100 - empty_pct

      distinct_cnt <- length(unique(col_vec))
      unique_cnt <- sum(table(col_vec) == 1)

      is_selected_class <- if(!is.null(selected_col) && selected_col == col) "selected-col-header" else ""

      table_counts <- table(head(col_vec, 100))
      max_count <- if(length(table_counts) > 0) max(table_counts) else 1

      tags$th(
        class = is_selected_class,
        onclick = sprintf("Shiny.setInputValue('pbi_col_clicked', '%s', {priority: 'event'});", col),

        div(class = "header-top-row",
            div(class = "header-title-text", col),
            span(ui_type, class = "header-type-label")
        ),

        if(input$pbi_quality) {
          div(class = "quality-bar-wrapper",
              div(class = "quality-bar",
                  div(class = "q-valid", style = paste0("width: ", valid_pct, "%;")),
                  div(class = "q-empty", style = paste0("width: ", empty_pct, "%;"))
              ),
              div(class = "quality-text-legend",
                  span(paste0("Valid: ", valid_pct, "%")),
                  span(paste0("Empty: ", empty_pct, "%"))
              )
          )
        },

        if(input$pbi_dist) {
          div(class = "mini-dist-container",
              div(class = "mini-bar-chart",
                  lapply(head(as.numeric(table_counts), 8), function(val) {
                    pct_h <- min(100, max(15, round((val / max_count) * 100)))
                    div(class = "mini-bar", style = paste0("height: ", pct_h, "%;"))
                  })
              ),
              div(class = "mini-dist-counts",
                  paste0(distinct_cnt, " distinct, ", unique_cnt, " unique")
              )
          )
        }
      )
    })

    rows <- NULL
    if (nrow(preview_df) > 0) {
      rows <- lapply(1:nrow(preview_df), function(i) {
        tags$tr(
          lapply(col_names, function(col) {
            val <- preview_df[i, col]
            cell_class <- if(!is.null(selected_col) && selected_col == col) "selected-col-cell" else ""
            tags$td(class = cell_class, if(is.na(val) || val == "") tags$em("null", style="color:#cbd5e1;") else htmlEscape(as.character(val)))
          })
        )
      })
    }

    tags$table(class = "pbi-table", tags$thead(tags$tr(headers)), tags$tbody(rows))
  })

  # --- INTERACTIVE COLUMN TRANSFORMATION PANEL ---
  output$column_transform_ui <- renderUI({
    df <- current_working_data()
    req(df)
    col <- active_profile_col()

    if (is.null(col) || !(col %in% colnames(df))) {
      return(p("Click a header column to begin transformation configurations.", style = "color: gray; font-style: italic;"))
    }

    is_num <- is.numeric(df[[col]])

    tagList(
      p(HTML(paste0("Active Column Target: <strong>", htmlEscape(col), "</strong>"))),

      selectInput("trans_type", "Convert Column Data Type:",
                  choices = c("No Change" = "none", "Numeric" = "numeric", "Text/Character" = "character", "Factor" = "factor")),

      checkboxInput("trans_trim", "Trim Whitespace (Leading/Trailing)", value = FALSE),

      selectInput("trans_case", "Modify String Case Structure:",
                  choices = c("No Change" = "none", "UPPERCASE" = "upper", "lowercase" = "lower", "Title Case" = "title")),

      tags$hr(),
      tags$h6("Handle Missing Values"),
      if (is_num) {
        selectInput("impute_numeric_col", "For This Numeric Column:",
                    choices = c("Keep Nulls" = "keep", "Replace with Mean" = "mean", "Replace with Median" = "median", "Drop Rows" = "drop"))
      } else {
        textInput("impute_text_val_col", "For This Text Column (Replace with word):", value = "Unknown")
      },

      br(),
      actionButton("btn_apply_col_trans", "Execute Column Transforms", class = "btn-primary w-100 btn-sm")
    )
  })

  observeEvent(input$btn_apply_col_trans, {
    col <- active_profile_col()
    req(col)
    df <- current_working_data()
    req(df)

    actions <- c()
    details_log <- c()

    # 1. Handling Whitespace Trim
    if (input$trans_trim) {
      df[[col]] <- trimws(as.character(df[[col]]))
      actions <- c(actions, "Trimmed whitespace")
      details_log <- c(details_log, "Trimmed leading/trailing spaces")
    }

    # 2. Handling Case Transformations
    if (input$trans_case == "upper") {
      df[[col]] <- toupper(as.character(df[[col]]))
      actions <- c(actions, "Changed Case")
      details_log <- c(details_log, "Changed case to UPPERCASE")
    } else if (input$trans_case == "lower") {
      df[[col]] <- tolower(as.character(df[[col]]))
      actions <- c(actions, "Changed Case")
      details_log <- c(details_log, "Changed case to lowercase")
    } else if (input$trans_case == "title") {
      df[[col]] <- tools::toTitleCase(tolower(as.character(df[[col]])))
      actions <- c(actions, "Changed Case")
      details_log <- c(details_log, "Changed case to Title Case")
    }

    # 3. Handling Type Conversions
    if (input$trans_type == "numeric") {
      df[[col]] <- as.numeric(df[[col]])
      actions <- c(actions, "Convert Type")
      details_log <- c(details_log, "Converted layout profile type to Numeric")
    } else if (input$trans_type == "character") {
      df[[col]] <- as.character(df[[col]])
      actions <- c(actions, "Convert Type")
      details_log <- c(details_log, "Converted layout profile type to Text")
    } else if (input$trans_type == "factor") {
      df[[col]] <- as.factor(df[[col]])
      actions <- c(actions, "Convert Type")
      details_log <- c(details_log, "Converted layout profile type to Factor")
    }

    # 4. Handling Column Imputation Rules
    col_vec <- df[[col]]
    if (is.numeric(col_vec)) {
      req(input$impute_numeric_col)
      na_indices <- which(is.na(col_vec))
      if (length(na_indices) > 0 && input$impute_numeric_col != "keep") {
        if (input$impute_numeric_col == "mean") {
          m_val <- mean(col_vec, na.rm = TRUE)
          if (is.nan(m_val)) m_val <- 0
          df[na_indices, col] <- m_val
          actions <- c(actions, "Impute Numeric")
          details_log <- c(details_log, paste("Replaced NULLs with Mean =", round(m_val, 2)))
        } else if (input$impute_numeric_col == "median") {
          med_val <- median(col_vec, na.rm = TRUE)
          if (is.na(med_val)) med_val <- 0
          df[na_indices, col] <- med_val
          actions <- c(actions, "Impute Numeric")
          details_log <- c(details_log, paste("Replaced NULLs with Median =", round(med_val, 2)))
        } else if (input$impute_numeric_col == "drop") {
          df <- df[-na_indices, , drop = FALSE]
          actions <- c(actions, "Drop Rows")
          details_log <- c(details_log, "Dropped row instances with NULLs")
        }
      }
    } else {
      req(input$impute_text_val_col)
      na_indices <- which(is.na(col_vec) | as.character(col_vec) == "")
      if (length(na_indices) > 0) {
        df[na_indices, col] <- input$impute_text_val_col
        actions <- c(actions, "Impute Text")
        details_log <- c(details_log, paste0("Replaced missing blanks with '", input$impute_text_val_col, "'"))
      }
    }

    if (length(actions) > 0) {
      new_rows <- data.frame(
        Time = rep(format(Sys.time(), "%H:%M:%S"), length(actions)),
        Column = rep(col, length(actions)),
        Action = actions,
        Details = details_log,
        stringsAsFactors = FALSE
      )
      actions_log(rbind(actions_log(), new_rows))
      modified_data(df)
      showNotification(paste("Success: Custom updates parsed onto column:", col), type = "message")
    } else {
      showNotification("No transformation fields changed.", type = "warning")
    }
  })

  # --- BOTTOM PANEL PROFILE STATISTICS METRIC ---
  output$column_stats_table_ui <- renderUI({
    df <- current_working_data()
    req(df)
    col <- active_profile_col()
    if(is.null(col) || !(col %in% colnames(df))) return(p("Click on a header column.", style="color:#64748b;"))

    col_vec <- df[[col]]
    total_len <- length(col_vec)
    na_count <- sum(is.na(col_vec) | as.character(col_vec) == "")
    distinct_cnt <- length(unique(col_vec))
    unique_cnt <- sum(table(col_vec) == 1)

    base_rows <- list(
      tags$tr(tags$td("Count"), tags$td(strong(total_len))),
      tags$tr(tags$td("Error"), tags$td(strong(0))),
      tags$tr(tags$td("Empty"), tags$td(strong(na_count))),
      tags$tr(tags$td("Distinct"), tags$td(strong(distinct_cnt))),
      tags$tr(tags$td("Unique"), tags$td(strong(unique_cnt)))
    )

    if(is.numeric(col_vec)) {
      clean_v <- na.omit(col_vec)
      numeric_rows <- list(
        tags$tr(tags$td("Min"), tags$td(strong(if(length(clean_v)>0) min(clean_v) else "N/A"))),
        tags$tr(tags$td("Max"), tags$td(strong(if(length(clean_v)>0) max(clean_v) else "N/A"))),
        tags$tr(tags$td("Average"), tags$td(strong(if(length(clean_v)>0) round(mean(clean_v), 4) else "N/A")))
      )
      base_rows <- c(base_rows, numeric_rows)
    }

    tags$table(class = "table table-sm table-striped",
               style = "font-size:0.8rem; margin:0;",
               tags$tbody(base_rows))
  })

  # --- PROFILE CHART LAYER COMPONENT ---
  output$profile_histogram_plot <- renderPlot({
    df <- current_working_data()
    req(df)
    col <- active_profile_col()
    req(col)

    col_vec <- na.omit(df[[col]])
    if(length(col_vec) == 0) return(NULL)

    plot_df <- data.frame(Value = col_vec)

    if(is.numeric(col_vec)) {
      ggplot(plot_df, aes(x = Value)) +
        geom_histogram(fill = "#0ea5e9", color = "#ffffff", bins = 30, alpha = 0.9) +
        theme_minimal(base_size = 12) +
        labs(x = NULL, y = NULL) +
        theme(
          panel.grid.minor = element_blank(),
          plot.margin = margin(10, 15, 10, 15)
        )
    } else {
      top_df <- as.data.frame(table(Value = col_vec))
      top_df <- top_df[order(-top_df$Freq), ]
      top_df = head(top_df, 15)

      ggplot(top_df, aes(x = reorder(Value, -Freq), y = Freq)) +
        geom_col(fill = "#0ea5e9", alpha = 0.9, width = 0.75) +
        theme_minimal(base_size = 12) +
        labs(x = NULL, y = NULL) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank(),
          plot.margin = margin(10, 15, 10, 15)
        )
    }
  })

  # --- DATA PREVIEW FILTER LOGIC ---
  output$preview_data_table <- renderTable({
    df <- current_working_data()
    req(df)

    if (input$preview_view_type == "nulls") {
      has_null <- apply(df, 1, function(row) {
        any(is.na(row) | as.character(row) == "")
      })
      filtered_df <- df[has_null, , drop = FALSE]
      if (nrow(filtered_df) == 0) {
        return(data.frame(Message = "No null rows detected inside the current data frame!"))
      }
      return(head(filtered_df, 500))
    } else {
      return(head(df, 500))
    }
  })

  # --- PENDING ACTIONS ACTION LOG TABLE ---
  output$pending_actions_table <- renderTable({
    log_df <- actions_log()
    if (nrow(log_df) == 0) {
      return(data.frame(Status = "No custom data adjustments tracked in this log session yet."))
    }
    log_df
  })

  # --- EXPORT INTERACTIVE SYSTEM MODAL ---
  observeEvent(input$btn_export, {
    showModal(modalDialog(
      title = "Export Cleaned Dataset Options",
      p("Please choose your desired configuration profile file type format below:"),
      div(style = "display: flex; gap: 12px; justify-content: center; padding: 20px;",
          downloadButton("download_csv", "CSV File Output", class = "btn-success"),
          downloadButton("download_txt", "Text (.txt) Output", class = "btn-info text-white"),
          downloadButton("download_xls", "Excel Workbook Output", class = "btn-primary")
      ),
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Dismiss")
    ))
  })

  output$download_csv <- downloadHandler(
    filename = function() { paste0("cleaned_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv") },
    content = function(file) { write.csv(current_working_data(), file, row.names = FALSE) }
  )

  output$download_txt <- downloadHandler(
    filename = function() { paste0("cleaned_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt") },
    content = function(file) { write.table(current_working_data(), file, sep = "\t", row.names = FALSE, quote = FALSE) }
  )

  output$download_xls <- downloadHandler(
    filename = function() { paste0("cleaned_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xls") },
    content = function(file) { write.table(current_working_data(), file, sep = "\t", row.names = FALSE) }
  )
}

options(shiny.maxRequestSize = 200 * 1024^2)
shinyApp(ui, server)
