library(shiny)
library(bslib)
library(shinyjs)
library(ggplot2)

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
      .source-item-btn { border: 1px solid #e2e8f0; border-radius: 6px; padding: 15px; text-align: center; background: #fff; width: 100%; display: block; transition: all 0.2s; cursor: pointer; }
      .source-item-btn:hover { border-color: #10b981; background: #f0fdf4; transform: translateY(-2px); }
      .source-item-btn.selected-item { border: 2px solid #10b981 !important; background: #e6fbf1 !important; }
      .source-item-btn i { font-size: 2rem; margin-bottom: 8px; display: block; }

      /* Data Source Branded Colors Restoration */
      .icon-excel { color: #107c41 !important; }
      .icon-json { color: #2563eb !important; }
      .icon-access { color: #a4373a !important; }
      .icon-sql { color: #e38b00 !important; }
      .icon-snowflake { color: #29b6f6 !important; }
      .icon-salesforce { color: #00a1e0 !important; }
      .icon-orange { color: #ff6f00 !important; }
      .icon-azure { color: #0078d4 !important; }
      .icon-code { color: #38bdf8 !important; }

      /* Power Query Data Grid Styling */
      .pbi-table-container { overflow-x: auto; max-height: 400px; overflow-y: auto; border: 1px solid #dee2e6; background: #fff; }
      .pbi-table { width: 100%; border-collapse: collapse; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; font-size: 0.82rem; }
      .pbi-table th { background: #f8f9fa; border: 1px solid #dee2e6; padding: 6px; font-weight: normal; vertical-align: top; min-width: 160px; position: sticky; top: 0; z-index: 5; cursor: pointer; user-select: none; }
      .pbi-table th:hover { background: #f1f5f9; }
      .pbi-table td { border: 1px solid #e2e8f0; padding: 4px 8px; white-space: nowrap; max-width: 220px; overflow: hidden; text-overflow: ellipsis; color: #334155; }

      /* Column Selection Highlighting */
      .pbi-table th.selected-col-header { background: #e2e8f0 !important; border-bottom: 3px solid #0284c7 !important; font-weight: 600; }
      .pbi-table td.selected-col-cell { background-color: #f1f5f9 !important; }

      /* Header Elements styling */
      .header-top-row { display: flex; justify-content: space-between; align-items: center; margin-bottom: 4px; pointer-events: none; }
      .header-title-text { font-weight: 600; color: #1e293b; overflow: hidden; text-overflow: ellipsis; }
      .header-type-label { font-weight: 700; color: #64748b; font-size: 0.7rem; background: #e2e8f0; padding: 1px 4px; border-radius: 3px; }

      /* Quality Bar */
      .quality-bar-wrapper { margin-top: 4px; margin-bottom: 4px; pointer-events: none; }
      .quality-bar { display: flex; height: 6px; border-radius: 2px; overflow: hidden; background: #e2e8f0; }
      .q-valid { background-color: #10b981; }
      .q-empty { background-color: #94a3b8; }
      .q-error { background-color: #ef4444; }
      .quality-text-legend { font-size: 0.72rem; color: #475569; display: flex; justify-content: space-between; margin-top: 1px; }

      /* Mini Inline Distribution Plot */
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
                    span("0 actions pending — go to Columns tab to add rules", style = "color: #666; font-size: 0.9rem;"),
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
                          checkboxInput("pbi_dist", "Column distribution", value = FALSE),
                          checkboxInput("pbi_profile", "Column profile", value = FALSE)
                      )
                    ),
                    div(class = "pbi-table-container",
                        uiOutput("power_bi_table")
                    )
                  ),

                  # Bottom Expandable Profile Segment matching Power BI View Layout
                  conditionalPanel(
                    condition = "input.pbi_profile == true",
                    br(),
                    layout_columns(
                      col_widths = c(4, 8),
                      card(
                        card_header(strong("Column statistics")),
                        uiOutput("column_stats_table_ui")
                      ),
                      card(
                        card_header(strong("Value distribution")),
                        plotOutput("profile_histogram_plot", height = "260px")
                      )
                    )
                  )
        ),

        nav_panel("Preview"),
        nav_panel("Pending actions")
      )
    )
  )
)

server <- function(input, output, session) {

  file_reset_trigger <- reactiveVal(FALSE)
  selected_source <- reactiveVal(NULL)
  active_profile_col <- reactiveVal(NULL)

  observeEvent(input$btn_clear_file, {
    shinyjs::runjs("
      $('#file_input').val('');
      $('#file_input_placeholder').val('');
      $('.progress-bar').css('width', '0%').text('');
      $('.progress').hide();
      $('.shiny-input-container .input-group .form-control').val('');
    ")
    file_reset_trigger(TRUE)
    active_profile_col(NULL)
    showNotification("Dataset cleared successfully.", type = "warning")
  })

  observeEvent(input$file_input, {
    file_reset_trigger(FALSE)
  })

  uploaded_data <- reactive({
    if (file_reset_trigger()) return(NULL)
    req(input$file_input)
    df <- read.csv(input$file_input$datapath, stringsAsFactors = FALSE)
    if(ncol(df) > 0 && is.null(active_profile_col())) {
      active_profile_col(colnames(df)[1])
    }
    df
  })

  # Listen to programmatic click events routed from table headers via JS integration
  observeEvent(input$selected_pbi_column, {
    active_profile_col(input$selected_pbi_column)
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

  output$filtered_sources_ui <- renderUI({
    search_term <- if (!is.null(input$search_source)) tolower(input$search_source) else ""
    all_sources <- list(
      list(id = "excel",   name = "Excel workbook",        icon = "fa-file-excel icon-excel"),
      list(id = "json",    name = "JSON file",             icon = "fa-code icon-json"),
      list(id = "access",  name = "Access database",       icon = "fa-database icon-access"),
      list(id = "sqldb",   name = "SQL Server database",   icon = "fa-database icon-sql"),
      list(id = "snow",    name = "Snowflake",             icon = "fa-snowflake icon-snowflake"),
      list(id = "sfobj",   name = "Salesforce Objects",    icon = "fa-cloud icon-salesforce"),
      list(id = "sfrep",   name = "Salesforce Reports",    icon = "fa-chart-bar icon-salesforce"),
      list(id = "gbq",     name = "Google BigQuery",       icon = "fa-cloud-meatball icon-orange"),
      list(id = "databr",  name = "Azure Databricks",      icon = "fa-fire icon-azure"),
      list(id = "py",      name = "Python script",         icon = "fa-brands fa-python icon-code"),
      list(id = "rscript", name = "R script",              icon = "fa-brands fa-r-project icon-code")
    )
    matched <- Filter(function(s) search_term == "" || grepl(search_term, tolower(s$name)), all_sources)
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
                                 tags$span(s$name, style = "color: #334155; font-weight: 500; font-size: 0.9rem;")
                     )
          )
        })
    )
  })

  output$dataset_name_ui <- renderText({ if (is.null(uploaded_data())) "No file loaded" else input$file_input$name })
  output$dataset_dims_ui <- renderText({ if (is.null(uploaded_data())) "0 rows • 0 columns" else paste(nrow(uploaded_data()), "rows •", ncol(uploaded_data()), "columns") })
  output$total_rows     <- renderText({ if (is.null(uploaded_data())) "0" else nrow(uploaded_data()) })
  output$total_cols     <- renderText({ if (is.null(uploaded_data())) "0" else ncol(uploaded_data()) })
  output$missing_count  <- renderText({ if (is.null(uploaded_data())) "0" else sum(is.na(uploaded_data())) })
  output$columns_issues <- renderText({ if (is.null(uploaded_data())) "0" else sum(sapply(uploaded_data(), function(col) any(is.na(col)))) })

  # --- OVERVIEW DATA DISPLAY CARDS ---
  output$column_cards_container <- renderUI({
    df <- uploaded_data()
    if (is.null(df)) return(p("Please upload a dataset file to generate column evaluations.", style = "color: gray; font-style: italic;"))

    col_names <- colnames(df)
    card_list <- lapply(col_names, function(name) {
      column_data = df[[name]]
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

  # --- TRANSFORM DATA (POWER BI ENGINE MODULE) ---
  output$power_bi_table <- renderUI({
    df <- uploaded_data()
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

      tags$th(
        class = is_selected_class,
        onclick = sprintf("Shiny.setInputValue('selected_pbi_column', '%s', {priority: 'event'});", col),

        div(class = "header-top-row",
            span(ui_type, class = "header-type-label"),
            div(class = "header-title-text", col)
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
                  lapply(head(as.numeric(table(head(col_vec, 100))), 8), function(val) {
                    pct_h <- min(100, max(15, round((val / max(table(col_vec))) * 100)))
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

    rows <- lapply(1:nrow(preview_df), function(i) {
      tags$tr(
        lapply(col_names, function(col) {
          val <- preview_df[i, col]
          cell_class <- if(!is.null(selected_col) && selected_col == col) "selected-col-cell" else ""
          tags$td(class = cell_class, if(is.na(val) || val == "") tags$em("null", style="color:#cbd5e1;") else as.character(val))
        })
      )
    })

    tags$table(class = "pbi-table", tags$thead(tags$tr(headers)), tags$tbody(rows))
  })

  # --- SUMMARY STATISTICS DETAILS COMPONENT ---
  output$column_stats_table_ui <- renderUI({
    df <- uploaded_data()
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

    tags$table(class = "table table-sm table-striped style='font-size:0.8rem; margin:0;'",
               tags$tbody(base_rows))
  })

  # --- DYNAMIC HISTOGRAM PLOT COMPONENT ---
  output$profile_histogram_plot <- renderPlot({
    df <- uploaded_data()
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
      top_df <- head(top_df, 15)

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
}

shinyApp(ui, server)
