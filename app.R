# ============================================================
# Zenith Bank — Failed E-Channel Transactions Dashboard
# Author: Ebunoluwa Idowu
# Role: Relationship Manager, Lagos Central Branch
# Course: Data Analytics II — Lagos Business School
# Save as: app.R in your DA EXAM folder
# Run with: shiny::runApp() in RStudio console
# ============================================================

library(shiny)
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(scales)
library(DT)
library(broom)
library(effsize)

# ── Load data ────────────────────────────────────────────────
df <- read_csv("zenith_echannel_complaints.csv") |>
  mutate(
    date              = as.Date(date),
    month             = factor(month, levels = c("October","November","December",
                                                 "January","February","March","April")),
    channel           = factor(channel),
    transaction_type  = factor(transaction_type),
    failure_reason    = factor(failure_reason),
    customer_segment  = factor(customer_segment),
    time_of_day       = factor(time_of_day,
                               levels = c("Morning","Afternoon","Evening")),
    escalated         = factor(escalated),
    resolution_status = factor(resolution_status),
    customer_age_group = factor(customer_age_group),
    account_type      = factor(account_type),
    repeat_complaint  = factor(repeat_complaint),
    log_amount        = log(amount_naira)
  )

z_red   <- "#C8102E"
z_blue  <- "#1A73E8"
z_gold  <- "#D4AF5A"
z_grey  <- "#94A3B8"
z_green <- "#2ECC71"

dark_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.background   = element_rect(fill = "#1A1A1A", colour = NA),
    panel.background  = element_rect(fill = "#1A1A1A", colour = NA),
    panel.grid.major  = element_line(colour = "#2A2A2A"),
    panel.grid.minor  = element_blank(),
    text              = element_text(colour = "#cccccc"),
    axis.text         = element_text(colour = "#888888", size = 10),
    axis.title        = element_text(colour = "#aaaaaa", size = 11),
    plot.title        = element_text(colour = "#F5F5F0", size = 13,
                                     face = "bold"),
    plot.subtitle     = element_text(colour = "#888888", size = 10),
    legend.background = element_rect(fill = "#1A1A1A", colour = NA),
    legend.text       = element_text(colour = "#cccccc"),
    strip.text        = element_text(colour = "#cccccc")
  )

ui <- fluidPage(
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@700;900&family=IBM+Plex+Sans:wght@300;400;500&family=IBM+Plex+Mono:wght@400;600&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
      * { box-sizing: border-box; margin: 0; padding: 0; }
      body { background: #0D0D0D; color: #F5F5F0;
             font-family: 'IBM Plex Sans', sans-serif; font-weight: 300; }
      .dash-header { background: #C8102E; padding: 28px 40px 24px;
                     position: relative; overflow: hidden; }
      .dash-header::after { content: 'Z'; position: absolute;
        right: -10px; top: -30px; font-family: 'Playfair Display', serif;
        font-size: 200px; font-weight: 900; color: rgba(255,255,255,0.07);
        line-height: 1; pointer-events: none; }
      .dash-title { font-family: 'Playfair Display', serif; font-size: 22px;
                    font-weight: 900; color: white; margin-bottom: 6px; }
      .dash-sub { color: rgba(255,255,255,0.7); font-size: 13px; }
      .dash-meta { display: flex; gap: 28px; margin-top: 16px; flex-wrap: wrap; }
      .meta-box span { display: block; font-family: 'IBM Plex Mono', monospace;
        font-size: 9px; letter-spacing: 2px; text-transform: uppercase;
        color: rgba(255,255,255,0.5); margin-bottom: 2px; }
      .meta-box strong { color: white; font-size: 13px; font-weight: 500; }
      .nav-tabs { background: #1A1A1A; border-bottom: 1px solid #333; padding: 0 32px; }
      .nav-tabs .nav-item .nav-link { color: #888 !important;
        background: transparent !important; border: none !important;
        border-bottom: 3px solid transparent !important; border-radius: 0 !important;
        padding: 12px 18px !important; font-family: 'IBM Plex Mono', monospace !important;
        font-size: 10px !important; letter-spacing: 1.5px !important;
        text-transform: uppercase !important; }
      .nav-tabs .nav-item .nav-link:hover { color: #F5F5F0 !important; }
      .nav-tabs .nav-item .nav-link.active { color: #C8102E !important;
        border-bottom-color: #C8102E !important; font-weight: 600 !important; }
      .tab-content { padding: 28px 40px; background: #0D0D0D; min-height: 80vh; }
      .kpi-grid { display: grid; grid-template-columns: repeat(4,1fr);
                  gap: 14px; margin-bottom: 28px; }
      .kpi-card { background: #1A1A1A; border: 1px solid #333;
                  border-radius: 4px; padding: 18px; }
      .kpi-label { font-family: 'IBM Plex Mono', monospace; font-size: 9px;
                   letter-spacing: 2px; text-transform: uppercase;
                   color: #888; margin-bottom: 8px; }
      .kpi-value { font-family: 'Playfair Display', serif; font-size: 30px;
                   font-weight: 900; color: #C8102E; line-height: 1; }
      .kpi-sub { font-size: 11px; color: #666; margin-top: 6px; }
      .kpi-card.gold .kpi-value { color: #D4AF5A; }
      .kpi-card.blue .kpi-value { color: #1A73E8; }
      .kpi-card.green .kpi-value { color: #2ECC71; }
      .plot-card { background: #1A1A1A; border: 1px solid #333;
                   border-radius: 4px; padding: 20px; margin-bottom: 18px; }
      .plot-title { font-family: 'Playfair Display', serif; font-size: 15px;
                    font-weight: 700; color: #F5F5F0; margin-bottom: 4px; }
      .plot-sub { font-family: 'IBM Plex Mono', monospace; font-size: 10px;
                  color: #666; letter-spacing: 1px; margin-bottom: 14px; }
      .ctrl-panel { background: #1A1A1A; border: 1px solid #333;
                    border-radius: 4px; padding: 16px 20px; margin-bottom: 18px; }
      .ctrl-label { font-family: 'IBM Plex Mono', monospace; font-size: 10px;
                    letter-spacing: 1.5px; text-transform: uppercase;
                    color: #888; margin-bottom: 8px; display: block; }
      .selectize-control .selectize-input { background: #222 !important;
        border: 1px solid #444 !important; color: #F5F5F0 !important;
        border-radius: 3px !important; }
      .selectize-dropdown { background: #222 !important;
        border: 1px solid #444 !important; color: #F5F5F0 !important; }
      .selectize-dropdown .option:hover,
      .selectize-dropdown .option.active { background: #C8102E !important; }
      .dataTables_wrapper { color: #F5F5F0 !important; font-size: 13px !important; }
      table.dataTable thead th { background: #222 !important;
        color: #D4AF5A !important; border-bottom: 1px solid #444 !important;
        font-family: 'IBM Plex Mono', monospace !important; font-size: 10px !important;
        letter-spacing: 1px !important; text-transform: uppercase !important; }
      table.dataTable tbody tr { background: #1A1A1A !important; }
      table.dataTable tbody tr:hover { background: #252525 !important; }
      table.dataTable tbody td { border-color: #333 !important; color: #ccc !important; }
      .dataTables_filter input, .dataTables_length select {
        background: #222 !important; border: 1px solid #444 !important;
        color: #F5F5F0 !important; border-radius: 3px !important; }
      .dataTables_info, .dataTables_paginate { color: #888 !important; }
      .paginate_button.current { background: #C8102E !important;
        color: white !important; border-radius: 3px !important; }
      .out-box { background: #111; border: 1px solid #333; border-radius: 4px;
                 padding: 16px 20px; font-family: 'IBM Plex Mono', monospace;
                 font-size: 12px; color: #ccc; line-height: 1.8; overflow-x: auto; }
      .insight { background: rgba(200,16,46,0.08);
                 border-left: 3px solid #C8102E; border-radius: 0 4px 4px 0;
                 padding: 14px 18px; margin-top: 14px; font-size: 13px;
                 color: #ccc; line-height: 1.7; }
      .insight strong { color: #F5F5F0; }
      .two-col { display: grid; grid-template-columns: 1fr 1fr; gap: 18px; }
      @media (max-width: 900px) {
        .kpi-grid { grid-template-columns: repeat(2,1fr); }
        .two-col { grid-template-columns: 1fr; }
      }
    "))
  ),
  
  div(class = "dash-header",
      div(class = "dash-title",
          "Failed E-Channel Transactions — Zenith Bank"),
      div(class = "dash-sub",
          "Interactive Analytics · Lagos Central Branch · Data Analytics II · LBS · Oct 2025 – Apr 2026"),
      div(class = "dash-meta",
          div(class = "meta-box",
              tags$span("Author"), tags$strong("Ebunoluwa Idowu")),
          div(class = "meta-box",
              tags$span("Role"), tags$strong("Relationship Manager")),
          div(class = "meta-box",
              tags$span("Branch"), tags$strong("Lagos Central")),
          div(class = "meta-box",
              tags$span("Period"), tags$strong("Oct 2025 – Apr 2026")),
          div(class = "meta-box",
              tags$span("Records"), tags$strong("120 Complaints"))
      )
  ),
  
  navbarPage(
    title = NULL,
    windowTitle = "Zenith E-Channel Dashboard",
    collapsible = TRUE,
    id = "main_nav",
    
    # TAB 1 — OVERVIEW
    tabPanel("Overview",
             div(class = "tab-content",
                 div(class = "kpi-grid",
                     div(class = "kpi-card",
                         div(class = "kpi-label", "Total Complaints"),
                         div(class = "kpi-value", "120"),
                         div(class = "kpi-sub", "Oct 2025 – Apr 2026")
                     ),
                     div(class = "kpi-card gold",
                         div(class = "kpi-label", "Top Failure Reason"),
                         div(class = "kpi-value", "NE"),
                         div(class = "kpi-sub", "Network Error — most frequent")
                     ),
                     div(class = "kpi-card blue",
                         div(class = "kpi-label", "Unresolved Cases"),
                         div(class = "kpi-value", "6"),
                         div(class = "kpi-sub", "5% of total complaints")
                     ),
                     div(class = "kpi-card green",
                         div(class = "kpi-label", "Avg Resolution"),
                         div(class = "kpi-value", "2.1"),
                         div(class = "kpi-sub", "Days — all complaints")
                     )
                 ),
                 div(class = "two-col",
                     div(class = "plot-card",
                         div(class = "plot-title", "Complaints by Failure Reason & Channel"),
                         div(class = "plot-sub", "MOBILE VS USSD"),
                         plotOutput("ov1", height = "280px")
                     ),
                     div(class = "plot-card",
                         div(class = "plot-title", "Monthly Complaint Trend"),
                         div(class = "plot-sub", "NOV 2025 – MAY 2026"),
                         plotOutput("ov2", height = "280px")
                     )
                 ),
                 div(class = "two-col",
                     div(class = "plot-card",
                         div(class = "plot-title", "Complaints by Time of Day"),
                         div(class = "plot-sub", "MORNING · AFTERNOON · EVENING"),
                         plotOutput("ov3", height = "280px")
                     ),
                     div(class = "plot-card",
                         div(class = "plot-title", "Resolution Status by Channel"),
                         div(class = "plot-sub", "RESOLVED VS UNRESOLVED"),
                         plotOutput("ov4", height = "280px")
                     )
                 )
             )
    ),
    
    # TAB 2 — EDA
    tabPanel("EDA",
             div(class = "tab-content",
                 div(class = "ctrl-panel",
                     tags$span(class = "ctrl-label", "Explore Variables"),
                     fluidRow(
                       column(4,
                              selectInput("eda_var", "Numeric Variable",
                                          choices = c(
                                            "Resolution Days" = "resolution_days",
                                            "Log Amount"      = "log_amount"
                                          ))
                       ),
                       column(4,
                              selectInput("eda_grp", "Group by",
                                          choices = c(
                                            "Channel"          = "channel",
                                            "Failure Reason"   = "failure_reason",
                                            "Customer Segment" = "customer_segment",
                                            "Time of Day"      = "time_of_day",
                                            "Escalated"        = "escalated"
                                          ))
                       )
                     )
                 ),
                 div(class = "two-col",
                     div(class = "plot-card",
                         div(class = "plot-title", "Distribution"),
                         plotOutput("eda_hist", height = "300px")
                     ),
                     div(class = "plot-card",
                         div(class = "plot-title", "Box Plot by Group"),
                         plotOutput("eda_box", height = "300px")
                     )
                 ),
                 div(class = "plot-card",
                     div(class = "plot-title", "Full Data Table"),
                     DTOutput("eda_tbl")
                 )
             )
    ),
    
    # TAB 3 — HYPOTHESIS TESTING
    tabPanel("Hypothesis Testing",
             div(class = "tab-content",
                 div(class = "ctrl-panel",
                     tags$span(class = "ctrl-label", "Configure Test"),
                     fluidRow(
                       column(4,
                              selectInput("hyp_var", "Outcome Variable",
                                          choices = c(
                                            "Resolution Days" = "resolution_days",
                                            "Log Amount"      = "log_amount"
                                          ))
                       ),
                       column(4,
                              selectInput("hyp_grp", "Compare Groups",
                                          choices = c(
                                            "Channel"          = "channel",
                                            "Escalated"        = "escalated",
                                            "Customer Segment" = "customer_segment"
                                          ))
                       ),
                       column(4,
                              selectInput("hyp_dir", "Alternative",
                                          choices = c(
                                            "Greater" = "greater",
                                            "Less"    = "less",
                                            "Two-sided" = "two.sided"
                                          ))
                       )
                     )
                 ),
                 div(class = "two-col",
                     div(class = "plot-card",
                         div(class = "plot-title", "Group Comparison"),
                         plotOutput("hyp_plot", height = "320px")
                     ),
                     div(class = "plot-card",
                         div(class = "plot-title", "Test Results"),
                         div(class = "out-box",
                             verbatimTextOutput("hyp_out")),
                         div(class = "insight",
                             uiOutput("hyp_interp"))
                     )
                 )
             )
    ),
    
    # TAB 4 — CORRELATION
    tabPanel("Correlation",
             div(class = "tab-content",
                 div(class = "two-col",
                     div(class = "plot-card",
                         div(class = "plot-title", "Correlation Matrix"),
                         plotOutput("corr_map", height = "420px")
                     ),
                     div(class = "plot-card",
                         div(class = "plot-title", "Scatter Plot"),
                         selectInput("sc_x", "X Variable",
                                     choices = c(
                                       "Log Amount" = "log_amount",
                                       "Amount ₦"   = "amount_naira"
                                     )),
                         selectInput("sc_col", "Colour by",
                                     choices = c(
                                       "Channel"          = "channel",
                                       "Escalated"        = "escalated",
                                       "Customer Segment" = "customer_segment",
                                       "Failure Reason"   = "failure_reason"
                                     )),
                         plotOutput("corr_sc", height = "300px")
                     )
                 )
             )
    ),
    
    # TAB 5 — REGRESSION
    tabPanel("Regression",
             div(class = "tab-content",
                 div(class = "ctrl-panel",
                     tags$span(class = "ctrl-label", "Select Predictors"),
                     checkboxGroupInput("reg_vars", NULL,
                                        choices = c(
                                          "Channel (Mobile=1)"    = "mobile",
                                          "Escalated (Yes=1)"     = "escalated_bin",
                                          "Log Amount"            = "log_amount",
                                          "SME Segment (SME=1)"   = "sme",
                                          "Repeat Complaint"      = "repeat_bin"
                                        ),
                                        selected = c("mobile","escalated_bin","log_amount","sme"),
                                        inline = TRUE
                     )
                 ),
                 div(class = "two-col",
                     div(class = "plot-card",
                         div(class = "plot-title", "Coefficient Plot"),
                         plotOutput("reg_coef", height = "300px")
                     ),
                     div(class = "plot-card",
                         div(class = "plot-title", "Model Summary"),
                         div(class = "out-box",
                             verbatimTextOutput("reg_out"))
                     )
                 ),
                 div(class = "two-col",
                     div(class = "plot-card",
                         div(class = "plot-title", "Residuals vs Fitted"),
                         plotOutput("reg_d1", height = "260px")
                     ),
                     div(class = "plot-card",
                         div(class = "plot-title", "Q-Q Plot"),
                         plotOutput("reg_d2", height = "260px")
                     )
                 )
             )
    ),
    
    # TAB 6 — COMPLAINT EXPLORER
    tabPanel("Complaint Explorer",
             div(class = "tab-content",
                 div(class = "ctrl-panel",
                     tags$span(class = "ctrl-label", "Filter Records"),
                     fluidRow(
                       column(3,
                              selectInput("f_ch", "Channel",
                                          c("All","Mobile","USSD"))),
                       column(3,
                              selectInput("f_rs", "Failure Reason",
                                          c("All", levels(df$failure_reason)))),
                       column(3,
                              selectInput("f_st", "Resolution Status",
                                          c("All","Resolved","Unresolved"))),
                       column(3,
                              selectInput("f_sg", "Segment",
                                          c("All","Retail","SME")))
                     )
                 ),
                 div(class = "plot-card",
                     div(class = "plot-title", "Filtered Complaint Records"),
                     div(class = "plot-sub",
                         "ZENITH BANK LAGOS CENTRAL BRANCH · EBUNOLUWA IDOWU"),
                     DTOutput("exp_tbl")
                 )
             )
    )
  )
)

server <- function(input, output, session) {
  
  # OVERVIEW
  output$ov1 <- renderPlot({
    ggplot(df, aes(x = fct_infreq(failure_reason), fill = channel)) +
      geom_bar(position = "dodge") +
      scale_fill_manual(values = c("Mobile" = z_red, "USSD" = z_blue)) +
      labs(x = NULL, y = "Complaints", fill = NULL) +
      dark_theme +
      theme(axis.text.x = element_text(angle = 25, hjust = 1),
            legend.position = "bottom")
  }, bg = "#1A1A1A")
  
  output$ov2 <- renderPlot({
    df |> count(month, channel) |>
      ggplot(aes(x = month, y = n, colour = channel, group = channel)) +
      geom_line(linewidth = 1.4) + geom_point(size = 3) +
      scale_colour_manual(values = c("Mobile" = z_red, "USSD" = z_blue)) +
      labs(x = NULL, y = "Complaints", colour = NULL) +
      dark_theme +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 20, hjust = 1))
  }, bg = "#1A1A1A")
  
  output$ov3 <- renderPlot({
    df |> count(time_of_day, channel) |>
      ggplot(aes(x = time_of_day, y = n, fill = channel)) +
      geom_col(position = "dodge", width = 0.6) +
      scale_fill_manual(values = c("Mobile" = z_red, "USSD" = z_blue)) +
      labs(x = NULL, y = "Complaints", fill = NULL) +
      dark_theme + theme(legend.position = "bottom")
  }, bg = "#1A1A1A")
  
  output$ov4 <- renderPlot({
    df |> count(channel, resolution_status) |>
      group_by(channel) |>
      mutate(pct = n / sum(n) * 100) |>
      ggplot(aes(x = channel, y = pct, fill = resolution_status)) +
      geom_col(width = 0.6) +
      scale_fill_manual(values = c("Resolved" = z_green,
                                   "Unresolved" = z_red)) +
      labs(x = NULL, y = "% of Complaints", fill = NULL) +
      dark_theme + theme(legend.position = "bottom")
  }, bg = "#1A1A1A")
  
  # EDA
  output$eda_hist <- renderPlot({
    ggplot(df, aes(x = .data[[input$eda_var]],
                   fill = .data[[input$eda_grp]])) +
      geom_histogram(bins = 10, colour = "#1A1A1A",
                     alpha = 0.85, position = "identity") +
      geom_vline(xintercept = mean(df[[input$eda_var]], na.rm = TRUE),
                 lty = 2, colour = z_gold, linewidth = 1) +
      labs(x = input$eda_var, y = "Count",
           fill = input$eda_grp) +
      dark_theme + theme(legend.position = "bottom")
  }, bg = "#1A1A1A")
  
  output$eda_box <- renderPlot({
    ggplot(df, aes(x = .data[[input$eda_grp]],
                   y = .data[[input$eda_var]],
                   fill = .data[[input$eda_grp]])) +
      geom_boxplot(alpha = 0.7, width = 0.5,
                   outlier.colour = z_gold) +
      geom_jitter(width = 0.1, size = 1.8,
                  alpha = 0.5, colour = "white") +
      labs(x = NULL, y = input$eda_var) +
      dark_theme +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 20, hjust = 1))
  }, bg = "#1A1A1A")
  
  output$eda_tbl <- renderDT({
    df |>
      select(complaint_id, date, channel, transaction_type,
             amount_naira, failure_reason, customer_segment,
             time_of_day, escalated, resolution_status,
             resolution_days) |>
      datatable(options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE) |>
      formatCurrency("amount_naira", currency = "₦", digits = 0)
  })
  
  # HYPOTHESIS
  output$hyp_plot <- renderPlot({
    ggplot(df, aes(x = .data[[input$hyp_grp]],
                   y = .data[[input$hyp_var]],
                   fill = .data[[input$hyp_grp]])) +
      geom_boxplot(alpha = 0.7, width = 0.5) +
      geom_jitter(width = 0.1, size = 2.5,
                  colour = "white", alpha = 0.7) +
      scale_fill_manual(values = c(z_red, z_blue, z_green)) +
      labs(x = NULL, y = input$hyp_var) +
      dark_theme + theme(legend.position = "none")
  }, bg = "#1A1A1A")
  
  output$hyp_out <- renderPrint({
    var  <- input$hyp_var
    grp  <- input$hyp_grp
    lvls <- levels(df[[grp]])
    g1   <- df |> filter(.data[[grp]] == lvls[1]) |> pull(var)
    g2   <- df |> filter(.data[[grp]] == lvls[2]) |> pull(var)
    t_r  <- t.test(g1, g2, alternative = input$hyp_dir)
    d_r  <- cohen.d(g1, g2)
    w_r  <- wilcox.test(g1, g2, alternative = input$hyp_dir)
    cat("=== T-TEST ===\n")
    cat(sprintf("%-20s mean = %.3f\n", lvls[1], mean(g1)))
    cat(sprintf("%-20s mean = %.3f\n", lvls[2], mean(g2)))
    cat(sprintf("t = %.3f,  p = %.4f\n", t_r$statistic, t_r$p.value))
    cat(ifelse(t_r$p.value < 0.05,
               "REJECT H0 — significant at 5%\n",
               "FAIL TO REJECT H0\n"))
    cat(sprintf("Cohen's d = %.3f (%s)\n\n", d_r$estimate, d_r$magnitude))
    cat("=== WILCOXON ===\n")
    cat(sprintf("p = %.4f  %s\n", w_r$p.value,
                ifelse(w_r$p.value < 0.05,
                       "REJECT H0","FAIL TO REJECT H0")))
  })
  
  output$hyp_interp <- renderUI({
    var  <- input$hyp_var
    grp  <- input$hyp_grp
    lvls <- levels(df[[grp]])
    g1   <- df |> filter(.data[[grp]] == lvls[1]) |> pull(var)
    g2   <- df |> filter(.data[[grp]] == lvls[2]) |> pull(var)
    t_r  <- t.test(g1, g2, alternative = input$hyp_dir)
    sig  <- t_r$p.value < 0.05
    HTML(paste0(
      "<strong>Interpretation:</strong> The difference in <em>", var,
      "</em> between <em>", lvls[1], "</em> (mean=",
      round(mean(g1), 2), ") and <em>", lvls[2], "</em> (mean=",
      round(mean(g2), 2), ") is ",
      ifelse(sig,
             "<strong>statistically significant</strong> (p < 0.05).",
             "<strong>not statistically significant</strong> at 5%."),
      " <strong>Business implication:</strong> ",
      ifelse(sig,
             "This difference is real and should inform SLA policy.",
             "More data needed before changing operational procedures.")
    ))
  })
  
  # CORRELATION
  output$corr_map <- renderPlot({
    df_c <- df |>
      mutate(
        mobile_n    = if_else(channel == "Mobile", 1, 0),
        escalated_n = if_else(escalated == "Yes", 1, 0),
        sme_n       = if_else(customer_segment == "SME", 1, 0),
        repeat_n    = if_else(repeat_complaint == "Yes", 1, 0)
      ) |>
      select(resolution_days, log_amount,
             mobile_n, escalated_n, sme_n, repeat_n)
    cm <- cor(df_c, use = "complete.obs")
    ggcorrplot(cm, method = "square", type = "lower",
               lab = TRUE, lab_size = 3.5,
               colors = c(z_blue, "#1A1A1A", z_red),
               ggtheme = dark_theme)
  }, bg = "#1A1A1A")
  
  output$corr_sc <- renderPlot({
    ggplot(df, aes(x = .data[[input$sc_x]],
                   y = resolution_days,
                   colour = .data[[input$sc_col]])) +
      geom_point(size = 2.5, alpha = 0.75) +
      geom_smooth(method = "lm", se = TRUE,
                  colour = z_gold, fill = "#333", lty = 2) +
      labs(x = input$sc_x, y = "Resolution Days",
           colour = input$sc_col) +
      dark_theme + theme(legend.position = "bottom")
  }, bg = "#1A1A1A")
  
  # REGRESSION
  df_reg <- df |>
    mutate(
      mobile        = if_else(channel == "Mobile", 1, 0),
      escalated_bin = if_else(escalated == "Yes", 1, 0),
      sme           = if_else(customer_segment == "SME", 1, 0),
      repeat_bin    = if_else(repeat_complaint == "Yes", 1, 0)
    )
  
  reg_model <- reactive({
    req(input$reg_vars)
    f <- paste("resolution_days ~",
               paste(input$reg_vars, collapse = " + "))
    lm(as.formula(f), data = df_reg)
  })
  
  output$reg_out <- renderPrint({
    m <- reg_model()
    g <- glance(m)
    cat("OLS — Dependent: Resolution Days\n")
    cat(rep("─", 40), "\n", sep = "")
    td <- tidy(m)
    for (i in 1:nrow(td)) {
      cat(sprintf("%-22s β=%7.4f p=%.4f %s\n",
                  td$term[i], td$estimate[i], td$p.value[i],
                  ifelse(td$p.value[i] < 0.05, "*", "")))
    }
    cat(rep("─", 40), "\n", sep = "")
    cat(sprintf("R²=%.4f Adj.R²=%.4f n=%d\n",
                g$r.squared, g$adj.r.squared, g$nobs))
    cat("* significant at 5%\n")
  })
  
  output$reg_coef <- renderPlot({
    tidy(reg_model(), conf.int = TRUE) |>
      filter(term != "(Intercept)") |>
      ggplot(aes(x = estimate,
                 y = reorder(term, estimate),
                 colour = estimate > 0)) +
      geom_vline(xintercept = 0, lty = 2, colour = "#555") +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                     height = 0.25, linewidth = 1.2) +
      geom_point(size = 4) +
      scale_colour_manual(values = c("TRUE"  = z_green,
                                     "FALSE" = z_red)) +
      labs(x = "Coefficient (β)", y = NULL) +
      dark_theme + theme(legend.position = "none")
  }, bg = "#1A1A1A")
  
  output$reg_d1 <- renderPlot({
    m <- reg_model()
    data.frame(fitted = fitted(m), resid = residuals(m)) |>
      ggplot(aes(x = fitted, y = resid)) +
      geom_point(colour = z_red, size = 2.5, alpha = 0.8) +
      geom_hline(yintercept = 0, lty = 2, colour = z_gold) +
      geom_smooth(se = FALSE, colour = z_blue, linewidth = 0.8) +
      labs(x = "Fitted Values", y = "Residuals") +
      dark_theme
  }, bg = "#1A1A1A")
  
  output$reg_d2 <- renderPlot({
    res <- residuals(reg_model())
    qqnorm(res, col = z_red, pch = 19, cex = 1.4,
           main = "", xlab = "Theoretical Quantiles",
           ylab = "Sample Quantiles",
           bg = "#1A1A1A", col.axis = "#888", col.lab = "#aaa")
    qqline(res, col = z_gold, lty = 2, lwd = 2)
  }, bg = "#1A1A1A")
  
  # COMPLAINT EXPLORER
  filtered <- reactive({
    d <- df
    if (input$f_ch != "All") d <- d |> filter(channel == input$f_ch)
    if (input$f_rs != "All") d <- d |> filter(failure_reason == input$f_rs)
    if (input$f_st != "All") d <- d |> filter(resolution_status == input$f_st)
    if (input$f_sg != "All") d <- d |> filter(customer_segment == input$f_sg)
    d
  })
  
  output$exp_tbl <- renderDT({
    filtered() |>
      select(complaint_id, date, month, channel,
             transaction_type, amount_naira,
             failure_reason, customer_segment,
             time_of_day, escalated,
             resolution_status, resolution_days,
             account_type, repeat_complaint) |>
      datatable(
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE, filter = "top"
      ) |>
      formatCurrency("amount_naira", currency = "₦", digits = 0) |>
      formatStyle("resolution_status",
                  backgroundColor = styleEqual(
                    c("Resolved","Unresolved"),
                    c("rgba(46,204,113,0.1)", "rgba(200,16,46,0.1)")
                  ))
  })
}

shinyApp(ui = ui, server = server)