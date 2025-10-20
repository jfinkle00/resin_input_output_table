# ==============================
# Resin I-O Scenario Dashboard
# ==============================
# Requires:
#  - resin_endmarket_tech_coeffs_composites.csv
#  - resin_endmarket_flows_composites.csv (optional; for mapping table)

library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)
library(DT)

# ---------- Build I-O (drops zero-coeff buckets) ----------
build_io <- function(
    tech_path = "resin_endmarket_tech_coeffs_composites.csv",
    desired_buckets = c("Automotive","Electrical","Packaging","Industrial",
                        "Construction_Materials","Medical_Composite"),
    drop_zero_cols = TRUE
) {
  if (!file.exists(tech_path)) {
    stop("Could not find '", tech_path, "'. Place it in the app folder.")
  }
  tech <- read_csv(tech_path, show_col_types = FALSE)
  
  # Keep ONLY the buckets you explicitly want, in that order
  end_markets <- desired_buckets
  
  pull_a <- function(b) {
    v <- tech %>% filter(Bucket == b) %>% pull(Tech_Coeff_aij)
    if (length(v) == 0 || is.na(v[1])) return(0)
    v[1]
  }
  a_vec <- setNames(vapply(end_markets, pull_a, numeric(1)), end_markets)
  
  # Optionally drop columns with zero resin coefficients (e.g., "Construction")
  if (drop_zero_cols) {
    end_markets <- names(a_vec)[a_vec != 0]
    a_vec <- a_vec[a_vec != 0]
  }
  
  industries <- c("Resin", end_markets)
  n <- length(industries)
  A <- matrix(0, nrow = n, ncol = n, dimnames = list(industries, industries))
  if (length(end_markets)) A["Resin", end_markets] <- a_vec
  
  I_mat <- diag(n); colnames(I_mat) <- rownames(I_mat) <- industries
  Leontief <- solve(I_mat - A)
  
  list(A = A, L = Leontief, industries = industries,
       end_markets = end_markets, tech = tech, resin_row = a_vec)
}

# Helper: null -> 0
nz0 <- function(x) if (is.null(x)) 0 else x

# ---------- UI ----------
ui <- page_fluid(
  theme = bs_theme(bootswatch = "flatly"),
  title = "Resin I-O Scenario Dashboard",
  layout_columns(
    col_widths = c(4, 8),
    # LEFT: controls
    card(
      full_screen = TRUE,
      card_header("Scenario Inputs ($, nominal)"),
      uiOutput("shocks_ui"),
      div(class = "mt-3",
          actionButton("run_btn", "Run Scenario", class = "btn btn-primary"),
          actionButton("reset_btn", "Reset", class = "btn btn-outline-secondary ms-2")
      ),
      hr(),
      h5("Scenario Manager"),
      textInput("scenario_name", "Name this scenario", placeholder = "e.g., +1B Auto, +0.5B Medical_Composite"),
      div(
        actionButton("save_scn", "Save to Session", class = "btn btn-success"),
        actionButton("load_scn", "Load Selected", class = "btn btn-outline-primary ms-2")
      ),
      selectInput("saved_scn_pick", "Saved scenarios (session)", choices = character(0)),
      downloadButton("dl_results", "Download Results (CSV)", class = "btn btn-outline-dark")
    ),
    # RIGHT: outputs
    card(
      full_screen = TRUE,
      card_header("Key Metrics"),
      fluidRow(
        column(4, uiOutput("kpi_total_fd")),
        column(4, uiOutput("kpi_resin_req")),
        column(4, uiOutput("kpi_resin_intensity"))
      ),
      hr(),
      tabsetPanel(
        tabPanel("Charts",
                 fluidRow(
                   column(6, plotOutput("plot_fd", height = 300)),
                   column(6, plotOutput("plot_resin_by_bucket", height = 300))
                 ),
                 hr(),
                 plotOutput("plot_cum_sensitivity", height = 320)
        ),
        tabPanel("Tables",
                 h5("A Matrix (Technical Coefficients) — Resin row"),
                 dataTableOutput("table_A"),
                 hr(),
                 h5("Leontief Inverse (Resin row)"),
                 dataTableOutput("table_L_resin"),
                 hr(),
                 h5("Scenario Final Demand (y) and Resin Requirements"),
                 dataTableOutput("table_results"),
                 hr(),
                 h5("Bucket → BEA columns matched (from flows file)"),
                 dataTableOutput("table_mapping")
        ),
        tabPanel("About",
                 div(
                   p("This dashboard uses a reduced resin-focused input–output model."),
                   tags$ul(
                     tags$li("Only the Resin → End-Market coefficients are non-zero in A."),
                     tags$li("Buckets with zero Resin coefficients are dropped automatically."),
                     tags$li("Construction plastics captured via 'Construction_Materials'."),
                     tags$li("Medical plastics broadened via 'Medical_Composite'.")
                   ),
                   p("Provide shocks as positive/negative dollar values. Results compute total resin output via x = (I − A)⁻¹ y.")
                 ))
      )
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  
  # Load I-O objects (A, Leontief, etc.)
  io <- build_io()
  A <- io$A; L <- io$L; industries <- io$industries; end_markets <- io$end_markets
  
  # Numeric inputs for each end-market (Resin not shocked here)
  output$shocks_ui <- renderUI({
    tagList(
      div(class = "small text-muted mb-2",
          "Enter final-demand shocks by end-market (in dollars). Use 1e9 for $1B, etc.")
    ) |>
      tagAppendChild(
        tagList(lapply(end_markets, function(em) {
          numericInput(
            inputId = paste0("shock_", em),
            label = em,
            value = 0,
            step = 1e6,
            width = "100%"
          )
        }))
      )
  })
  
  # Scenario store (in session)
  saved_scenarios <- reactiveVal(list())
  
  observeEvent(input$save_scn, {
    nm <- input$scenario_name
    if (is.null(nm) || !nzchar(nm)) {
      showNotification("Please enter a scenario name before saving.", type = "warning")
      return()
    }
    y <- numeric(length(industries)); names(y) <- industries
    y["Resin"] <- 0
    for (em in end_markets) y[em] <- nz0(input[[paste0("shock_", em)]])
    s <- saved_scenarios(); s[[nm]] <- y; saved_scenarios(s)
    updateSelectInput(session, "saved_scn_pick", choices = names(s), selected = nm)
    showNotification(paste0("Saved scenario '", nm, "'"), type = "message")
  })
  
  observeEvent(input$load_scn, {
    s <- saved_scenarios(); nm <- input$saved_scn_pick
    if (is.null(nm) || !nzchar(nm) || is.null(s[[nm]])) return()
    y <- s[[nm]]
    for (em in end_markets) {
      updateNumericInput(session, paste0("shock_", em), value = y[em])
    }
    showNotification(paste0("Loaded scenario '", nm, "'"), type = "message")
  })
  
  observeEvent(input$reset_btn, {
    for (em in end_markets) updateNumericInput(session, paste0("shock_", em), value = 0)
  })
  
  # Run scenario
  scenario <- eventReactive(input$run_btn, {
    y <- numeric(length(industries)); names(y) <- industries
    y["Resin"] <- 0
    for (em in end_markets) y[em] <- nz0(input[[paste0("shock_", em)]])
    x <- as.numeric(L %*% y); names(x) <- industries
    list(y = y, x = x, resin_out = x["Resin"])
  }, ignoreInit = TRUE)
  
  # KPIs
  output$kpi_total_fd <- renderUI({
    req(scenario()); total_fd <- sum(scenario()$y[names(scenario()$y) != "Resin"])
    card(card_header("Total Final-Demand Shock"), h3(dollar(total_fd)))
  })
  output$kpi_resin_req <- renderUI({
    req(scenario()); card(card_header("Total Resin Required"), h3(dollar(scenario()$resin_out)))
  })
  output$kpi_resin_intensity <- renderUI({
    req(scenario())
    total_fd <- sum(scenario()$y[names(scenario()$y) != "Resin"])
    intensity <- ifelse(total_fd == 0, 0, scenario()$resin_out / total_fd)
    card(card_header("Resin Intensity (Resin $ / Shock $)"), h3(percent(intensity, accuracy = 0.01)))
  })
  
  # Tables
  output$table_A <- renderDataTable({
    a_df <- as.data.frame(A)
    # Show only the Resin row to avoid all-zero rows clutter
    a_df <- a_df["Resin", , drop = FALSE]
    a_df <- data.frame(Input_Industry = rownames(a_df), a_df, row.names = NULL)
    datatable(a_df, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$table_L_resin <- renderDataTable({
    L_resin <- data.frame(To = colnames(L), L_ij = as.numeric(L["Resin", ]))
    datatable(L_resin, options = list(pageLength = 10))
  })
  
  output$table_results <- renderDataTable({
    req(scenario())
    y <- scenario()$y; x <- scenario()$x
    df <- data.frame(
      Industry = names(y),
      FinalDemand_Shock = as.numeric(y),
      TotalOutput_Required = as.numeric(x),
      row.names = NULL
    )
    datatable(df, options = list(scrollX = TRUE, pageLength = 12))
  })
  
  # Optional mapping table (bucket -> BEA cols) if the flows file is present
  output$table_mapping <- renderDataTable({
    if (!file.exists("resin_endmarket_flows_composites.csv")) return(NULL)
    flows <- read_csv("resin_endmarket_flows_composites.csv", show_col_types = FALSE)
    # Sanitize names so we can safely select the value column formerly named "Resin_Input_$"
    names(flows) <- make.names(names(flows))  # "Resin_Input_$" -> "Resin_Input__"
    datatable(
      flows %>% select(Bucket, Matched_BEACols, Resin_Input__),
      options = list(scrollX = TRUE, pageLength = 10)
    )
  })
  
  # Charts
  output$plot_fd <- renderPlot({
    req(scenario())
    y <- scenario()$y[end_markets]
    df <- data.frame(Bucket = factor(names(y), levels = end_markets),
                     Shock = as.numeric(y))
    ggplot(df, aes(Bucket, Shock)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = dollar) +
      labs(title = "Final-Demand Shocks by Bucket", x = NULL, y = "Dollars")
  })
  
  output$plot_resin_by_bucket <- renderPlot({
    # Direct resin requirement by bucket = a_ij * y_j (Resin row only)
    req(scenario())
    y <- scenario()$y
    a_row <- A["Resin", , drop = TRUE]
    contrib <- a_row[names(y)] * y
    df <- data.frame(Bucket = factor(names(contrib), levels = names(contrib)),
                     Resin_Dollars = as.numeric(contrib))
    df <- df[df$Bucket != "Resin", , drop = FALSE]
    ggplot(df, aes(Bucket, Resin_Dollars)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = dollar) +
      labs(title = "Direct Resin Requirement by Bucket (a_ij × shock)", x = NULL, y = "Dollars")
  })
  
  output$plot_cum_sensitivity <- renderPlot({
    a_row <- A["Resin", end_markets, drop = TRUE]
    df <- data.frame(Bucket = factor(end_markets, levels = end_markets),
                     Resin_per_Shock = as.numeric(a_row))
    ggplot(df, aes(Bucket, Resin_per_Shock)) +
      geom_point(size = 3) +
      geom_segment(aes(xend = Bucket, y = 0, yend = Resin_per_Shock)) +
      coord_flip() +
      scale_y_continuous(labels = percent_format(accuracy = 0.01)) +
      labs(title = "Resin per $ of Final-Demand Shock (Coefficient a_ij)",
           x = NULL, y = "Resin $ / Shock $")
  })
  
  # Download
  output$dl_results <- downloadHandler(
    filename = function() paste0("resin_io_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      req(scenario())
      y <- scenario()$y; x <- scenario()$x
      a_row <- A["Resin", , drop = TRUE]
      direct_contrib <- a_row[names(y)] * y
      
      out <- data.frame(
        Industry = names(y),
        FinalDemand_Shock = as.numeric(y),
        Resin_Coeff_aij = as.numeric(a_row[names(y)]),
        Direct_Resin_ByBucket = as.numeric(direct_contrib),
        TotalOutput_Required = as.numeric(x),
        row.names = NULL
      )
      write_csv(out, file)
    }
  )
}

shinyApp(ui, server)
