# enhanced_io_dashboard.R
# ============================================================
# Enhanced Shiny IO Dashboard — Plastics/Resin Demand Analytics
# - Modern dashboard UI with shinydashboard
# - Enhanced plastics subsector tracking (326110-326190)
# - Baseline vs scenario comparisons
# - Interactive visualizations with plotly
# - Sensitivity analysis
# - Comprehensive reporting
# ============================================================

library(shiny)
library(shinydashboard)
library(readxl)
library(stringr)
library(MASS)
library(DT)
library(plotly)
library(scales)

# --------- CONFIG ---------
DEFAULT_XLSX_PATH <- "C:/Users/Jfink/OneDrive/Desktop/10302025IOTest.xlsx"
DEFAULT_SHEET     <- "Sheet3"
PLASTICS_CODES    <- c("325211","326110","326120","326130","326140","326150","326160","326190")

# Plastics subsector labels
PLASTICS_LABELS <- c(
  "325211" = "Plastics material and resin",
  "326110" = "Plastics packaging materials",
  "326120" = "Plastics pipe, fittings, profile shapes",
  "326130" = "Laminated plastics plate, sheet",
  "326140" = "Polystyrene foam products",
  "326150" = "Urethane and foam products",
  "326160" = "Plastics bottle manufacturing",
  "326190" = "Other plastics products"
)

# Brand palette
BRAND_COLS <- c("#04303a", "#351532", "#376452", "#ba1e23", "#20db91")
BRAND_BG   <- "#f4fff8"

# --------- Helpers ---------
clean_code_names <- function(x){
  x <- as.character(x)
  x <- str_trim(x)
  mask <- grepl("^\\d+\\.0$", x)
  x[mask] <- sub("\\.0$", "", x[mask])
  x
}

invert_stable <- function(M, ridge0=1e-9, max_tries=6){
  for (i in 0:max_tries) {
    ridge <- ridge0 * (10^i)
    M2 <- M + diag(ridge, nrow(M))
    inv <- tryCatch(solve(M2), error=function(e) NULL)
    if (!is.null(inv)) return(list(L=inv, method=sprintf("solve(ridge=%.0e)", ridge)))
  }
  list(L = MASS::ginv(M), method="ginv pseudoinverse")
}

io_from_excel_base <- function(path, sheet, verbose=TRUE){
  
  if(verbose) cat("Reading Excel file...\n")
  raw <- readxl::read_excel(path, sheet=sheet, col_names=FALSE)
  raw <- as.data.frame(raw, stringsAsFactors=FALSE)
  
  if(verbose) cat("File dimensions:", nrow(raw), "rows x", ncol(raw), "columns\n")
  
  # FIX #1: Header row is 1 (not 2)
  header_row <- 1
  if(verbose) cat("Using header row:", header_row, "\n")
  
  # FIX #2: Handle NA in headers
  headers <- as.character(unlist(raw[header_row, , drop=TRUE]))
  headers[is.na(headers)] <- ""  # Replace NA with empty string
  
  # Create placeholders for blank headers
  placeholders <- paste0("col", seq_along(headers))
  is_blank <- headers == ""  # Changed from is.na() to == ""
  headers[is_blank] <- placeholders[is_blank]
  headers <- make.unique(headers)
  names(raw) <- headers
  
  if(verbose) cat("Total columns:", length(headers), "\n")
  
  # Data starts at row 2 (immediately after header row 1)
  df <- raw[(header_row+1):nrow(raw), , drop=FALSE]
  if(verbose) cat("Data rows:", nrow(df), "\n")
  
  # FIX #3: Handle NA in row codes
  row_codes <- as.character(df[,1,drop=TRUE])
  row_codes[is.na(row_codes)] <- ""  # Replace NA with empty string
  
  # Remove first column (codes)
  df <- df[, -1, drop=FALSE]
  
  # Clean column names
  colnames(df) <- clean_code_names(colnames(df))
  
  if(verbose) cat("Converting to numeric and replacing NA with 0...\n")
  
  # FIX #4: CRITICAL - Replace ALL NA values with 0
  for (j in seq_len(ncol(df))) {
    df[,j] <- suppressWarnings(as.numeric(df[,j]))
    df[is.na(df[,j]), j] <- 0  # THIS IS THE KEY FIX!
  }
  
  all_cols <- colnames(df)
  
  # Find T001 (total output)
  total_output_col <- "T001"
  if(!(total_output_col %in% all_cols)) {
    # Try case-insensitive search
    idx <- which(toupper(all_cols) == "T001")
    if(length(idx) == 1) {
      total_output_col <- all_cols[idx]
    } else {
      if(verbose) {
        cat("ERROR: T001 not found\n")
        cat("Available columns (last 10):", paste(tail(all_cols, 10), collapse=", "), "\n")
      }
      stop("Cannot find T001 column")
    }
  }
  
  if(verbose) cat("Found T001 at column:", which(all_cols == total_output_col), "\n")
  
  # Find FD columns
  fd_cols <- c("S00101","S00102","GSLGE","GSLGH","GSLGO","S00201","S00202","S00203")
  fd_cols_present <- intersect(fd_cols, all_cols)
  
  if (length(fd_cols_present)==0){
    if(verbose) cat("Standard FD columns not found, searching for alternatives...\n")
    fd_cols_present <- all_cols[grepl("^(S001|S002|GSLG|F0|F1)", all_cols)]
    if(length(fd_cols_present) == 0) {
      stop("No FD columns found")
    }
  }
  
  if(verbose) {
    cat("FD columns found:", length(fd_cols_present), "\n")
    cat("First FD:", fd_cols_present[1], "\n")
  }
  
  # Find industry columns (before FD)
  first_fd_idx <- match(fd_cols_present[1], all_cols)
  if(verbose) cat("First FD at position:", first_fd_idx, "\n")
  
  ind_cols <- all_cols[1:(first_fd_idx-1)]
  ind_cols <- ind_cols[grepl("^[0-9A-Z]+$", ind_cols)]
  
  if(length(ind_cols) == 0) {
    stop("No industry columns detected")
  }
  
  if(verbose) cat("Industry columns found:", length(ind_cols), "\n")
  
  # Extract matrices
  U_full <- as.matrix(df[, ind_cols, drop=FALSE])
  F_full <- as.matrix(df[, fd_cols_present, drop=FALSE])
  X_full <- as.numeric(df[, total_output_col, drop=TRUE])
  rc_full <- row_codes
  
  # Make square
  n_full <- min(nrow(U_full), ncol(U_full))
  U_full <- U_full[1:n_full, 1:n_full, drop=FALSE]
  F_full <- F_full[1:n_full, , drop=FALSE]
  X_full <- X_full[1:n_full]
  rc_full <- rc_full[1:n_full]
  
  if(verbose) cat("Matrix after squaring:", n_full, "x", n_full, "\n")
  
  # Remove empty industries
  keep <- which((X_full>0) | rowSums(U_full)>0 | colSums(U_full)>0 | rowSums(F_full)>0)
  U <- U_full[keep, keep, drop=FALSE]
  F <- F_full[keep, , drop=FALSE]
  X <- X_full[keep]
  rc <- rc_full[keep]
  
  if(verbose) cat("After removing empties:", length(rc), "industries\n")
  
  # Remove zero-sum rows
  nz <- which((rowSums(U)+rowSums(F)+X) > 0)
  U <- U[nz, nz, drop=FALSE]
  F <- F[nz, , drop=FALSE]
  X <- X[nz]
  rc <- rc[nz]
  
  if(verbose) cat("After removing zero-sum:", length(rc), "industries\n")
  
  # Handle zero/negative X values
  n <- nrow(U)
  X[!is.finite(X) | X==0] <- 1e-9
  
  if(verbose) cat("Computing technical coefficients...\n")
  
  # Technical coefficients
  A <- U %*% diag(1 / X, nrow=n, ncol=n)
  A[!is.finite(A)] <- 0
  
  if(verbose) cat("Computing Leontief inverse...\n")
  
  # Leontief inverse
  M <- diag(n) - A
  inv_res <- invert_stable(M)
  L <- inv_res$L
  
  if(verbose) {
    cat("Inversion method:", inv_res$method, "\n")
    cat("Mean diagonal multiplier:", round(mean(diag(L)), 3), "\n")
  }
  
  # Check for NaN/Inf in L
  if(any(!is.finite(L))) {
    n_bad <- sum(!is.finite(L))
    warning("Leontief inverse contains ", n_bad, " non-finite values")
    L[!is.finite(L)] <- 0
  }
  
  if(verbose) cat("\n✓ IO table loaded successfully!\n\n")
  
  list(
    U = U,
    F = F,
    X = X,
    L = L,
    codes = rc,
    fd_cols = colnames(F),
    inv_method = inv_res$method,
    n_industries = length(rc),
    file_path = path,
    sheet_name = sheet
  )
}

# ---- End-market mapping ----
.endmarket_specs <- list(
  "Agriculture"                                     = c("11"),
  "Mining"                                          = c("21"),
  "Utilities"                                       = c("22"),
  "Construction"                                    = c("23"),
  "Wood Product and Non-metallic Manufacturing"     = c("321","327"),
  "Metal, Fabricated Metal, and Machinery Manufacturing" = c("331-333"),
  "Electrical Equipments and appliance product manufacturing" = c("334-335"),
  "Transportation"                                  = c("336"),
  "Furniture and other related prodcuts Manufacturing" = c("337"),
  "Medical Equipment and Supplies"                  = c("3391"),
  "Miscellaneous Manufacturing"                     = c("3399"),
  "Food, Beverage, and Tobacco Manufacturing"       = c("311-312"),
  "Textile Mills and Apparel/fabric manufacturing"  = c("313-316"),
  "Paper and Printing"                              = c("322-323"),
  "Petroleum and Chemical manufacturing"            = c("324-325"),
  "Plastics Product and Rubber"                     = c("326"),
  "Wholesale Trade"                                 = c("42"),
  "Retail Trade"                                    = c("44-45"),
  "All Other Retail"                                = c("4B"),
  "Transportation and Warehousing"                  = c("48-49"),
  "Information and Finance"                         = c("51-52"),
  "Real Estate and Leasing"                         = c("53"),
  "Professional, Scientific, and Technical Services"= c("54"),
  "Management and Administration"                   = c("55-56"),
  "Education and Health Care"                       = c("61-62"),
  "Arts, Recreation, Accomadation, and Food Services" = c("71-72"),
  "Other Services excluding Public Admin"           = c("81"),
  "State, Federal, and Local Government"            = c("S and G")
)

.build_prefix_patterns <- function(spec_vec){
  pat_list <- character(0)
  for (s in spec_vec){
    s <- gsub("\\s+", " ", s)
    parts <- unlist(strsplit(s, "\\sand\\s|;|,"))
    parts <- trimws(parts)
    for (p in parts){
      if (grepl("^[0-9]{2,}-[0-9]{2,}$", p)) {
        a <- as.integer(sub("-.*$", "", p)); b <- as.integer(sub("^.*-", "", p))
        seqv <- a:b
        pat_list <- c(pat_list, paste0("^", paste0(seqv, collapse="|^")))
      } else if (toupper(p) == "4B") {
        pat_list <- c(pat_list, "^4B")
      } else if (toupper(p) %in% c("S AND G","S&G","SG","G","S")) {
        pat_list <- c(pat_list, "^(S|G)")
      } else if (grepl("^[0-9]{2,}$", p)) {
        pat_list <- c(pat_list, paste0("^", p))
      }
    }
  }
  if (length(pat_list)==0) return(NULL)
  paste(pat_list, collapse="|")
}

make_bucket_fn_from_specs <- function(specs){
  compiled <- lapply(specs, .build_prefix_patterns)
  buckets <- names(compiled)
  function(code_vec){
    out <- rep("Other", length(code_vec))
    for (i in seq_along(code_vec)){
      c0 <- toupper(as.character(code_vec[i]))
      for (b in seq_along(compiled)){
        pat <- compiled[[b]]
        if (!is.null(pat) && grepl(pat, c0)) { out[i] <- buckets[b]; break }
      }
    }
    out
  }
}

bucket_by_prefix <- make_bucket_fn_from_specs(.endmarket_specs)

distribute_totals_to_f <- function(buckets, groups, F, totals_named){
  n <- nrow(F)
  f <- rep(0, n); names(f) <- names(buckets)
  baseline_fd <- rowSums(F)
  for (g in groups){
    target_total <- totals_named[g]
    idx <- which(buckets == g)
    if (length(idx)==0 || is.na(target_total) || target_total<=0) next
    base <- baseline_fd[idx]
    share <- if (sum(base) > 0) base / sum(base) else rep(1/length(idx), length(idx))
    f[idx] <- f[idx] + as.numeric(target_total) * share
  }
  f
}

build_G_groups <- function(buckets, groups, F){
  n <- nrow(F); K <- length(groups)
  G <- matrix(0, nrow=n, ncol=K); colnames(G) <- groups
  f_base <- rowSums(F)
  grp_totals_base <- tapply(f_base, INDEX=buckets, FUN=sum)
  for (g in groups){
    total_g <- if (!is.null(grp_totals_base[g])) grp_totals_base[g] else 0
    totals <- rep(0, K); names(totals) <- groups; totals[g] <- total_g
    G[, g] <- distribute_totals_to_f(buckets, groups, F, totals)
  }
  G
}

calibrate_endmarket_scalers <- function(L, G_groups, X){
  Z <- L %*% G_groups
  ZtZ <- t(Z) %*% Z
  ZtX <- t(Z) %*% X
  alpha <- tryCatch(solve(ZtZ, ZtX), error=function(e) MASS::ginv(ZtZ) %*% ZtX)
  alpha <- as.numeric(alpha); alpha[alpha < 0] <- 0
  names(alpha) <- colnames(G_groups)
  alpha
}

# ---------------- UI ----------------
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Plastics IO Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("End-Market Inputs", tabName = "inputs", icon = icon("sliders")),
      menuItem("Plastics Analysis", tabName = "plastics", icon = icon("industry")),
      menuItem("All Industries", tabName = "all_industries", icon = icon("table")),
      menuItem("Comparison", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("Sensitivity", tabName = "sensitivity", icon = icon("chart-line")),
      menuItem("Settings", tabName = "settings", icon = icon("cog"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML(sprintf("
        .content-wrapper, .right-side {
          background-color: %s;
        }
        .box {
          border-top-color: #04303a;
        }
        .small-box h3 {
          font-size: 32px;
          font-weight: bold;
        }
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #04303a;
        }
      ", BRAND_BG)))
    ),
    
    tabItems(
      # Dashboard Tab
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("vbox_total_plastics", width = 3),
                valueBoxOutput("vbox_top_subsector", width = 3),
                valueBoxOutput("vbox_pct_change", width = 3),
                valueBoxOutput("vbox_industries", width = 3)
              ),
              fluidRow(
                box(
                  title = "Plastics Demand Overview", status = "primary", solidHeader = TRUE, width = 8,
                  plotlyOutput("dash_plastics_plot", height = "350px")
                ),
                box(
                  title = "End-Market Distribution", status = "info", solidHeader = TRUE, width = 4,
                  plotlyOutput("dash_endmarket_pie", height = "350px")
                )
              ),
              fluidRow(
                box(
                  title = "Top 10 Industries (Scenario)", status = "success", solidHeader = TRUE, width = 12,
                  plotlyOutput("dash_top10_plot", height = "300px")
                )
              )
      ),
      
      # End-Market Inputs Tab
      tabItem(tabName = "inputs",
              fluidRow(
                box(
                  title = "Input Configuration", status = "warning", solidHeader = TRUE, width = 4,
                  radioButtons("mode", "Input mode", 
                               choices = c("Absolute totals"="abs", "Percent change vs baseline"="%"), 
                               inline=FALSE, selected = "abs"),
                  checkboxInput("use_alpha", "Apply α (calibrated scalers)", value = TRUE),
                  actionButton("calib", "Auto-calibrate α", class = "btn-warning", icon = icon("calculator")),
                  hr(),
                  actionButton("reset_inputs", "Reset All Inputs", class = "btn-danger", icon = icon("undo")),
                  actionButton("run", "Run Scenario", class = "btn-success btn-lg", icon = icon("play"), 
                               style = "margin-top: 10px; width: 100%;")
                ),
                box(
                  title = "End-Market Inputs", status = "primary", solidHeader = TRUE, width = 8,
                  collapsible = TRUE,
                  fluidRow(
                    column(6,
                           numericInput("val_construction", "Construction", value = NA, step = 1000),
                           numericInput("val_transportation", "Transportation (336)", value = NA, step = 1000),
                           numericInput("val_medical", "Medical Equipment", value = NA, step = 1000),
                           numericInput("val_retail", "Retail Trade (44-45)", value = NA, step = 1000),
                           numericInput("val_infofin", "Information & Finance", value = NA, step = 1000),
                           numericInput("val_prof", "Professional/Scientific/Technical", value = NA, step = 1000),
                           numericInput("val_edhealth", "Education & Health Care", value = NA, step = 1000),
                           numericInput("val_govt", "Government", value = NA, step = 1000)
                    ),
                    column(6,
                           numericInput("val_agriculture", "Agriculture", value = NA, step = 1000),
                           numericInput("val_manufacturing", "All Other Manufacturing", value = NA, step = 1000),
                           numericInput("val_allretail", "All Other Retail (4B)", value = NA, step = 1000),
                           numericInput("val_transware", "Transportation & Warehousing", value = NA, step = 1000),
                           numericInput("val_realestate", "Real Estate & Leasing", value = NA, step = 1000),
                           numericInput("val_mgmtadmin", "Management & Admin", value = NA, step = 1000),
                           numericInput("val_artsaccfood", "Arts/Recreation/Food", value = NA, step = 1000),
                           numericInput("val_othersvc", "Other Services", value = NA, step = 1000)
                    )
                  ),
                  actionButton("show_more_inputs", "Show All End Markets", icon = icon("plus-circle"))
                )
              ),
              fluidRow(
                box(
                  title = "Calibrated Alpha Values", status = "info", solidHeader = TRUE, width = 12,
                  collapsible = TRUE, collapsed = TRUE,
                  DTOutput("alpha_table")
                )
              )
      ),
      
      # Plastics Analysis Tab
      tabItem(tabName = "plastics",
              fluidRow(
                box(
                  title = "Plastics Subsector Breakdown", status = "primary", solidHeader = TRUE, width = 12,
                  plotlyOutput("plastics_subsector_plot", height = "450px")
                )
              ),
              fluidRow(
                box(
                  title = "Baseline vs Scenario Comparison", status = "success", solidHeader = TRUE, width = 6,
                  plotlyOutput("plastics_comparison_plot", height = "400px")
                ),
                box(
                  title = "Plastics Demand Details", status = "info", solidHeader = TRUE, width = 6,
                  DTOutput("plastics_detail_table")
                )
              ),
              fluidRow(
                box(
                  title = "Waterfall Chart - Changes from Baseline", status = "warning", solidHeader = TRUE, width = 12,
                  plotlyOutput("plastics_waterfall", height = "350px")
                )
              )
      ),
      
      # All Industries Tab
      tabItem(tabName = "all_industries",
              fluidRow(
                box(
                  title = "All Industries Output", status = "primary", solidHeader = TRUE, width = 12,
                  plotlyOutput("allPlot", height = "520px")
                )
              ),
              fluidRow(
                box(
                  title = "Industry Results Table", status = "info", solidHeader = TRUE, width = 12,
                  DTOutput("allTbl"),
                  downloadButton("dl_all", "Download CSV", class = "btn-primary")
                )
              )
      ),
      
      # Comparison Tab
      tabItem(tabName = "comparison",
              fluidRow(
                box(
                  title = "Baseline vs Scenario - Top Industries", status = "success", solidHeader = TRUE, width = 12,
                  plotlyOutput("comparison_plot", height = "500px")
                )
              ),
              fluidRow(
                box(
                  title = "Absolute Changes", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("abs_change_plot", height = "400px")
                ),
                box(
                  title = "Percentage Changes", status = "warning", solidHeader = TRUE, width = 6,
                  plotlyOutput("pct_change_plot", height = "400px")
                )
              ),
              fluidRow(
                box(
                  title = "Change Statistics", status = "info", solidHeader = TRUE, width = 12,
                  DTOutput("change_stats_table")
                )
              )
      ),
      
      # Sensitivity Tab
      tabItem(tabName = "sensitivity",
              fluidRow(
                box(
                  title = "Sensitivity Analysis Controls", status = "warning", solidHeader = TRUE, width = 4,
                  selectInput("sens_endmarket", "Select End Market", 
                              choices = names(.endmarket_specs)),
                  sliderInput("sens_range", "Variation Range (%)", 
                              min = -50, max = 50, value = c(-25, 25), step = 5),
                  sliderInput("sens_steps", "Number of Steps", 
                              min = 5, max = 20, value = 10, step = 1),
                  actionButton("run_sensitivity", "Run Sensitivity Analysis", 
                               class = "btn-primary", icon = icon("chart-area"))
                ),
                box(
                  title = "Sensitivity Results - Plastics Demand", status = "primary", solidHeader = TRUE, width = 8,
                  plotlyOutput("sensitivity_plot", height = "400px")
                )
              ),
              fluidRow(
                box(
                  title = "Multi-Subsector Sensitivity", status = "info", solidHeader = TRUE, width = 12,
                  plotlyOutput("multi_sensitivity_plot", height = "400px")
                )
              )
      ),
      
      # Settings Tab
      tabItem(tabName = "settings",
              fluidRow(
                box(
                  title = "Data Source", status = "primary", solidHeader = TRUE, width = 6,
                  fileInput("file", "Upload BEA Use workbook (.xlsx)", accept = c(".xlsx")),
                  textInput("path", "Or use local path", value = DEFAULT_XLSX_PATH),
                  textInput("sheet", "Sheet name", value = DEFAULT_SHEET),
                  actionButton("reload_data", "Reload Data", class = "btn-info", icon = icon("refresh"))
                ),
                box(
                  title = "System Information", status = "info", solidHeader = TRUE, width = 6,
                  verbatimTextOutput("invinfo"),
                  hr(),
                  h5("End-Market Mapping"),
                  fileInput("mapcsv", "Upload custom mapping CSV (code, end_market)", accept=".csv")
                )
              ),
              fluidRow(
                box(
                  title = "Code Mapping Preview", status = "success", solidHeader = TRUE, width = 12,
                  collapsible = TRUE, collapsed = TRUE,
                  DTOutput("mapPreview")
                )
              )
      )
    )
  )
)

# ---------------- SERVER ----------------
server <- function(input, output, session) {
  
  # Reactive values for state management
  rv <- reactiveValues(
    baseline_results = NULL,
    scenario_results = NULL,
    io_loaded = FALSE
  )
  
  # ---- Load IO ----
  io_data <- reactive({
    input$reload_data  # Dependency
    infile <- input$file
    path <- if (!is.null(infile)) infile$datapath else input$path
    validate(need(file.exists(path), "Please upload a workbook or provide a valid local path."))
    
    io <- io_from_excel_base(path = path, sheet = input$sheet)
    
    # Store baseline
    if (!rv$io_loaded) {
      buckets <- bucket_by_prefix(io$codes)
      names(buckets) <- io$codes
      f_base <- rowSums(io$F)
      names(f_base) <- io$codes
      xhat_base <- as.numeric(io$L %*% f_base)
      names(xhat_base) <- io$codes
      rv$baseline_results <- xhat_base
      rv$io_loaded <- TRUE
    }
    
    io
  })
  
  # ---- Mapping ----
  buckets_reactive <- reactive({
    io <- io_data(); codes <- io$codes
    buckets <- bucket_by_prefix(codes)
    if (!is.null(input$mapcsv)) {
      mapdf <- tryCatch(read.csv(input$mapcsv$datapath, stringsAsFactors = FALSE), error=function(e) NULL)
      if (!is.null(mapdf) && all(c("code","end_market") %in% names(mapdf))) {
        mm <- match(codes, mapdf$code); overwrite <- !is.na(mm)
        buckets[overwrite] <- mapdf$end_market[mm[overwrite]]
      }
    }
    names(buckets) <- codes
    buckets
  })
  
  # ---- Alpha ----
  alpha_vals <- reactiveVal(NULL)
  
  observeEvent(input$calib, {
    io <- io_data()
    buckets <- buckets_reactive()
    groups <- names(.endmarket_specs)
    G_groups <- build_G_groups(buckets, groups, io$F)
    alpha <- calibrate_endmarket_scalers(io$L, G_groups, io$X)
    alpha_vals(alpha)
    showNotification("α calibrated successfully!", type = "message", duration = 3)
  })
  
  output$alpha_table <- renderDT({
    a <- alpha_vals()
    if (is.null(a)) return(NULL)
    df <- data.frame(
      EndMarket = names(a), 
      Alpha = round(as.numeric(a), 4),
      stringsAsFactors = FALSE
    )
    datatable(df, rownames = FALSE, options = list(pageLength = 10, dom = 't'))
  })
  
  # ---- Reset inputs ----
  observeEvent(input$reset_inputs, {
    updateNumericInput(session, "val_construction", value = NA)
    updateNumericInput(session, "val_agriculture", value = NA)
    updateNumericInput(session, "val_transportation", value = NA)
    updateNumericInput(session, "val_medical", value = NA)
    updateNumericInput(session, "val_manufacturing", value = NA)
    updateNumericInput(session, "val_retail", value = NA)
    updateNumericInput(session, "val_allretail", value = NA)
    updateNumericInput(session, "val_transware", value = NA)
    updateNumericInput(session, "val_infofin", value = NA)
    updateNumericInput(session, "val_realestate", value = NA)
    updateNumericInput(session, "val_prof", value = NA)
    updateNumericInput(session, "val_mgmtadmin", value = NA)
    updateNumericInput(session, "val_edhealth", value = NA)
    updateNumericInput(session, "val_artsaccfood", value = NA)
    updateNumericInput(session, "val_othersvc", value = NA)
    updateNumericInput(session, "val_govt", value = NA)
    showNotification("All inputs reset", type = "warning", duration = 2)
  })
  
  # ---- Build scenario f ----
  scenario_f <- eventReactive(input$run, {
    io <- io_data()
    buckets <- buckets_reactive()
    groups <- names(.endmarket_specs)
    f_base <- rowSums(io$F); names(f_base) <- io$codes
    
    vals <- c(
      "Agriculture" = input$val_agriculture,
      "Construction" = input$val_construction,
      "Transportation" = input$val_transportation,
      "Medical Equipment and Supplies" = input$val_medical,
      "Retail Trade" = input$val_retail,
      "All Other Retail" = input$val_allretail,
      "Transportation and Warehousing" = input$val_transware,
      "Information and Finance" = input$val_infofin,
      "Real Estate and Leasing" = input$val_realestate,
      "Professional, Scientific, and Technical Services" = input$val_prof,
      "Management and Administration" = input$val_mgmtadmin,
      "Education and Health Care" = input$val_edhealth,
      "Arts, Recreation, Accomadation, and Food Services" = input$val_artsaccfood,
      "Other Services excluding Public Admin" = input$val_othersvc,
      "State, Federal, and Local Government" = input$val_govt
    )
    for (g in setdiff(groups, names(vals))) vals[g] <- NA
    vals <- vals[groups]
    
    a <- alpha_vals()
    if (!is.null(a) && isTRUE(input$use_alpha)) {
      mult_alpha <- rep(1, length(groups)); names(mult_alpha) <- groups
      mult_alpha[names(a)] <- a
    } else {
      mult_alpha <- rep(1, length(groups)); names(mult_alpha) <- groups
    }
    
    if (input$mode == "abs") {
      totals <- vals; totals[is.na(totals)] <- 0
      totals <- totals * mult_alpha
      f_abs <- distribute_totals_to_f(buckets, groups, io$F, totals)
      return(f_abs)
    } else {
      pct <- vals; pct[is.na(pct)] <- 0
      mult <- 1 + (pct/100)
      grp_totals_base <- tapply(f_base, INDEX=buckets, FUN=sum)
      for (g in setdiff(groups, names(grp_totals_base))) grp_totals_base[g] <- 0
      grp_totals_base <- grp_totals_base[groups]
      grp_target <- grp_totals_base * mult * mult_alpha
      f_abs <- distribute_totals_to_f(buckets, groups, io$F, grp_target)
      return(f_abs)
    }
  }, ignoreInit = TRUE)
  
  # ---- Results ----
  results <- reactive({
    req(scenario_f())
    io <- io_data()
    f <- scenario_f()
    xhat <- as.numeric(io$L %*% f)
    names(xhat) <- io$codes
    rv$scenario_results <- xhat
    xhat
  })
  
  # ---- Value Boxes ----
  output$vbox_total_plastics <- renderValueBox({
    xhat <- results()
    present <- intersect(PLASTICS_CODES, names(xhat))
    total <- if (length(present) > 0) sum(xhat[present]) else 0
    valueBox(
      value = paste0("$", format(round(total/1000, 1), big.mark = ","), "B"),
      subtitle = "Total Plastics Demand",
      icon = icon("industry"),
      color = "blue"
    )
  })
  
  output$vbox_top_subsector <- renderValueBox({
    xhat <- results()
    present <- intersect(PLASTICS_CODES, names(xhat))
    if (length(present) == 0) {
      top_code <- "N/A"
      top_label <- "N/A"
    } else {
      top_code <- present[which.max(xhat[present])]
      top_label <- PLASTICS_LABELS[top_code]
    }
    valueBox(
      value = substr(top_label, 1, 25),
      subtitle = paste("Top Subsector:", top_code),
      icon = icon("star"),
      color = "green"
    )
  })
  
  output$vbox_pct_change <- renderValueBox({
    baseline <- rv$baseline_results
    scenario <- results()
    if (is.null(baseline)) {
      pct <- 0
    } else {
      present <- intersect(PLASTICS_CODES, names(scenario))
      base_total <- if (length(present) > 0) sum(baseline[present]) else 0
      scen_total <- if (length(present) > 0) sum(scenario[present]) else 0
      pct <- if (base_total > 0) ((scen_total - base_total) / base_total) * 100 else 0
    }
    valueBox(
      value = paste0(ifelse(pct >= 0, "+", ""), round(pct, 1), "%"),
      subtitle = "Change from Baseline",
      icon = icon("chart-line"),
      color = if (pct >= 0) "yellow" else "red"
    )
  })
  
  output$vbox_industries <- renderValueBox({
    io <- io_data()
    valueBox(
      value = length(io$codes),
      subtitle = "Industries in Model",
      icon = icon("list"),
      color = "purple"
    )
  })
  
  # ---- Dashboard Plots ----
  output$dash_plastics_plot <- renderPlotly({
    xhat <- results()
    present <- intersect(PLASTICS_CODES, names(xhat))
    if (length(present) == 0) return(NULL)
    
    df <- data.frame(
      code = present,
      value = as.numeric(xhat[present]),
      label = PLASTICS_LABELS[present],
      stringsAsFactors = FALSE
    )
    df <- df[order(-df$value), , drop = FALSE]
    cols <- rep(BRAND_COLS, length.out = nrow(df))
    
    plot_ly(
      data = df,
      x = ~value, y = ~reorder(label, value),
      type = "bar", orientation = "h",
      marker = list(color = cols),
      hovertemplate = "<b>%{y}</b><br>Output: $%{x:,.0f}M<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Gross Output ($M)", zeroline = FALSE),
        yaxis = list(title = ""),
        margin = list(l = 180, r = 30, t = 30, b = 50),
        paper_bgcolor = BRAND_BG,
        plot_bgcolor = BRAND_BG
      )
  })
  
  output$dash_endmarket_pie <- renderPlotly({
    f <- scenario_f()
    if (is.null(f)) return(NULL)
    buckets <- buckets_reactive()
    grp_tot <- tapply(f, INDEX = buckets, FUN = sum)
    grp_tot <- sort(grp_tot, decreasing = TRUE)
    grp_tot <- head(grp_tot, 8)
    
    plot_ly(
      labels = names(grp_tot),
      values = as.numeric(grp_tot),
      type = "pie",
      marker = list(colors = rep(BRAND_COLS, length.out = length(grp_tot))),
      textposition = "inside",
      textinfo = "label+percent",
      hovertemplate = "<b>%{label}</b><br>$%{value:,.0f}M<br>%{percent}<extra></extra>"
    ) %>%
      layout(
        showlegend = FALSE,
        paper_bgcolor = BRAND_BG
      )
  })
  
  output$dash_top10_plot <- renderPlotly({
    xhat <- results()
    df <- data.frame(code = names(xhat), value = as.numeric(xhat), stringsAsFactors = FALSE)
    df <- df[order(-df$value), , drop = FALSE]
    df <- head(df, 10)
    df <- df[order(df$value), , drop = FALSE]
    cols <- rep(BRAND_COLS, length.out = nrow(df))
    
    plot_ly(
      data = df,
      x = ~value, y = ~code,
      type = "bar", orientation = "h",
      marker = list(color = cols),
      hovertemplate = "<b>%{y}</b><br>$%{x:,.0f}M<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Gross Output ($M)"),
        yaxis = list(title = ""),
        margin = list(l = 100, r = 30, t = 10, b = 40),
        paper_bgcolor = BRAND_BG,
        plot_bgcolor = BRAND_BG
      )
  })
  
  # ---- Plastics Analysis ----
  output$plastics_subsector_plot <- renderPlotly({
    xhat <- results()
    baseline <- rv$baseline_results
    present <- intersect(PLASTICS_CODES, names(xhat))
    if (length(present) == 0) return(NULL)
    
    df <- data.frame(
      code = present,
      scenario = as.numeric(xhat[present]),
      baseline = if (!is.null(baseline)) as.numeric(baseline[present]) else 0,
      label = PLASTICS_LABELS[present],
      stringsAsFactors = FALSE
    )
    
    plot_ly(df) %>%
      add_trace(x = ~baseline, y = ~label, type = "bar", orientation = "h",
                name = "Baseline", marker = list(color = "#cccccc"),
                hovertemplate = "<b>%{y}</b><br>Baseline: $%{x:,.0f}M<extra></extra>") %>%
      add_trace(x = ~scenario, y = ~label, type = "bar", orientation = "h",
                name = "Scenario", marker = list(color = BRAND_COLS[4]),
                hovertemplate = "<b>%{y}</b><br>Scenario: $%{x:,.0f}M<extra></extra>") %>%
      layout(
        barmode = "group",
        xaxis = list(title = "Gross Output ($M)"),
        yaxis = list(title = ""),
        legend = list(x = 0.7, y = 0.98),
        margin = list(l = 200, r = 30, t = 30, b = 50),
        paper_bgcolor = BRAND_BG,
        plot_bgcolor = BRAND_BG
      )
  })
  
  output$plastics_comparison_plot <- renderPlotly({
    xhat <- results()
    baseline <- rv$baseline_results
    if (is.null(baseline)) return(NULL)
    
    present <- intersect(PLASTICS_CODES, names(xhat))
    if (length(present) == 0) return(NULL)
    
    df <- data.frame(
      code = present,
      change = as.numeric(xhat[present]) - as.numeric(baseline[present]),
      label = PLASTICS_LABELS[present],
      stringsAsFactors = FALSE
    )
    df <- df[order(df$change), , drop = FALSE]
    
    cols <- ifelse(df$change >= 0, BRAND_COLS[5], BRAND_COLS[4])
    
    plot_ly(
      data = df,
      x = ~change, y = ~label,
      type = "bar", orientation = "h",
      marker = list(color = cols),
      hovertemplate = "<b>%{y}</b><br>Change: $%{x:,.0f}M<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Change from Baseline ($M)", zeroline = TRUE),
        yaxis = list(title = ""),
        margin = list(l = 200, r = 30, t = 30, b = 50),
        paper_bgcolor = BRAND_BG,
        plot_bgcolor = BRAND_BG
      )
  })
  
  output$plastics_detail_table <- renderDT({
    xhat <- results()
    baseline <- rv$baseline_results
    present <- intersect(PLASTICS_CODES, names(xhat))
    if (length(present) == 0) return(NULL)
    
    df <- data.frame(
      Code = present,
      Description = PLASTICS_LABELS[present],
      Baseline = if (!is.null(baseline)) round(baseline[present], 2) else 0,
      Scenario = round(xhat[present], 2),
      Change = if (!is.null(baseline)) round(xhat[present] - baseline[present], 2) else 0,
      Pct_Change = if (!is.null(baseline)) {
        pct <- ((xhat[present] - baseline[present]) / baseline[present]) * 100
        paste0(round(pct, 1), "%")
      } else "N/A",
      stringsAsFactors = FALSE
    )
    
    datatable(df, rownames = FALSE, options = list(pageLength = 10, dom = 't')) %>%
      formatCurrency(c("Baseline", "Scenario", "Change"), "$", digits = 0)
  })
  
  output$plastics_waterfall <- renderPlotly({
    xhat <- results()
    baseline <- rv$baseline_results
    if (is.null(baseline)) return(NULL)
    
    present <- intersect(PLASTICS_CODES, names(xhat))
    if (length(present) == 0) return(NULL)
    
    changes <- xhat[present] - baseline[present]
    names(changes) <- PLASTICS_LABELS[present]
    
    x_vals <- c("Baseline", names(changes), "Scenario")
    measures <- c("absolute", rep("relative", length(changes)), "total")
    
    base_total <- sum(baseline[present])
    scen_total <- sum(xhat[present])
    y_vals <- c(base_total, changes, scen_total)
    
    plot_ly(
      x = x_vals,
      y = y_vals,
      measure = measures,
      type = "waterfall",
      orientation = "v",
      connector = list(line = list(color = "rgb(63, 63, 63)")),
      decreasing = list(marker = list(color = BRAND_COLS[4])),
      increasing = list(marker = list(color = BRAND_COLS[5])),
      totals = list(marker = list(color = BRAND_COLS[1]))
    ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Gross Output ($M)"),
        margin = list(l = 60, r = 30, t = 30, b = 120),
        paper_bgcolor = BRAND_BG,
        plot_bgcolor = BRAND_BG
      )
  })
  
  # ---- All Industries ----
  output$allPlot <- renderPlotly({
    xhat <- results()
    df <- data.frame(code = names(xhat), value = as.numeric(xhat), stringsAsFactors = FALSE)
    df <- df[order(-df$value), , drop = FALSE]
    df <- head(df, 30)
    df <- df[order(df$value), , drop = FALSE]
    cols <- rep(BRAND_COLS, length.out = nrow(df))
    
    plot_ly(
      data = df,
      x = ~value, y = ~code,
      type = "bar", orientation = "h",
      marker = list(color = cols),
      hovertemplate = "<b>%{y}</b><br>$%{x:,.0f}M<extra></extra>"
    ) %>%
      layout(
        title = list(text = "Top 30 Industries — Scenario Output", x = 0),
        xaxis = list(title = "Gross Output ($M)"),
        yaxis = list(title = ""),
        margin = list(l = 140, r = 30, t = 60, b = 40),
        paper_bgcolor = BRAND_BG,
        plot_bgcolor = BRAND_BG
      )
  })
  
  output$allTbl <- renderDT({
    xhat <- results()
    baseline <- rv$baseline_results
    
    df <- data.frame(
      Code = names(xhat),
      Scenario = round(xhat, 2),
      stringsAsFactors = FALSE
    )
    
    if (!is.null(baseline)) {
      df$Baseline <- round(baseline[names(xhat)], 2)
      df$Change <- round(df$Scenario - df$Baseline, 2)
      df$Pct_Change <- paste0(round((df$Change / df$Baseline) * 100, 1), "%")
      df <- df[, c("Code", "Baseline", "Scenario", "Change", "Pct_Change")]
    }
    
    df <- df[order(-df$Scenario), , drop = FALSE]
    datatable(df, rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })
  
  output$dl_all <- downloadHandler(
    filename = function() sprintf("io_results_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")),
    content = function(file) {
      xhat <- results()
      df <- data.frame(code = names(xhat), output = round(xhat, 2))
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # ---- Comparison Tab ----
  output$comparison_plot <- renderPlotly({
    baseline <- rv$baseline_results
    scenario <- results()
    if (is.null(baseline)) return(NULL)
    
    df <- data.frame(
      code = names(scenario),
      baseline = baseline[names(scenario)],
      scenario = scenario,
      stringsAsFactors = FALSE
    )
    df <- df[order(-df$scenario), , drop = FALSE]
    df <- head(df, 20)
    
    plot_ly(df) %>%
      add_trace(x = ~code, y = ~baseline, type = "bar", name = "Baseline",
                marker = list(color = "#cccccc")) %>%
      add_trace(x = ~code, y = ~scenario, type = "bar", name = "Scenario",
                marker = list(color = BRAND_COLS[4])) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "Industry Code"),
        yaxis = list(title = "Gross Output ($M)"),
        margin = list(b = 100),
        paper_bgcolor = BRAND_BG,
        plot_bgcolor = BRAND_BG
      )
  })
  
  output$abs_change_plot <- renderPlotly({
    baseline <- rv$baseline_results
    scenario <- results()
    if (is.null(baseline)) return(NULL)
    
    change <- scenario - baseline[names(scenario)]
    df <- data.frame(code = names(change), change = change, stringsAsFactors = FALSE)
    df <- df[order(-abs(df$change)), , drop = FALSE]
    df <- head(df, 15)
    df <- df[order(df$change), , drop = FALSE]
    
    cols <- ifelse(df$change >= 0, BRAND_COLS[5], BRAND_COLS[4])
    
    plot_ly(
      data = df,
      x = ~change, y = ~code,
      type = "bar", orientation = "h",
      marker = list(color = cols),
      hovertemplate = "<b>%{y}</b><br>Change: $%{x:,.0f}M<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Absolute Change ($M)", zeroline = TRUE),
        yaxis = list(title = ""),
        margin = list(l = 100),
        paper_bgcolor = BRAND_BG,
        plot_bgcolor = BRAND_BG
      )
  })
  
  output$pct_change_plot <- renderPlotly({
    baseline <- rv$baseline_results
    scenario <- results()
    if (is.null(baseline)) return(NULL)
    
    pct_change <- ((scenario - baseline[names(scenario)]) / baseline[names(scenario)]) * 100
    pct_change[!is.finite(pct_change)] <- 0
    
    df <- data.frame(code = names(pct_change), pct = pct_change, stringsAsFactors = FALSE)
    df <- df[order(-abs(df$pct)), , drop = FALSE]
    df <- head(df, 15)
    df <- df[order(df$pct), , drop = FALSE]
    
    cols <- ifelse(df$pct >= 0, BRAND_COLS[5], BRAND_COLS[4])
    
    plot_ly(
      data = df,
      x = ~pct, y = ~code,
      type = "bar", orientation = "h",
      marker = list(color = cols),
      hovertemplate = "<b>%{y}</b><br>Change: %{x:.1f}%<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Percent Change (%)", zeroline = TRUE),
        yaxis = list(title = ""),
        margin = list(l = 100),
        paper_bgcolor = BRAND_BG,
        plot_bgcolor = BRAND_BG
      )
  })
  
  output$change_stats_table <- renderDT({
    baseline <- rv$baseline_results
    scenario <- results()
    if (is.null(baseline)) return(NULL)
    
    change <- scenario - baseline[names(scenario)]
    pct_change <- (change / baseline[names(scenario)]) * 100
    pct_change[!is.finite(pct_change)] <- 0
    
    df <- data.frame(
      Metric = c("Total Output (Baseline)", "Total Output (Scenario)", 
                 "Absolute Change", "Percent Change", 
                 "Industries Increased", "Industries Decreased"),
      Value = c(
        paste0("$", format(round(sum(baseline)/1000, 1), big.mark = ","), "B"),
        paste0("$", format(round(sum(scenario)/1000, 1), big.mark = ","), "B"),
        paste0("$", format(round(sum(change)/1000, 1), big.mark = ","), "B"),
        paste0(round((sum(change)/sum(baseline))*100, 2), "%"),
        sum(change > 0),
        sum(change < 0)
      ),
      stringsAsFactors = FALSE
    )
    
    datatable(df, rownames = FALSE, options = list(dom = 't'))
  })
  
  # ---- Sensitivity Analysis ----
  observeEvent(input$run_sensitivity, {
    output$sensitivity_plot <- renderPlotly({
      io <- io_data()
      buckets <- buckets_reactive()
      groups <- names(.endmarket_specs)
      endmarket <- input$sens_endmarket
      
      f_base <- rowSums(io$F)
      grp_totals_base <- tapply(f_base, INDEX = buckets, FUN = sum)
      base_val <- grp_totals_base[endmarket]
      
      range_pct <- seq(input$sens_range[1], input$sens_range[2], 
                       length.out = input$sens_steps)
      
      results_list <- list()
      
      for (pct in range_pct) {
        totals <- rep(0, length(groups))
        names(totals) <- groups
        totals[endmarket] <- base_val * (1 + pct/100)
        
        f_temp <- distribute_totals_to_f(buckets, groups, io$F, totals)
        xhat_temp <- as.numeric(io$L %*% f_temp)
        names(xhat_temp) <- io$codes
        
        present <- intersect(PLASTICS_CODES, names(xhat_temp))
        total_plastics <- sum(xhat_temp[present])
        
        results_list[[length(results_list) + 1]] <- data.frame(
          pct_change = pct,
          total_plastics = total_plastics,
          stringsAsFactors = FALSE
        )
      }
      
      df <- do.call(rbind, results_list)
      
      plot_ly(
        data = df,
        x = ~pct_change, y = ~total_plastics,
        type = "scatter", mode = "lines+markers",
        line = list(color = BRAND_COLS[4], width = 3),
        marker = list(size = 8, color = BRAND_COLS[1]),
        hovertemplate = "Change: %{x:.1f}%<br>Plastics: $%{y:,.0f}M<extra></extra>"
      ) %>%
        layout(
          title = paste("Sensitivity to", endmarket),
          xaxis = list(title = paste(endmarket, "Change (%)")),
          yaxis = list(title = "Total Plastics Demand ($M)"),
          paper_bgcolor = BRAND_BG,
          plot_bgcolor = BRAND_BG
        )
    })
    
    output$multi_sensitivity_plot <- renderPlotly({
      io <- io_data()
      buckets <- buckets_reactive()
      groups <- names(.endmarket_specs)
      endmarket <- input$sens_endmarket
      
      f_base <- rowSums(io$F)
      grp_totals_base <- tapply(f_base, INDEX = buckets, FUN = sum)
      base_val <- grp_totals_base[endmarket]
      
      range_pct <- seq(input$sens_range[1], input$sens_range[2], 
                       length.out = input$sens_steps)
      
      present <- intersect(PLASTICS_CODES, io$codes)
      
      df_list <- list()
      
      for (pct in range_pct) {
        totals <- rep(0, length(groups))
        names(totals) <- groups
        totals[endmarket] <- base_val * (1 + pct/100)
        
        f_temp <- distribute_totals_to_f(buckets, groups, io$F, totals)
        xhat_temp <- as.numeric(io$L %*% f_temp)
        names(xhat_temp) <- io$codes
        
        for (code in present) {
          df_list[[length(df_list) + 1]] <- data.frame(
            pct_change = pct,
            code = code,
            label = PLASTICS_LABELS[code],
            value = xhat_temp[code],
            stringsAsFactors = FALSE
          )
        }
      }
      
      df <- do.call(rbind, df_list)
      
      p <- plot_ly(data = df, x = ~pct_change, y = ~value, color = ~label,
                   type = "scatter", mode = "lines",
                   colors = BRAND_COLS) %>%
        layout(
          title = paste("All Plastics Subsectors -", endmarket, "Sensitivity"),
          xaxis = list(title = paste(endmarket, "Change (%)")),
          yaxis = list(title = "Output ($M)"),
          paper_bgcolor = BRAND_BG,
          plot_bgcolor = BRAND_BG
        )
      
      p
    })
  })
  
  # ---- Settings outputs ----
  output$invinfo <- renderText({
    io <- io_data()
    paste0("Leontief inverse: ", io$inv_method,
           "\nIndustries: ", length(io$codes),
           "\nFD columns: ", paste(io$fd_cols, collapse = ", "))
  })
  
  output$mapPreview <- renderDT({
    b <- buckets_reactive()
    df <- data.frame(
      code = names(b),
      end_market = as.character(b),
      stringsAsFactors = FALSE
    )
    datatable(df, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })
}

shinyApp(ui, server)
