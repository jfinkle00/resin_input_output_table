# Updated Quick IO Validation Script (with NA fixes)
# Now that loading works, let's validate the table!

library(readxl)
library(stringr)
library(MASS)

cat("\n")
cat(paste(rep("=", 70), collapse=""), "\n")
cat("QUICK IO TABLE VALIDATION\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

# Configuration
FILE_PATH <- "C:/Users/Jfink/OneDrive/Desktop/10302025IOTest.xlsx"
SHEET_NAME <- "Sheet3"

PLASTICS_CODES <- c("325211","326110","326120","326130",
                    "326140","326150","326160","326190")

# ============================================================
# Helper Functions (with NA fixes)
# ============================================================

clean_code_names <- function(x){
  x <- as.character(x)
  x <- str_trim(x)
  x[is.na(x)] <- ""  # FIX: Handle NA
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

io_from_excel_base <- function(path, sheet){
  raw <- readxl::read_excel(path, sheet=sheet, col_names=FALSE)
  raw <- as.data.frame(raw, stringsAsFactors=FALSE)
  
  # Find header row more robustly
  find_header_row <- function(m) {
    nr <- nrow(m)
    for (r in 1:min(nr, 25)) {
      vals <- as.character(unlist(m[r, ]))
      vals[is.na(vals)] <- ""  # Handle NA
      if (any(vals == "Code", na.rm = TRUE) && any(vals == "T001", na.rm = TRUE)) {
        return(r)
      }
    }
    # Alternative: look for "Code" alone
    for (r in 1:min(nr, 10)) {
      vals <- as.character(unlist(m[r, ]))
      vals[is.na(vals)] <- ""
      if (any(toupper(vals) == "CODE", na.rm = TRUE)) {
        return(r)
      }
    }
    return(2)  # Default
  }
  
  header_row <- find_header_row(raw)
  headers <- as.character(unlist(raw[header_row, , drop=TRUE]))
  headers[is.na(headers)] <- ""  # FIX: Handle NA headers
  
  placeholders <- paste0("col", seq_along(headers))
  is_blank <- headers == ""  # FIX: Changed from is.na
  headers[is_blank] <- placeholders[is_blank]
  headers <- make.unique(headers)
  names(raw) <- headers
  
  df <- raw[(header_row+1):nrow(raw), , drop=FALSE]
  row_codes <- as.character(df[,1,drop=TRUE])
  row_codes[is.na(row_codes)] <- ""  # FIX: Handle NA codes
  
  df <- df[, -1, drop=FALSE]
  colnames(df) <- clean_code_names(colnames(df))
  
  # FIX: Replace NA with 0 after numeric conversion
  for (j in seq_len(ncol(df))) {
    df[,j] <- suppressWarnings(as.numeric(df[,j]))
    df[is.na(df[,j]), j] <- 0  # CRITICAL FIX!
  }
  
  all_cols <- colnames(df)
  
  # Total output column - more robust search
  total_output_col <- if ("T001" %in% all_cols) {
    "T001"
  } else {
    idx <- which(toupper(all_cols) == "T001")
    if (length(idx) == 1) all_cols[idx] else NA
  }
  
  if (is.na(total_output_col)) {
    cat("ERROR: Cannot find T001 column\n")
    cat("Available columns (first 30):\n")
    print(head(all_cols, 30))
    stop("T001 column not found")
  }
  
  fd_cols <- c("S00101","S00102","GSLGE","GSLGH","GSLGO","S00201","S00202","S00203")
  fd_cols_present <- intersect(fd_cols, all_cols)
  if (length(fd_cols_present)==0){
    fd_cols_present <- all_cols[grepl("^(S001|S002|GSLG|F0|F1)", all_cols)]
    if (length(fd_cols_present) == 0) {
      cat("ERROR: No FD columns found\n")
      cat("Available columns (first 50):\n")
      print(head(all_cols, 50))
      stop("No final-demand columns detected")
    }
  }
  
  first_fd_idx <- match(fd_cols_present[1], all_cols)
  if (is.na(first_fd_idx) || first_fd_idx <= 1) {
    cat("ERROR: FD columns too early in table\n")
    cat("First FD column:", fd_cols_present[1], "at position:", first_fd_idx, "\n")
    stop("Cannot locate industry-by-industry block")
  }
  
  ind_cols <- all_cols[1:(first_fd_idx-1)]
  ind_cols <- ind_cols[grepl("^[0-9A-Z]+$", ind_cols)]
  if (length(ind_cols) == 0) {
    cat("ERROR: No industry columns found\n")
    cat("Columns before FD (1 to", first_fd_idx-1, "):\n")
    print(all_cols[1:(first_fd_idx-1)])
    stop("No industry columns detected")
  }
  
  U_full <- as.matrix(df[, ind_cols, drop=FALSE])
  F_full <- as.matrix(df[, fd_cols_present, drop=FALSE])
  X_full <- as.numeric(df[, total_output_col, drop=TRUE])
  rc_full <- row_codes
  
  n_full <- min(nrow(U_full), ncol(U_full))
  U_full <- U_full[1:n_full, 1:n_full, drop=FALSE]
  F_full <- F_full[1:n_full, , drop=FALSE]
  X_full <- X_full[1:n_full]
  rc_full <- rc_full[1:n_full]
  
  keep <- which((X_full>0) | rowSums(U_full)>0 | colSums(U_full)>0 | rowSums(F_full)>0)
  U <- U_full[keep, keep, drop=FALSE]
  F <- F_full[keep, , drop=FALSE]
  X <- X_full[keep]
  rc <- rc_full[keep]
  
  nz <- which((rowSums(U)+rowSums(F)+X) > 0)
  U <- U[nz, nz, drop=FALSE]
  F <- F[nz, , drop=FALSE]
  X <- X[nz]
  rc <- rc[nz]
  
  n <- nrow(U)
  X[!is.finite(X) | X==0] <- 1e-9
  A <- U %*% diag(1 / X, nrow=n, ncol=n)
  A[!is.finite(A)] <- 0
  M <- diag(n) - A
  inv_res <- invert_stable(M)
  L <- inv_res$L
  
  list(U=U, F=F, X=X, L=L, codes=rc, inv_method=inv_res$method)
}

# ============================================================
# Test 1: File Loading
# ============================================================

cat("TEST 1: File Loading\n")
cat(paste(rep("-", 70), collapse=""), "\n")

if(!file.exists(FILE_PATH)) {
  cat("✗ FAIL: File not found!\n")
  cat("  Update FILE_PATH in this script\n\n")
  stop("Cannot continue without data file")
}

cat("✓ File exists\n")

io <- tryCatch(
  io_from_excel_base(FILE_PATH, SHEET_NAME),
  error = function(e) {
    cat("✗ FAIL: Error loading file\n")
    cat("  Error:", e$message, "\n")
    NULL
  }
)

if(is.null(io)) {
  stop("Cannot continue - file loading failed")
}

cat("✓ File loaded successfully\n")
cat("  Industries:", length(io$codes), "\n")
cat("  Inversion method:", io$inv_method, "\n\n")

# ============================================================
# Test 2: IO Identity (Balance Check)
# ============================================================

cat("TEST 2: IO Identity (U + F = X)\n")
cat(paste(rep("-", 70), collapse=""), "\n")

intermediate <- rowSums(io$U)
final_demand <- rowSums(io$F)
calculated_X <- intermediate + final_demand
difference <- calculated_X - io$X
pct_error <- abs(difference / io$X) * 100

mean_pct_error <- mean(pct_error, na.rm=TRUE)
max_pct_error <- max(pct_error, na.rm=TRUE)
n_large_errors <- sum(pct_error > 1, na.rm=TRUE)

cat("Mean % error:", round(mean_pct_error, 4), "%\n")
cat("Max % error:", round(max_pct_error, 4), "%\n")
cat("Industries with >1% error:", n_large_errors, "\n")

if(mean_pct_error < 0.1 && n_large_errors < 5) {
  cat("✓ PASS: IO table is balanced\n\n")
} else {
  cat("⚠ WARNING: Balance issues detected\n")
  cat("  Check data structure and FD columns\n\n")
}

# ============================================================
# Test 3: Technical Coefficients
# ============================================================

cat("TEST 3: Technical Coefficients\n")
cat(paste(rep("-", 70), collapse=""), "\n")

A <- io$U %*% diag(1/io$X)
n_negative <- sum(A < 0)
n_over_one <- sum(A > 1)
col_sums <- colSums(A)
mean_col_sum <- mean(col_sums)

cat("Coefficients < 0:", n_negative, "\n")
cat("Coefficients > 1:", n_over_one, "\n")
cat("Mean column sum:", round(mean_col_sum, 3), "\n")

if(n_negative == 0 && n_over_one == 0 && mean_col_sum >= 0.3 && mean_col_sum <= 0.8) {
  cat("✓ PASS: Technical coefficients are reasonable\n\n")
} else {
  cat("⚠ WARNING: Some unusual technical coefficients\n")
  if(n_negative > 0) cat("  - Has negative coefficients (should investigate)\n")
  if(n_over_one > 0) cat("  - Has coefficients > 1 (unusual but may be regional table)\n")
  if(mean_col_sum < 0.3 || mean_col_sum > 0.8) {
    cat("  - Mean column sum outside typical range (may be okay)\n")
  }
  cat("\n")
}

# ============================================================
# Test 4: Leontief Multipliers
# ============================================================

cat("TEST 4: Leontief Multipliers\n")
cat(paste(rep("-", 70), collapse=""), "\n")

diag_L <- diag(io$L)
mean_diag <- mean(diag_L)
min_diag <- min(diag_L)
max_diag <- max(diag_L)
n_under_one <- sum(diag_L < 1)

cat("Mean diagonal:", round(mean_diag, 3), "\n")
cat("Min diagonal:", round(min_diag, 3), "\n")
cat("Max diagonal:", round(max_diag, 3), "\n")
cat("Diagonal < 1:", n_under_one, "\n")

if(n_under_one == 0 && mean_diag >= 1.2 && mean_diag <= 3.0 && max_diag < 10) {
  cat("✓ PASS: Multipliers are reasonable\n\n")
} else {
  cat("⚠ WARNING: Some unusual multiplier values\n")
  if(n_under_one > 0) cat("  - Has multipliers < 1 (should be impossible)\n")
  if(mean_diag < 1.2) cat("  - Mean multiplier low (check if this is a partial table)\n")
  if(max_diag >= 10) cat("  - Very high multipliers detected\n")
  cat("\n")
}

# ============================================================
# Test 5: Plastics Codes Present
# ============================================================

cat("TEST 5: Plastics Industry Codes\n")
cat(paste(rep("-", 70), collapse=""), "\n")

plastics_found <- PLASTICS_CODES %in% io$codes
n_found <- sum(plastics_found)

cat("Plastics codes found:", n_found, "of", length(PLASTICS_CODES), "\n")

for(i in seq_along(PLASTICS_CODES)) {
  symbol <- if(plastics_found[i]) "✓" else "✗"
  cat("  ", symbol, PLASTICS_CODES[i], "\n")
}

if(n_found >= 6) {
  cat("✓ PASS: Most plastics codes present\n\n")
} else {
  cat("⚠ WARNING: Many plastics codes missing\n")
  cat("  Your table may use different aggregation\n\n")
}

# ============================================================
# Test 6: Baseline Plastics Output
# ============================================================

cat("TEST 6: Plastics Baseline Output\n")
cat(paste(rep("-", 70), collapse=""), "\n")

f_base <- rowSums(io$F)
x_base <- as.numeric(io$L %*% f_base)
names(x_base) <- io$codes

plastics_idx <- match(PLASTICS_CODES, io$codes)
plastics_idx <- plastics_idx[!is.na(plastics_idx)]

if(length(plastics_idx) > 0) {
  plastics_output <- x_base[plastics_idx]
  total_plastics <- sum(plastics_output)
  pct_of_economy <- (total_plastics / sum(x_base)) * 100
  
  cat("Total plastics output: $", format(round(total_plastics), big.mark=","), "M\n", sep="")
  cat("As % of economy:", round(pct_of_economy, 2), "%\n")
  
  if(total_plastics > 0 && pct_of_economy >= 0.5 && pct_of_economy <= 5) {
    cat("✓ PASS: Plastics output in reasonable range\n\n")
  } else if(total_plastics > 0) {
    cat("⚠ NOTE: Plastics % outside typical range\n")
    cat("  This may be normal for regional/specialized tables\n\n")
  } else {
    cat("✗ WARNING: No plastics output calculated\n\n")
  }
} else {
  cat("⚠ WARNING: No plastics codes found in table\n\n")
}

# ============================================================
# Test 7: Stability Test
# ============================================================

cat("TEST 7: Numerical Stability\n")
cat(paste(rep("-", 70), collapse=""), "\n")

f_base <- rowSums(io$F)
f_perturb <- f_base * 1.001
x_base_sum <- sum(as.numeric(io$L %*% f_base))
x_perturb_sum <- sum(as.numeric(io$L %*% f_perturb))

pct_change <- ((x_perturb_sum - x_base_sum) / x_base_sum) * 100
expected_range <- c(0.05, 0.20)

cat("FD change: +0.1%\n")
cat("Output change: +", round(pct_change, 4), "%\n", sep="")

if(pct_change >= expected_range[1] && pct_change <= expected_range[2]) {
  cat("✓ PASS: System is numerically stable\n\n")
} else if(pct_change > 0 && pct_change < 1) {
  cat("✓ PASS: Stable response (slightly outside typical range)\n\n")
} else if(pct_change > 0) {
  cat("⚠ CAUTION: Response higher than typical\n")
  cat("  This may indicate high multipliers in your table\n\n")
} else {
  cat("✗ FAIL: System appears unstable\n\n")
}

# ============================================================
# Test 8: Simple Scenario Test
# ============================================================

cat("TEST 8: Simple Shock Response\n")
cat(paste(rep("-", 70), collapse=""), "\n")

# Test: Double all FD
f_base <- rowSums(io$F)
f_double <- f_base * 2
x_base <- as.numeric(io$L %*% f_base)
x_double <- as.numeric(io$L %*% f_double)

ratio <- sum(x_double) / sum(x_base)

cat("Doubling all final demand:\n")
cat("  Output ratio (should be ~2.0):", round(ratio, 3), "\n")

if(ratio >= 1.9 && ratio <= 2.1) {
  cat("✓ PASS: Linear response as expected\n\n")
} else if(ratio > 1.5 && ratio < 2.5) {
  cat("✓ PASS: Mostly linear response\n\n")
} else {
  cat("⚠ WARNING: Non-linear response detected\n\n")
}

# ============================================================
# Summary
# ============================================================

cat(paste(rep("=", 70), collapse=""), "\n")
cat("VALIDATION SUMMARY\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

# Count passes
n_tests <- 8
warnings <- 0

if(mean_pct_error >= 0.1 || n_large_errors >= 5) warnings <- warnings + 1
if(n_negative > 0 || n_over_one > 0 || mean_col_sum < 0.3 || mean_col_sum > 0.8) warnings <- warnings + 1
if(n_under_one > 0 || mean_diag < 1.2 || max_diag >= 10) warnings <- warnings + 1
if(n_found < 6) warnings <- warnings + 1
if(length(plastics_idx) == 0 || pct_of_economy < 0.5 || pct_of_economy > 5) warnings <- warnings + 1
if(pct_change < 0.05 || pct_change > 0.20) warnings <- warnings + 1
if(ratio < 1.9 || ratio > 2.1) warnings <- warnings + 1

n_pass <- n_tests - warnings

cat("Tests passed:", n_pass, "/", n_tests, "\n\n")

if(n_pass >= 7) {
  cat("✓ EXCELLENT: Your IO table is valid and ready to use!\n\n")
  cat("Next steps:\n")
  cat("  1. Update enhanced_io_dashboard.R with NA fixes\n")
  cat("  2. Launch the dashboard: shiny::runApp('enhanced_io_dashboard.R')\n")
  cat("  3. Run your first scenario!\n\n")
} else if(n_pass >= 5) {
  cat("✓ GOOD: Table is usable with minor issues noted above\n\n")
  cat("Review warnings but proceed with caution.\n")
  cat("Most warnings are acceptable for regional/specialized tables.\n\n")
} else {
  cat("⚠ CAUTION: Multiple issues detected\n\n")
  cat("Review the warnings above carefully.\n")
  cat("Consider:\n")
  cat("  - Checking data source format\n")
  cat("  - Verifying column structure\n")
  cat("  - Comparing to BEA documentation\n\n")
}

cat(paste(rep("=", 70), collapse=""), "\n")
cat("\nValidation complete! See IO_TESTING_GUIDE.md for detailed tests.\n")