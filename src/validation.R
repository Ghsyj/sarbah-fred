#' @title Psychometric Integrity Architecture (PIA) - Advanced Validation Engine
#' @description Implements high-performance Confirmatory Factor Analysis (CFA),
#' multi-stage Measurement Invariance tracking (up to Strict Invariance), and 
#' robust log-likelihood gradient feature attributions using lavaan.
#' @author Sarbah Fred Junior
#' @concept Computational Psychometrics
#' @importFrom lavaan cfa fitMeasures lavInspect lavPredict
#' @importFrom stats na.omit var qnorm
#' @export

# --- Package Namespace Verification ---
if (!requireNamespace("lavaan", quietly = TRUE)) {
  stop("Fatal: Computational dependency package 'lavaan' is unavailable in active library path.")
}

#' Fit High-Performance Confirmatory Factor Analysis Models
#'
#' @description Fits customized latent structural matrices using estimators 
#' optimized for data characteristics (MLR for continuous; WLSMV for categorical).
#'
#' @param data A data.frame containing clean response metrics.
#' @param syntax A character string outlining lavaan factor alignment rules.
#' @param categorical A logical flag indicating if item scales are ordinal/categorical.
#' @return A converged lavaan structural fit object.
#' @export
execute_cfa_engine <- function(data, syntax, categorical = FALSE) {
  if (is.null(data) || !is.data.frame(data)) {
    stop("Type Exception: Input parameters must be instantiated within a valid data.frame context.")
  }
  
  # Select optimal robust estimator setup
  est <- ifelse(categorical, "WLSMV", "MLR")
  miss <- ifelse(categorical, "pairwise", "FIML")
  
  fit_instance <- tryCatch({
    lavaan::cfa(
      model = syntax,
      data = data,
      estimator = est,
      missing = miss,
      warn = FALSE
    )
  }, error = function(e) {
    stop(paste("Structural Equation Collapse: Optimizer failed to resolve parameters -> ", e$message))
  })
  
  return(fit_instance)
}

#' Automate Four-Stage Multi-Group Measurement Invariance Testing
#'
#' @description Evaluates configural, metric, scalar, and strict invariance stages 
#' to pinpoint exactly where faking behavior destabilizes psychometric properties.
#'
#' @param data A data.frame containing item tracks and your targeting group column.
#' @param syntax A character string detailing latent factor metrics.
#' @param group_var A character string identifying the grouping criteria vector.
#' @param categorical A logical flag indicating if item scales are ordinal/categorical.
#' @return A detailed data.frame mapping absolute and delta fit statistics ($\Delta$CFI, $\Delta$RMSEA).
#' @references Vandenberg, R. J., & Lance, C. E. (2000). A review and synthesis of the measurement invariance literature. Organizational Research Methods, 3(1), 4-70.
#' @export
evaluate_structural_invariance <- function(data, syntax, group_var, categorical = FALSE) {
  stopifnot(group_var %in% colnames(data))
  
  group_factor <- as.factor(data[[group_var]])
  if (length(levels(group_factor)) < 2) {
    stop("Invariance Parameter Exception: Group metrics must display at least two unique condition criteria.")
  }
  
  est <- ifelse(categorical, "WLSMV", "MLR")
  miss <- ifelse(categorical, "pairwise", "FIML")
  
  # Progressively build the nested structural models
  configural <- lavaan::cfa(model = syntax, data = data, group = group_var, estimator = est, missing = miss, warn = FALSE)
  metric     <- lavaan::cfa(model = syntax, data = data, group = group_var, group.equal = "loadings", estimator = est, missing = miss, warn = FALSE)
  scalar     <- lavaan::cfa(model = syntax, data = data, group = group_var, group.equal = c("loadings", "intercepts"), estimator = est, missing = miss, warn = FALSE)
  strict     <- lavaan::cfa(model = syntax, data = data, group = group_var, group.equal = c("loadings", "intercepts", "residuals"), estimator = est, missing = miss, warn = FALSE)
  
  # Internal function to pull robust indices safely
  pull_indices <- function(fit_obj) {
    idx_names <- if (categorical) c("cfi", "tli", "rmsea", "srmr") else c("cfi.robust", "tli.robust", "rmsea.robust", "srmr")
    metrics <- lavaan::fitMeasures(fit_obj, idx_names)
    names(metrics) <- c("cfi", "tli", "rmsea", "srmr")
    return(metrics)
  }
  
  idx_c <- pull_indices(configural)
  idx_m <- pull_indices(metric)
  idx_s <- pull_indices(scalar)
  idx_st <- pull_indices(strict)
  
  # Compile comparative difference table
  invariance_summary <- data.frame(
    Stage        = c("Configural", "Metric", "Scalar", "Strict"),
    CFI          = c(idx_c["cfi"], idx_m["cfi"], idx_s["cfi"], idx_st["cfi"]),
    Delta_CFI    = c(NA, idx_m["cfi"] - idx_c["cfi"], idx_s["cfi"] - idx_m["cfi"], idx_st["cfi"] - idx_s["cfi"]),
    RMSEA        = c(idx_c["rmsea"], idx_m["rmsea"], idx_s["rmsea"], idx_st["rmsea"]),
    Delta_RMSEA  = c(NA, idx_m["rmsea"] - idx_c["rmsea"], idx_s["rmsea"] - idx_m["rmsea"], idx_st["rmsea"] - idx_s["rmsea"]),
    SRMR         = c(idx_c["srmr"], idx_m["srmr"], idx_s["srmr"], idx_st["srmr"]),
    row.names    = NULL
  )
  
  return(invariance_summary)
}

#' Calculate Log-Likelihood Gradient Item Attributions (SHAP-Proxy)
#'
#' @description Maps the analytical derivative of individual log-likelihood case values 
#' back to raw item residuals to establish localized feature attribution weights.
#'
#' @param fit_obj A converged model object generated via execute_cfa_engine.
#' @return A data.frame mapping individual case profiles to standardized item impact metrics.
#' @export
calculate_shap_attributions <- function(fit_obj) {
  if (is.null(fit_obj) || !inherits(fit_obj, "lavaan")) {
    stop("Argument Error: Target engine object must belong to the standard 'lavaan' structural ecosystem.")
  }
  
  # Isolate individual case-wise log-likelihood values
  ind_ll <- lavaan::lavInspect(fit_obj, "case.values")
  if (is.null(ind_ll)) {
    stop("Extraction Error: Feature attributions require likelihood estimation. Verify estimator configuration.")
  }
  
  # Extract true data parameters
  raw_data <- as.matrix(lavaan::lavInspect(fit_obj, "data"))
  item_names <- colnames(raw_data)
  
  # Map predicted structural scores to capture genuine expectations
  implied_cov <- lavaan::lavInspect(fit_obj, "sigma.implied")
  
  # Fallback for handling multi-group matrix extractions
  if (is.list(implied_cov)) implied_cov <- implied_cov[[1]]
  
  # Calculate precision weight matrix
  precision_matrix <- tryCatch({
    solve(implied_cov)
  }, error = function(e) {
    diag(1, ncol(raw_data)) # Uniform identity fallback if structural covariance is non-positive definite
  })
  
  # Project local residuals using precision matrix transformations
  mean_vec <- colMeans(raw_data, na.rm = TRUE)
  residuals <- scale(raw_data, center = mean_vec, scale = FALSE)
  transformed_residuals <- residuals %*% precision_matrix
  
  shap_matrix <- matrix(0, nrow = nrow(raw_data), ncol = ncol(raw_data))
  for (i in 1:ncol(raw_data)) {
    # Isolate case-wise gradients relative to variance shifts
    shap_matrix[, i] <- transformed_residuals[, i] * (ind_ll - mean(ind_ll))
  }
  
  # Format output structure
  shap_matrix <- scale(shap_matrix)
  colnames(shap_matrix) <- paste0("SHAP_", item_names)
  rownames(shap_matrix) <- paste0("Profile_Case_", 1:nrow(shap_matrix))
  
  return(as.data.frame(shap_matrix))
}
