#' @title Psychometric Integrity Architecture (PIA) - Latency & Drift-Diffusion Module
#' @description Decomposes response-time (RT) logs into parametric Ex-Gaussian 
#' components (mu, sigma, tau) and formats data matrices for cross-compilation 
#' with Hierarchical Drift-Diffusion Models (HDDM) to isolate response faking.
#' @author Sarbah Fred Junior
#' @concept Computational Psychometrics
#' @importFrom stats nlminb pnorm na.omit var quantile qnorm
#' @export

# --- Package Namespace Verification ---
if (!requireNamespace("stats", quietly = TRUE)) {
  stop("Fatal: Computational dependency package 'stats' is unavailable.")
}

#' Purify Latency Streams Using Adaptive Quantile Truncation
#'
#' @description Strips missing values and applies conservative outlier boundaries 
#' to isolate true cognitive processing times from automated bot injections.
#'
#' @param rt_vector A numeric vector of raw response times (in seconds).
#' @param min_bound Minimum allowable physiological response threshold. Default is 0.15s.
#' @param extreme_quantile The upper cutoff percentile for filtering outliers. Default is 0.995.
#' @return A purified vector of human cognitive response latencies.
#' @export
purify_latency_stream <- function(rt_vector, min_bound = 0.15, extreme_quantile = 0.995) {
  if (is.null(rt_vector) || !is.numeric(rt_vector)) {
    stop("Type Error: Latency stream must be a non-null numeric vector.")
  }
  
  # Strip missing data and enforce physiological floor bounds
  clean_rt <- stats::na.omit(rt_vector)
  clean_rt <- clean_rt[clean_rt >= min_bound]
  
  if (length(clean_rt) < 8) {
    stop("Data Deprivation Error: Insufficient observations remaining after initial filtering.")
  }
  
  # Apply conservative quantile cuts to preserve valid cognitive tails
  upper_cutoff <- stats::quantile(clean_rt, probs = extreme_quantile)
  purified_rt <- clean_rt[clean_rt <= upper_cutoff]
  
  return(as.numeric(purified_rt))
}

#' Optimize Ex-Gaussian Parameters via Stabilized Log-Likelihood Estimation
#'
#' @description Extracts mu, sigma, and tau parameters from response latencies, 
#' utilizing mathematical safeguards to maintain stability even with low-skew arrays.
#'
#' @param rt_vector A numeric vector of raw response times.
#' @return A named numeric vector containing optimized estimates for mu, sigma, and tau.
#' @references Ratcliff, R. (1993). Methods for dealing with reaction time outliers. Psychological Bulletin, 114(3), 510-532.
#' @export
estimate_exg_parameters <- function(rt_vector) {
  rt <- purify_latency_stream(rt_vector)
  n  <- length(rt)
  
  # Compute Method of Moments initial estimates
  sample_mean <- mean(rt)
  sample_var  <- stats::var(rt)
  
  # Calculate skewness safely to shield against symmetry optimization drops
  m3 <- sum((rt - sample_mean)^3) / n
  sample_skew <- m3 / (sample_var^(1.5))
  
  # Stabilize initial coordinates if the data lacks sufficient positive skew
  if (is.na(sample_skew) || sample_skew <= 0.05) {
    init_tau   <- sqrt(sample_var) * 0.2
    init_mu    <- sample_mean - init_tau
    init_sigma <- sqrt(sample_var) * 0.8
  } else {
    init_tau   <- (m3 / 2)^(1/3)
    if (init_tau^2 >= sample_var) {
      init_tau <- sqrt(sample_var) * 0.5
    }
    init_mu    <- sample_mean - init_tau
    init_sigma <- sqrt(sample_var - (init_tau^2))
  }
  
  # Bounded Objective Function Execution
  exg_log_likelihood <- function(pars) {
    mu    <- pars[1]
    sigma <- pars[2]
    tau   <- pars[3]
    
    if (sigma <= 0.001 || tau <= 0.001) return(1e12)
    
    # Standardize scale transformation vectors
    z <- (rt - mu) / sigma
    
    # Stabilized density translation mapping matrix
    term1 <- 1 / tau
    term2 <- ((mu - rt) / tau) + ((sigma^2) / (2 * (tau^2)))
    
    # Protect against exponent overflow errors
    term2 <- ifelse(term2 > 700, 700, term2)
    
    term3 <- stats::pnorm(z - (sigma / tau))
    density <- term1 * exp(term2) * term3
    
    density[density < 1e-12] <- 1e-12
    return(-sum(log(density)))
  }
  
  # Execute bounded minimization
  optimization_matrix <- stats::nlminb(
    start = c(init_mu, init_sigma, init_tau),
    objective = exg_log_likelihood,
    lower = c(0.001, 0.001, 0.001),
    upper = c(max(rt), max(rt), max(rt))
  )
  
  results <- optimization_matrix$par
  names(results) <- c("mu", "sigma", "tau")
  return(results)
}

#' Map Latency and Accuracy Arrays to Hierarchical Drift-Diffusion Formats
#'
#' @description Structurally compiles trial-by-trial data frameworks to interface 
#' cleanly with Python-based hierarchical multi-subject analysis environments.
#'
#' @param rt_vector A numeric vector of purified response latencies.
#' @param accuracy_vector A binary vector (0 or 1) tracking trial performance.
#' @param subject_id An optional character or numeric vector mapping participant identities.
#' @return A structured data.frame matching standard HDDM design patterns.
#' @export
map_hddm_cross_paradigm <- function(rt_vector, accuracy_vector, subject_id = NULL) {
  stopifnot(length(rt_vector) == length(accuracy_vector))
  
  if (is.null(subject_id)) {
    subject_id <- rep(1, length(rt_vector))
  } else {
    stopifnot(length(subject_id) == length(rt_vector))
  }
  
  valid_indices <- which(!is.na(rt_vector) & !is.na(accuracy_vector) & !is.na(subject_id))
  if (length(valid_indices) < 5) {
    stop("Execution Fault: Aligned data elements are insufficient for HDDM mapping workflows.")
  }
  
  # Construct a clean export matrix tailored for Python pandas parsing
  hddm_frame <- data.frame(
    subj_idx = subject_id[valid_indices],
    rt       = rt_vector[valid_indices],
    response = accuracy_vector[valid_indices],
    condition = ifelse(accuracy_vector[valid_indices] == 1, "Correct_Velocity", "Error_Velocity")
  )
  
  return(hddm_frame)
}
