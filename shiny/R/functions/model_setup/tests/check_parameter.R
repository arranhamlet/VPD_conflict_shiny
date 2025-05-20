#' Validate Parameter Inputs
#'
#' Validates one or more numeric parameters against specified rules such as being an integer,
#' a probability, non-negative, above zero, or not exceeding a reference value. Supports both
#' per-element and per-object validation for lists of named vectors.
#'
#' @param parameter A numeric vector or a named list of numeric vectors. Each element will be
#'   checked against the specified validation rules.
#' @param check_integer Logical. If TRUE, checks that values are integers.
#' @param check_probability Logical. If TRUE, checks that values are probabilities (between 0 and 1).
#' @param check_above_zero Logical. If TRUE, checks that values are strictly greater than 0.
#' @param check_non_negative Logical. If TRUE, checks that values are greater than or equal to 0.
#' @param check_not_above_reference_value Numeric. If specified, checks that values do not exceed this reference value.
#' @param check_if_sum_above_zero Logical. If TRUE, checks that the sum of the vector is greater than 0.
#' @param summary Logical. If TRUE and `parameter` is a list, returns one row per list element summarizing any validation failures.
#'   If FALSE (default), returns one row per failing value.
#'
#' @return A `data.table` object with columns:
#' \describe{
#'   \item{Parameter}{The name of the failing parameter (list name or index).}
#'   \item{Value}{The actual value (only for `summary = FALSE`).}
#'   \item{Reason}{The reason why the check failed.}
#' }
#' In `summary = TRUE` mode, only `Parameter` and `Reason` are returned.
#'
#' @examples
#' params <- list(
#'   good = c(0.1, 0.5),
#'   bad_prob = c(-0.1, 1.2),
#'   bad_int = c(1.1, 2),
#'   negative = c(-1, -2)
#' )
#'
#' check_parameter(params, check_probability = TRUE, check_integer = TRUE, summary = TRUE)
#' check_parameter(params, check_probability = TRUE, check_integer = TRUE)
#'
#' @export

check_parameter <- function(parameter, 
                            check_integer = FALSE, 
                            check_probability = FALSE, 
                            check_above_zero = FALSE,
                            check_non_negative = FALSE,
                            check_not_above_reference_value = NULL,
                            check_if_sum_above_zero = FALSE,
                            summary = TRUE) {
  
  parameter_name <- deparse(substitute(parameter))
  
  is_param_list <- is.list(parameter)
  if (is_param_list) {
    parameter_flat <- unlist(parameter, recursive = FALSE)
    param_names <- rep(names(parameter_flat), times = lengths(parameter_flat))
    parameter_vector <- unlist(parameter)
  } else {
    param_names <- as.character(seq_along(parameter))
    parameter_vector <- parameter
  }
  
  # Early exit if not numeric
  if (!is.numeric(parameter_vector)) {
    warning(sprintf("The parameter '%s' must be numeric.", parameter_name))
    return(data.table::data.table(Parameter = parameter_name, Value = NA_real_, Reason = "Not numeric"))
  }
  
  # --- Summary mode (list input only) ---
  if (summary && is_param_list) {
    fail_dt <- data.table::data.table(Parameter = character(), Reason = character())
    
    for (param_name in names(parameter)) {
      value <- parameter[[param_name]]
      reasons <- character()
      
      if (!is.numeric(value)) {
        reasons <- c(reasons, "Not numeric")
      } else {
        if (check_integer && any(value != floor(value))) {
          reasons <- c(reasons, "Must be all integers")
        }
        if (check_above_zero && any(value <= 0)) {
          reasons <- c(reasons, "Must be all above 0")
        }
        if (check_non_negative && any(value < 0)) {
          reasons <- c(reasons, "Must be all non-negative")
        }
        if (check_probability && any(value < 0 | value > 1)) {
          reasons <- c(reasons, "Must be all probabilities (between 0 and 1)")
        }
        if (!is.null(check_not_above_reference_value) && any(value > check_not_above_reference_value)) {
          reasons <- c(reasons, paste("Must not exceed", check_not_above_reference_value))
        }
        if (check_if_sum_above_zero && sum(value) <= 0) {
          reasons <- c(reasons, "Sum must be above zero")
        }
      }
      
      if (length(reasons) > 0) {
        fail_dt <- data.table::rbindlist(list(
          fail_dt,
          data.table::data.table(
            Parameter = param_name,
            Reason = paste(reasons, collapse = " and ")
          )
        ))
      }
    }
    
    return(fail_dt)
  }
  
  # --- Per-element (default mode) ---
  n <- length(parameter_vector)
  max_fails <- 5 * n + 1
  failure_param <- character(max_fails)
  failure_val <- numeric(max_fails)
  failure_reason <- character(max_fails)
  k <- 1
  
  if (check_if_sum_above_zero && sum(parameter_vector) <= 0) {
    failure_param[k] <- "Sum of values"
    failure_val[k] <- sum(parameter_vector)
    failure_reason[k] <- "Sum must be above zero"
    k <- k + 1
  }
  
  add_failure <- function(idx, msg) {
    len <- length(idx)
    if (len > 0) {
      failure_param[k:(k + len - 1)] <<- param_names[idx]
      failure_val[k:(k + len - 1)] <<- parameter_vector[idx]
      failure_reason[k:(k + len - 1)] <<- msg
      k <<- k + len
    }
  }
  
  if (check_integer) {
    add_failure(which(parameter_vector != floor(parameter_vector)), "Must be an integer")
  }
  if (check_above_zero) {
    add_failure(which(parameter_vector <= 0), "Must be above 0")
  }
  if (check_non_negative) {
    add_failure(which(parameter_vector < 0), "Must be non-negative")
  }
  if (check_probability) {
    add_failure(which(parameter_vector < 0 | parameter_vector > 1), "Must be a probability (between 0 and 1)")
  }
  if (!is.null(check_not_above_reference_value)) {
    add_failure(which(parameter_vector > check_not_above_reference_value), 
                paste("Must not be above", check_not_above_reference_value))
  }
  
  if (k == 1) {
    return(data.table::data.table(Parameter = character(), Value = numeric(), Reason = character()))
  }
  
  return(data.table::data.table(
    Parameter = failure_param[1:(k - 1)],
    Value = failure_val[1:(k - 1)],
    Reason = failure_reason[1:(k - 1)]
  ))
}
