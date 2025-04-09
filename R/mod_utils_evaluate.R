# R/utils_evaluate.R

#' Calculate Standard Regression Metrics
#'
#' Calculates MAE, RMSE, and MAPE using the yardstick package.
#'
#' @param y_true Numeric vector of actual/true values.
#' @param y_pred Numeric vector of predicted values. Must be same length as y_true.
#'
#' @return A tibble with columns '.metric', '.estimator', '.estimate'.
#'   Returns NULL if inputs are invalid or calculation fails.
#'
#' @noRd
#'
#' @import yardstick
#' @import dplyr
#' @import tibble
calculate_metrics <- function(y_true, y_pred) {

  if (length(y_true) != length(y_pred)) {
    warning("Length mismatch between true and predicted values. Cannot calculate metrics.")
    return(NULL)
  }
  if (length(y_true) == 0) {
    warning("Input vectors are empty. Cannot calculate metrics.")
    return(NULL)
  }
  if (!is.numeric(y_true) || !is.numeric(y_pred)) {
    warning("Input vectors must be numeric. Cannot calculate metrics.")
    return(NULL)
  }

  metrics_df <- NULL
  tryCatch({
    # Create a temporary dataframe for yardstick functions
    data_for_metrics <- tibble::tibble(
      truth = as.numeric(y_true),
      estimate = as.numeric(y_pred)
    )

    # Define the metric set
    regression_metrics <- yardstick::metric_set(
      yardstick::mae,
      yardstick::rmse,
      yardstick::mape
    )

    # Calculate metrics
    metrics_df <- regression_metrics(data_for_metrics, truth = truth, estimate = estimate)

    # Handle potential Inf MAPE if truth contains zeros
    metrics_df <- metrics_df %>%
      dplyr::mutate(.estimate = ifelse(is.infinite(.estimate), NA, .estimate))


  }, error = function(e) {
    warning(paste("Metric calculation failed:", e$message))
    metrics_df <<- NULL
  })

  return(metrics_df)
}
