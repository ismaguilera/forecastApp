# R/utils_preprocess.R

#' Create and Prepare a Recipe for XGBoost Time Series Forecasting
#'
#' This function generates time series features (lags, date components, rolling windows)
#' suitable for tree-based models like XGBoost using the recipes package.
#'
#' @param df A dataframe/tibble with at least 'ds' (Date) and 'y' (numeric value) columns.
#' @param date_col Character string, name of the date column (default 'ds').
#' @param value_col Character string, name of the value column (default 'y').
#' @param max_lag Integer, the maximum lag to generate for the value column (default 7).
#' @param window_sizes Integer vector, window sizes for rolling features (default c(7, 14)).
#' @param freq_str Character string, frequency of the data ('day' or 'week'), used
#'   to determine Fourier periods. Default 'day'.
#'
#' @return A prepared recipe object (`workflow` from recipes).
#'
#' @noRd
#'
#' @import recipes dplyr timetk rlang slider
#' @importFrom timetk step_timeseries_signature step_holiday_signature step_fourier
#' @importFrom slider slide_dbl
#' @importFrom lubridate wday mday yday week month year
#' @importFrom rlang !! sym
# Add !! for injection, ensure sym is still there if needed elsewhere
create_tree_recipe <- function(df, date_col = 'ds', value_col = 'y', max_lag = 7, window_sizes = c(7, 14), freq_str = "day") {
  internal_value_col <- "y"

  if (value_col != "y") {
    df_renamed <- df %>% dplyr::select(ds = !!rlang::sym(date_col), y = !!rlang::sym(value_col)) # Rename to ds, y
    warning("Input value column renamed to 'y' internally for recipe steps.")
  } else {
    df_renamed <- df %>% dplyr::select(ds, y) # Assume ds, y exist
  }

  # Ensure necessary columns exist
  stopifnot(date_col %in% names(df))
  stopifnot(value_col %in% names(df))
  stopifnot(inherits(df_renamed[[date_col]], "Date"))
  stopifnot(is.numeric(df_renamed[["y"]]))
  stopifnot(freq_str %in% c("day", "week"))

  max_window <- if(length(window_sizes) > 0 && !anyNA(window_sizes)) max(window_sizes) else 0
  if (max_window > 0 && max_lag < 1) {
    warning("Rolling features require max_lag >= 1. Setting max_lag=1.")
    max_lag <- 1
  }

  # Default step_lag naming seems to be lag_X_VariableName
  lag_cols_names_actual <- paste0("lag_", 1:max_lag, "_", internal_value_col) # e.g., lag_1_y

  # Check rows *after* confirming max_lag
  if(nrow(df_renamed) < max_lag + 1) stop(paste("Need at least", max_lag + 1, "rows for max_lag =", max_lag))
  # Create the recipe
  xgb_recipe <- recipes::recipe(y ~ ds, data = df_renamed) %>%
    # Omit rows with NA in the target *before* creating lags/windows
    recipes::step_naomit(y, skip = TRUE) %>% # skip=TRUE if you want it optional
    timetk::step_timeseries_signature(ds) %>% # Date features
    # Use appropriate selectors instead of matches() directly in step_rm
    recipes::step_rm(contains("iso"), contains("xts"), contains("hour"), contains("min"),
                     contains("sec"), contains("am.pm"), contains("qday"), contains("mday"),
                     contains("mday7"), contains("mweek"), contains("day")) %>%
    # Use appropriate selectors for step_normalize
    recipes::step_normalize(contains("index.num"), contains("year"), contains("yday")) %>%
    # Use appropriate selectors for step_mutate_at (or step_mutate + across)
    # step_mutate_at is superseded, using step_mutate with across is preferred
    recipes::step_mutate(across(ends_with("_wday") | ends_with("_lbl"), as.factor)) %>% # Use ends_with or contains

    # --- Fourier Terms ---
    # Determine periods based on frequency
    {
      if(freq_str == "day") {
        # For daily: Weekly and Yearly seasonality
        # K = number of sine/cosine pairs, e.g., K=2 for weekly, K=5 for yearly
        timetk::step_fourier(., ds, period = 7, K = 2) %>%
          timetk::step_fourier(., ds, period = 365.25, K = 5)
      } else if (freq_str == "week") {
        # For weekly: Yearly seasonality (approx 52.18 weeks/year)
        timetk::step_fourier(., ds, period = 365.25 / 7, K = 3)
      } else {
        . # Pass through if freq unknown
      }
    } %>%
    # --- End Fourier Terms ---

    # 1. Create Lags (Keep)
    recipes::step_lag( {{internal_value_col}} , lag = 1:max_lag) %>%
    # recipes::step_lag(y, lag = 1:28) %>%
    recipes::step_naomit(starts_with("lag_"), skip = TRUE) %>% # skip=TRUE if you want it optional

    # --- Rolling Window Features ---
    # 2. Apply Rolling Window Features to Lags
    { # Rolling Mean
      if (!!max_window > 0) {
        recipes::step_mutate(.,
                             across(all_of(lag_cols_names_actual),
                                    ~ slider::slide_dbl(.x, mean, na.rm = TRUE, .before = !!max_window - 1, .complete = FALSE),
                                    .names = paste0("roll_mean_", !!max_window, "_{.col}")
                             )
        )
      } else { . }
    } %>%
    { # Rolling Std Dev
      if (!!max_window > 0) {
        recipes::step_mutate(.,
                             across(all_of(lag_cols_names_actual),
                                    ~ slider::slide_dbl(.x, sd, na.rm = TRUE, .before = !!max_window - 1, .complete = FALSE),
                                    .names = paste0("roll_sd_", !!max_window, "_{.col}")
                             )
        )
      } else { . }
    } %>%
    # --- End Rolling Window ---

    recipes::step_rm(ds) %>% # Remove original date column AFTER feature extraction
    recipes::step_impute_mean(all_predictors(), -all_nominal_predictors()) %>% # Impute numerics (Keep)
    recipes::step_zv(all_predictors()) %>% # Remove zero-variance (inc single-level factors) BEFORE dummy
    recipes::step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% # Create dummies
    recipes::step_normalize(all_numeric_predictors(), -all_outcomes())  %>%
    recipes::step_zv(all_predictors()) # Remove ZV again after dummy creation

  # Return the UNPREPARED recipe object
  # The prep() step will be done later, either directly or within the tuning workflow
  return(xgb_recipe)
}
