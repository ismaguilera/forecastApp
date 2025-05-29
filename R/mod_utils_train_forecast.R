# R/utils_train_forecast.R

#' Train ARIMA Model
#'
#' Trains an ARIMA model using `forecast::Arima` or `forecast::auto.arima`.
#' The function can handle non-seasonal and seasonal ARIMA models, and can incorporate
#' external regressors derived from a provided holidays dataframe.
#'
#' @param train_df A tibble with 'ds' (Date) and 'y' (numeric) columns representing the training data.
#' @param config A list containing ARIMA parameters:
#'   \itemize{
#'     \item `auto`: Logical. If `TRUE`, uses `forecast::auto.arima` to find the best model.
#'           If `FALSE`, uses `forecast::Arima` with specified orders.
#'     \item `p`: Integer. The order of the non-seasonal AR part.
#'     \item `d`: Integer. The degree of non-seasonal differencing.
#'     \item `q`: Integer. The order of the non-seasonal MA part.
#'     \item `seasonal`: Logical. If `TRUE`, a seasonal model is fitted.
#'     \item `P`: Integer. The order of the seasonal AR part.
#'     \item `D`: Integer. The degree of seasonal differencing.
#'     \item `Q`: Integer. The order of the seasonal MA part.
#'     \item `period`: Integer. The seasonal period (e.g., 7 for daily data with weekly seasonality,
#'           52 for weekly data with yearly seasonality). This is used if `seasonal` is `TRUE` and `auto` is `FALSE`.
#'           If `auto` is `TRUE`, `period` is inferred from `aggregation_level`.
#'   }
#' @param aggregation_level Character string. Indicates data frequency ("Daily" or "Weekly").
#'   This influences the automatic determination of `frequency` for the `ts` object (e.g., 7 for "Daily", 52 for "Weekly")
#'   when `config$auto` is `TRUE` and `config$seasonal` is `TRUE`.
#' @param holidays_df Optional. A tibble with 'ds' (Date) and 'holiday' (character/factor) columns.
#'   If provided, it's processed to create an `xreg` matrix for the ARIMA model.
#'   For "Daily" aggregation, dummy variables are created for each holiday.
#'   For "Weekly" aggregation, a single regressor indicates if any holiday occurred within the week.
#'
#' @return A fitted ARIMA model object (class `ARIMA` or `forecast_ARIMA` from the `forecast` package).
#'   Returns `NULL` on error (e.g., insufficient data, invalid configuration).
#'   The returned model object may have attributes:
#'   \itemize{
#'     \item `aggregation_level`: The `aggregation_level` used.
#'     \item `holiday_regressor_type`: Type of holiday regressor used ("daily_dummies", "weekly_aggregated", or "none").
#'   }
#'
#' @noRd
#'
#' @import forecast dplyr tibble lubridate tidyr
#' @importFrom stats ts frequency start fitted cycle time model.matrix
train_arima <- function(train_df, config, aggregation_level, holidays_df = NULL) {
  message("Starting train_arima")
  # Basic validation
  if (!is.data.frame(train_df) || !all(c("ds", "y") %in% names(train_df))) {
    stop("train_df must be a dataframe with 'ds' and 'y' columns.")
  }
  if(nrow(train_df) < 5) { # Increased min rows slightly for robustness
    warning("ARIMA training requires sufficient data points. Returning NULL.")
    return(NULL)
  }
  if(!aggregation_level %in% c("Daily", "Weekly")) {
    warning("Unknown aggregation level provided to train_arima. Seasonality might be incorrect.")
    # Defaulting to non-seasonal or handle differently? Let's default freq to 1.
    aggregation_level <- "Unknown" # Mark as unknown
  }

  xreg_matrix_train <- NULL
  if (!is.null(holidays_df) && nrow(holidays_df) > 0 &&
      all(c("ds", "holiday") %in% names(holidays_df))) {

    # Filtrar feriados dentro del rango de train_df
    holidays_in_train_range <- holidays_df %>%
      dplyr::mutate(ds = as.Date(ds)) %>%
      dplyr::filter(ds >= min(train_df$ds) & ds <= max(train_df$ds))

    if (nrow(holidays_in_train_range) > 0) {
      message("ARIMA: Processing ", nrow(holidays_in_train_range), " holidays for xreg.")

      if (aggregation_level == "Weekly") {
        message("ARIMA: Using aggregated weekly holiday regressor.")
        
        # Ensure holidays_in_train_range$ds is Date
        holidays_in_train_range <- holidays_in_train_range %>%
          dplyr::mutate(ds = as.Date(ds))

        weekly_holiday_regressor_df <- train_df %>%
          dplyr::select(ds) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            has_holiday_in_week = {
              week_start <- ds
              week_end <- ds + lubridate::days(6)
              any(holidays_in_train_range$ds >= week_start & holidays_in_train_range$ds <= week_end)
            }
          ) %>%
          dplyr::ungroup() %>% # Ungroup after rowwise operation
          dplyr::mutate(has_holiday_in_week = as.integer(has_holiday_in_week)) # Convert boolean to 0/1

        if (nrow(weekly_holiday_regressor_df) > 0 && "has_holiday_in_week" %in% names(weekly_holiday_regressor_df)) {
          xreg_matrix_train <- as.matrix(weekly_holiday_regressor_df %>% dplyr::select(has_holiday_in_week))
          message("ARIMA: Weekly holiday regressor matrix created with 1 column.")
        } else {
          message("ARIMA: Weekly holiday regressor creation resulted in no columns or data.")
          xreg_matrix_train <- NULL
        }

      } else { # Daily or other aggregation levels
        message("ARIMA: Using daily holiday dummy variable regressors.")
        # Crear variables dummy para cada feriado
        # Asegurarse de que los nombres de las columnas sean válidos para R
        holiday_dummies_train <- holidays_in_train_range %>%
          dplyr::mutate(holiday = make.names(holiday), value = 1) %>% # make.names para asegurar nombres válidos
          tidyr::pivot_wider(names_from = holiday, values_from = value, values_fill = 0)

        # Unir con train_df para asegurar todas las fechas y el orden correcto
        # y luego seleccionar solo las columnas dummy
        temp_train_df_with_dummies <- train_df %>%
          dplyr::select(ds) %>%
          dplyr::left_join(holiday_dummies_train, by = "ds") %>%
          dplyr::arrange(ds)

        # Seleccionar solo las columnas de feriados (excluir 'ds')
        # y rellenar NAs (para días no feriados o feriados no presentes en el subconjunto) con 0
        xreg_candidates <- temp_train_df_with_dummies %>% dplyr::select(-ds)
        xreg_candidates[is.na(xreg_candidates)] <- 0

        if (ncol(xreg_candidates) > 0) {
          xreg_matrix_train <- as.matrix(xreg_candidates)
          # Guardar los nombres de las columnas de regresores para el pronóstico
          # Esto es crucial si se van a necesitar los mismos regresores para el futuro
          # Podrías almacenar attr(xreg_matrix_train, "holiday_names") <- colnames(xreg_matrix_train)
          # O devolverlo junto con el modelo.
        } else {
          message("ARIMA: No daily holiday regressors created after processing.")
        }
      }
    } else {
      message("ARIMA: No holidays found within the training data range.")
    }
  } else if (!is.null(holidays_df) && nrow(holidays_df) > 0) {
    warning("train_arima: holidays_df provided but missing 'ds' or 'holiday' columns.")
  }

  # Determine frequency based on period if seasonal, otherwise try to infer
  freq <- 1 # Default if non-seasonal or cannot determine
  period_used <- NA # Store the period actually used
  if (config$seasonal) {
    message("Seasonal ARIMA requested.")
    if (config$auto) {
      message("Auto ARIMA selected.")
      # Infer frequency for auto.arima based on aggregation level
      if (aggregation_level == "Daily") {
        freq <- 7 # Weekly seasonality for daily data
        message("Setting frequency=7 for daily data auto-seasonal ARIMA.")
      } else if (aggregation_level == "Weekly") {
        freq <- 52 # Yearly seasonality for weekly data (~52.18)
        message("Setting frequency=52 for weekly data auto-seasonal ARIMA.")
      } else {
        freq <- 1 # Fallback for unknown aggregation
        warning("Cannot determine auto frequency for unknown aggregation level. Assuming freq=1.")
      }
    } else {
      message("Manual ARIMA selected.")
      # Use user-defined period for manual seasonal ARIMA
      freq <- as.integer(config$period)
      if (is.na(freq) || freq <= 1) {
        warning("Invalid or missing seasonal period for manual ARIMA (<= 1). Disabling seasonal component.")
        freq <- 1
        config$seasonal <- FALSE # Override seasonal if period invalid
      } else {
        message(paste("Using user-defined frequency/period:", freq))
      }
    }
    period_used <- freq # Store the period decided upon
  } else {
    message("Non-seasonal ARIMA requested (freq=1).")
    freq <- 1 # Explicitly non-seasonal
  }

  # Add check for sufficient rows *for the frequency*
  if (nrow(train_df) < 2 * freq) {
    warning(paste("Insufficient data points (", nrow(train_df), ") for frequency =", freq, ". Need at least 2*freq. Disabling seasonality/returning NULL."))
    # Option 1: Disable seasonality if freq was > 1
    # freq <- 1
    # config$seasonal <- FALSE
    # period_used <- NA
    # Option 2: Return NULL (safer if model likely unstable)
    return(NULL)
  }

  # Create time series object
  start_date <- min(train_df$ds)
  # Correctly get year and period fraction for ts start
  start_year <- lubridate::year(start_date)

  ts_start <- if (freq > 1) {
    # Get position within the first cycle of the specified frequency
    day_in_cycle <- switch(as.character(freq),
                           "7" = lubridate::wday(start_date, week_start = 1), # Monday=1
                           "52" = lubridate::week(start_date),
                           # Add other common frequencies if needed (4=quarter, 12=month)
                           # Fallback using yday (less accurate for weeks/months)
                           floor(lubridate::yday(start_date) / (365.25 / freq)) + 1
    )
    c(start_year, day_in_cycle)
  } else {
    start_year # If freq=1, start is just the year
  }

  # if (freq > 1) {
  #   day_of_year <- lubridate::yday(start_date)
  #   start_frac <- day_of_year / (freq * (365.25/freq)) # Approximate fraction of period
  #   ts_start <- c(start_year, 1 + floor(start_frac * freq)) # Adjust start based on position within first period
  # } else {
  #   ts_start <- start_year # If frequency is 1, just use the year
  # }

  y_ts <- tryCatch({
    stats::ts(train_df$y, frequency = freq, start = ts_start)
  }, error = function(e) {
    warning(paste("Failed to create ts object:", e$message))
    return(NULL)
  })

  if(is.null(y_ts)) return(NULL)
 message("TS object created successfully.")
  message(paste("Created ts object with frequency:", stats::frequency(y_ts))) # Log actual ts frequency

  model <- NULL
  tryCatch({
    if (config$auto) {
      model <- forecast::auto.arima(y_ts,
                                    seasonal = config$seasonal, # Pass TRUE/FALSE correctly
                                    # Add other auto.arima args if desired
                                    stepwise = TRUE, approximation = TRUE, trace = FALSE,
                                    xreg = xreg_matrix_train) # AÑADIDO xreg
      message("auto.arima finished.")
      message("ARIMA model fitted successfully.") # Added success message
    } else {
      # Use determined freq as period for manual seasonal
      seasonal_list <- if(config$seasonal) list(order = c(config$P, config$D, config$Q), period = freq) else FALSE
      model <- forecast::Arima(y_ts,
                               order = c(config$p, config$d, config$q),
                               seasonal = seasonal_list,
                               xreg = xreg_matrix_train) # AÑADIDO xreg
      message("Manual Arima finished.")
      message("ARIMA model fitted successfully.") # Added success message
    }
  # tryCatch({
  #   if (config$auto) {
  #     model <- forecast::auto.arima(y_ts,
  #                                   seasonal = config$seasonal,
  #                                   stepwise = TRUE, # Faster
  #                                   approximation = TRUE, # Faster
  #                                   trace = FALSE # Suppress output
  #     )
  #   } else {
  #     if (config$seasonal) {
  #       model <- forecast::Arima(y_ts,
  #                                order = c(config$p, config$d, config$q),
  #                                seasonal = list(order = c(config$P, config$D, config$Q), period = freq)
  #       )
  #     } else {
  #       model <- forecast::Arima(y_ts, order = c(config$p, config$d, config$q))
  #     }
  #   }
  }, error = function(e) {
    warning(paste("ARIMA model training failed:", e$message))
    model <<- NULL # Assign NULL to the outer scope model variable
  })

  if (!is.null(model)) {
    attr(model, "aggregation_level") <- aggregation_level
    if (!is.null(xreg_matrix_train)) {
      if (aggregation_level == "Weekly" && ncol(xreg_matrix_train) == 1 && colnames(xreg_matrix_train)[1] == "has_holiday_in_week") {
        attr(model, "holiday_regressor_type") <- "weekly_aggregated"
      } else {
        attr(model, "holiday_regressor_type") <- "daily_dummies"
      }
    } else {
      attr(model, "holiday_regressor_type") <- "none"
    }
    message(paste("Stored attributes in model: aggregation_level =", attr(model, "aggregation_level"),
                  ", holiday_regressor_type =", attr(model, "holiday_regressor_type")))
  }

  message("Finished train_arima")
  return(model)
}


#' Forecast using ARIMA Model
#'
#' Generates forecasts from a trained ARIMA model.
#'
#' @param model A fitted ARIMA model object from `train_arima`.
#' @param total_periods_needed Integer, number of periods to forecast ahead.
#' @param train_end_date Date. The last date of the training data.
#' @param freq_str Character string. Frequency for date sequence generation ('day', 'week').
#' @param future_xreg Optional matrix. External regressors for the forecast period.
#'   If the model was trained with xreg and `future_xreg` is not provided, this function
#'   will attempt to generate it based on `holidays_df` and model attributes.
#' @param holidays_df Optional. A tibble with 'ds' and 'holiday' columns, used to generate
#'   `future_xreg` if it's not directly provided and the model used holidays.
#'
#' @return A list containing:
#'   \itemize{
#'     \item `forecast`: A tibble with columns: 'ds', 'yhat', 'yhat_lower_80', 'yhat_upper_80',
#'           'yhat_lower_95', 'yhat_upper_95'.
#'     \item `fitted`: A numeric vector of fitted values from the model.
#'   }
#'   Returns `NULL` if `model` is invalid or an error occurs.
#'
#' @noRd
#'
#' @import forecast dplyr tibble lubridate
#' @importFrom stats time frequency cycle fitted
forecast_arima <- function(model, total_periods_needed, train_end_date, freq_str = "day", future_xreg = NULL, holidays_df = NULL) {
  if (is.null(model) || !inherits(model, c("ARIMA", "forecast_ARIMA"))) {
    message("Starting forecast_arima - Invalid model")
    warning("Invalid ARIMA model object provided. Returning NULL.")
    return(NULL)
  }
  if (!is.numeric(total_periods_needed) || total_periods_needed < 1) {
    warning("Invalid horizon provided. Returning NULL.")
    return(NULL)
  }

  message("Starting forecast_arima")
  # First, generate forecast_dates as they are needed for xreg generation and output
  by_period <- switch(freq_str,
                      "day" = lubridate::days(1),
                      "week" = lubridate::weeks(1),
                      lubridate::days(1) # Default
  )
  # Correct start_forecast_date based on freq_str
  start_forecast_date <- if (freq_str == "week") {
    train_end_date + lubridate::weeks(1)
  } else {
    train_end_date + lubridate::days(1)
  }
  forecast_dates <- seq.Date(from = start_forecast_date, by = freq_str, length.out = total_periods_needed)

  if (!is.null(model$xreg) && is.null(future_xreg)) {
    message("ARIMA model was trained with xreg, and future_xreg was not provided. Attempting to generate future_xreg.")
    aggregation_level_trained <- attr(model, "aggregation_level")
    holiday_type_trained <- attr(model, "holiday_regressor_type")

    if (is.null(aggregation_level_trained) || is.null(holiday_type_trained)) {
      warning("ARIMA Forecast: Model is missing 'aggregation_level' or 'holiday_regressor_type' attribute. Cannot generate future_xreg.")
    } else if (is.null(holidays_df) || nrow(holidays_df) == 0) {
      warning("ARIMA Forecast: holidays_df is missing or empty. Cannot generate future_xreg.")
    } else if (holiday_type_trained == "none") {
      message("ARIMA Forecast: Model was trained without holiday regressors (type 'none'). No future_xreg to generate.")
    } else {
      # Ensure holidays_df has 'ds' as Date
      holidays_df_forecast <- holidays_df %>% dplyr::mutate(ds = as.Date(ds))

      if (holiday_type_trained == "weekly_aggregated") {
        message("ARIMA Forecast: Generating future_xreg for weekly aggregated holidays.")
        
        holidays_in_forecast_range <- holidays_df_forecast %>%
          dplyr::filter(ds >= min(forecast_dates) & ds <= (max(forecast_dates) + lubridate::days(6))) # cover full last week

        future_xreg_df <- tibble::tibble(ds = forecast_dates) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            has_holiday_in_week = {
              week_start_current <- ds
              week_end_current <- ds + lubridate::days(6)
              any(holidays_in_forecast_range$ds >= week_start_current & holidays_in_forecast_range$ds <= week_end_current)
            }
          ) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(has_holiday_in_week = as.integer(has_holiday_in_week))
        
        if (nrow(future_xreg_df) > 0 && "has_holiday_in_week" %in% names(future_xreg_df)) {
          future_xreg <- as.matrix(future_xreg_df %>% dplyr::select(has_holiday_in_week))
          message("ARIMA Forecast: Generated weekly_aggregated future_xreg with ", nrow(future_xreg), " rows and 1 column.")
        } else {
           message("ARIMA Forecast: Failed to generate weekly_aggregated future_xreg components.")
           future_xreg <- NULL # Ensure it's NULL if generation failed
        }

      } else if (holiday_type_trained == "daily_dummies") {
        message("ARIMA Forecast: Generating future_xreg for daily holiday dummies.")
        trained_holiday_names <- colnames(model$xreg)
        if (is.null(trained_holiday_names) || length(trained_holiday_names) == 0) {
          warning("ARIMA Forecast: Model was trained with daily_dummies, but no regressor names found in model$xreg colnames.")
          future_xreg <- NULL
        } else {
          holidays_in_forecast_range <- holidays_df_forecast %>%
            dplyr::filter(ds >= min(forecast_dates) & ds <= max(forecast_dates))

          if (nrow(holidays_in_forecast_range) > 0) {
            future_holiday_dummies_raw <- holidays_in_forecast_range %>%
              dplyr::mutate(holiday = make.names(holiday), value = 1) %>%
              tidyr::pivot_wider(names_from = holiday, values_from = value, values_fill = 0)
            
            future_dates_df_template <- tibble::tibble(ds = forecast_dates)
            
            temp_future_df_with_dummies <- future_dates_df_template %>%
              dplyr::left_join(future_holiday_dummies_raw, by = "ds")
            
            # Create a zero matrix with correct column names and order
            future_xreg_matrix_template <- matrix(0, nrow = total_periods_needed, ncol = length(trained_holiday_names))
            colnames(future_xreg_matrix_template) <- trained_holiday_names
            
            # Fill the template
            for (col_name in trained_holiday_names) {
              if (col_name %in% names(temp_future_df_with_dummies)) {
                # Ensure to replace NAs that arise from left_join if a date had no holiday
                future_xreg_matrix_template[, col_name] <- ifelse(is.na(temp_future_df_with_dummies[[col_name]]), 0, temp_future_df_with_dummies[[col_name]])
              }
            }
            future_xreg <- future_xreg_matrix_template
            message("ARIMA Forecast: Generated daily_dummies future_xreg with ", nrow(future_xreg), " rows and ", ncol(future_xreg), " columns.")
          } else {
            message("ARIMA Forecast: No holidays found in the forecast range for daily_dummies. Generating zero matrix.")
            future_xreg <- matrix(0, nrow = total_periods_needed, ncol = length(trained_holiday_names))
            colnames(future_xreg) <- trained_holiday_names
          }
        }
      } else {
        warning(paste("ARIMA Forecast: Unknown holiday_regressor_type '", holiday_type_trained, "'. Cannot generate future_xreg.", sep=""))
      }
    }
  } else if (!is.null(model$xreg) && !is.null(future_xreg)) {
     message("ARIMA Forecast: future_xreg was provided directly.")
  }


  # Validation of generated or provided future_xreg
  if (!is.null(model$xreg) && is.null(future_xreg)) { # If xreg was expected but not generated/provided
      warning("ARIMA model was trained with xreg, but future_xreg is NULL after generation attempt. Forecasting without xreg.")
  } else if (!is.null(model$xreg) && !is.null(future_xreg)) { # If xreg is present
    if (ncol(future_xreg) != ncol(model$xreg)) {
      stop(paste0("Mismatch in xreg columns for forecast. Model: ", ncol(model$xreg), ", Future: ", ncol(future_xreg), ". Check generation logic or provided future_xreg."))
    }
    if (nrow(future_xreg) != total_periods_needed) {
      stop(paste0("Number of rows in future_xreg (", nrow(future_xreg), ") does not match total_periods_needed (", total_periods_needed, ")."))
    }
    if (!is.matrix(future_xreg)) {
      warning("Generated future_xreg is not a matrix. Attempting to convert.")
      future_xreg <- as.matrix(future_xreg)
    }
  }
  fcst <- NULL
  tryCatch({
    fcst <- forecast::forecast(model, h = total_periods_needed, level = c(80, 95), xreg = future_xreg)
  }, error = function(e) {
    warning(paste("ARIMA forecast generation failed:", e$message))
    fcst <<- NULL
  })

  if(is.null(fcst)) {
    warning("ARIMA forecast generation failed, fcst object is NULL.") # Added warning
    return(NULL)
  }

  # Convert forecast object to a tibble
  fcst_df <- tryCatch({
    # Ensure dates vector length matches forecast length
    if(length(forecast_dates) != length(fcst$mean)) {
      stop(paste0("Generated forecast_dates length (", length(forecast_dates), 
                  ") does not match forecast horizon mean length (", length(fcst$mean), ")."))
    }

    tibble::tibble(
      ds = forecast_dates, # Use the forecast_dates generated earlier
      yhat = as.numeric(fcst$mean),
      yhat_lower_80 = as.numeric(fcst$lower[, 1]),
      yhat_upper_80 = as.numeric(fcst$upper[, 1]),
      yhat_lower_95 = as.numeric(fcst$lower[, 2]),
      yhat_upper_95 = as.numeric(fcst$upper[, 2])
    ) # No need to slice, seq.Date generates correct length
  }, error = function(e) {
    warning(paste("Failed to convert ARIMA forecast to tibble:", e$message))
    return(NULL)
  })


  # return(fcst_df)
  # Also return fitted values for convenience in server logic
  message(paste("forecast_arima: About to call stats::fitted() on object of class:", class(fcst))) # Added message
  # Note: The original forecast object `fcst` is needed for this
  fitted_vals <- stats::fitted(fcst)
  message(paste("forecast_arima: Length of fitted values:", length(fitted_vals))) # Added message
  message(paste("forecast_arima: Any NAs in fitted values?", anyNA(fitted_vals))) # Added message

  message("Finished forecast_arima")
  return(list(
    forecast = fcst_df,
    fitted = fitted_vals # Use stats::fitted on forecast object
  ))
}

#' Train ETS Model
#'
#' Trains an Exponential Smoothing State Space Model (ETS) using `forecast::ets`.
#' For higher frequency data (e.g., daily data with yearly seasonality, frequency > 24),
#' it may use `forecast::stlf`, which performs an STL decomposition and then fits an
#' ETS model to the seasonally adjusted data.
#'
#' @param train_df A tibble with 'ds' (Date) and 'y' (numeric) columns for training.
#' @param config A list containing ETS parameters:
#'   \itemize{
#'     \item `manual`: Logical. If `TRUE`, uses manually specified ETS components.
#'           If `FALSE` (default), components are automatically selected by `forecast::ets` (model="ZZZ").
#'     \item `ets_e`: Character. Error component ('A'-Additive, 'M'-Multiplicative, 'Z'-Auto).
#'     \item `ets_t`: Character. Trend component ('N'-None, 'A'-Additive, 'M'-Multiplicative, 'Z'-Auto).
#'     \item `ets_s`: Character. Seasonality component ('N'-None, 'A'-Additive, 'M'-Multiplicative, 'Z'-Auto).
#'     \item `ets_damped_str`: Character. Damping parameter ('TRUE', 'FALSE', 'NULL' for auto).
#'           Note: internally converted to logical `TRUE`, `FALSE`, or `NULL`.
#'   }
#' @param aggregation_level Character string. Data frequency ("Daily", "Weekly").
#'   This influences the `frequency` of the `ts` object (7 for "Daily", 52 for "Weekly").
#'   If the determined frequency is > 24, `forecast::stlf` is used; otherwise, `forecast::ets` is used directly.
#' @param total_periods_needed Integer. The total number of periods for which forecasts will eventually be needed.
#'   This is passed as the `h` argument to `forecast::stlf` if `stlf` is used, as `stlf`
#'   returns a forecast object directly.
#'
#' @return A fitted model object. This can be an `ets` object (if `forecast::ets` was used directly)
#'   or a `forecast` object (if `forecast::stlf` was used, as `stlf` returns a forecast object
#'   containing the underlying model and the forecast). Returns `NULL` on error.
#'
#' @noRd
#' @import forecast dplyr lubridate stats
train_ets <- function(train_df, config, aggregation_level, total_periods_needed) {
  message("Starting train_ets")
  # Basic validation
  if (!is.data.frame(train_df) || !all(c("ds", "y") %in% names(train_df))) {
    stop("train_df must be a dataframe with 'ds' and 'y' columns.")
  }
  if(nrow(train_df) < 5) {
    warning("ETS training requires sufficient data points. Returning NULL.")
    return(NULL)
  }
  has_nonpos <- any(train_df$y <= 0, na.rm = TRUE)
  if(has_nonpos) {
    message("Data has non-positive values. Multiplicative ETS components may be restricted.")
    }

  if(!aggregation_level %in% c("Daily", "Weekly")) {
    warning("Unknown aggregation level provided to train_ets. Seasonality might be incorrect.")
    aggregation_level <- "Unknown"
  }

  # --- Determine Frequency (Period) ---
  # Use same logic as ARIMA for consistency when seasonality might be present
  # freq <- 1

  # --- Determine Frequency (as before) ---
  # freq <- 1
  # if (config$seasonal) {
  #   if (config$auto) {
  #     if (aggregation_level == "Daily") freq <- 7
  #     else if (aggregation_level == "Weekly") freq <- 52
  #   } else { # Manual seasonal
  #     manual_period <- as.integer(config$period) # Assumes config$period exists from server
  #     if (!is.na(manual_period) && manual_period > 1) { freq <- manual_period
  #     } else { freq <- 1; config$seasonal <- FALSE; warning("Manual seasonal period invalid (<=1). Treating as non-seasonal.") }
  #   }
  # }
  # message(paste("ETS: Determined frequency:", freq))
  # --- End Frequency Determination ---

  freq <- 1 # Default to non-seasonal
  attempt_seasonal <- FALSE # Flag if seasonality is possible

  if (aggregation_level == "Daily") {
    freq <- 7
    attempt_seasonal <- TRUE
    message("ETS: Setting frequency=7 for daily data.")
  } else if (aggregation_level == "Weekly") {
    freq <- 52
    attempt_seasonal <- TRUE
    message("ETS: Setting frequency=52 for weekly data.")
    }

  # Check if manual config explicitly disables seasonality
  if (config$manual && config$ets_s == "N") {
    message("Manual ETS config explicitly set Seasonality to None ('N').")
    attempt_seasonal <- FALSE # Override based on user input
    freq <- 1 # Force non-seasonal frequency
  }

  # If no seasonality possible based on aggregation or manual setting, ensure freq is 1
  if (!attempt_seasonal) { freq <- 1 }
  message(paste("ETS: Determined frequency for ts object:", freq))

  #
  # if (aggregation_level == "Daily") {
  #   freq <- 7
  #   message("ETS: Setting frequency=7 for daily data.")
  # } else if (aggregation_level == "Weekly") {
  #   freq <- 52 # Yearly seasonality for weekly data
  #   message("ETS: Setting frequency=52 for weekly data.")
  # } else {
  #   message("ETS: Frequency=1 (non-seasonal or unknown aggregation).")
  # }
  # --- End Frequency Determination ---
  #  Check for sufficient data points for the frequency

  # Check for sufficient data points for the frequency
  if (freq > 1 && nrow(train_df) < 2 * freq) {
    warning(paste("Insufficient data (", nrow(train_df), ") for frequency =", freq, ". Fitting non-seasonal ETS instead."))
    freq <- 1 # Revert to non-seasonal if not enough data
    # return(NULL) # Return NULL if definitely not enough data
  }
  # Check for negative values if multiplicative seasonality might be chosen
  # if(freq > 1 && any(train_df$y <= 0, na.rm = TRUE)) {
  #   message("Data contains zero or negative values. Forcing additive methods in ETS.")
  #   allow_multiplicative <- FALSE
  # } else {
  #   allow_multiplicative <- TRUE # Allow ets() to choose if data is positive
  # }

  # Create time series object
  y_ts <- NULL
  tryCatch({
    start_date <- min(train_df$ds)
    start_year <- lubridate::year(start_date)
    ts_start <- if (freq > 1) {
      day_in_cycle <- switch(as.character(freq),
                             "7" = lubridate::wday(start_date, week_start = 1),
                             "52" = lubridate::week(start_date),
                             floor(lubridate::yday(start_date) / (365.25 / freq)) + 1)
      c(start_year, day_in_cycle)
    } else { start_year }
    y_ts <- stats::ts(train_df$y, frequency = freq, start = ts_start)
  }, error = function(e){
    warning(paste("Failed to create ts object for ETS:", conditionMessage(e)))
    return(NULL) # Return NULL if ts creation fails
  })

  if(is.null(y_ts)) return(NULL)
  message("TS object created successfully.") # Added message
  message(paste("Created ts object with frequency:", stats::frequency(y_ts))) # Log actual frequency

  # --- Determine ETS Arguments ---
  # Initialize base args list
  ets_call_args <- list() # Will be passed to do.call(ets, ...)
  stlm_ets_args <- list() # Will be passed to stlm(..., model.args = stlm_ets_args)

  # model_code_3letter <- "ZZZ" # Default for model/etsmodel arg
  damped_arg <- NULL        # Default for damped arg

  ets_model_code <- "ZZZ" # For etsmodel arg in stlm or model in ets
  # ets_damped_arg <- NULL # For damped arg in stlm/ets
  # allow_mult <- !has_nonpos # Base setting

  if (config$manual) {
    message("Manual ETS configuration selected.")
    e_comp <- config$ets_e; t_comp <- config$ets_t; s_comp <- config$ets_s
    # Force S='N' if freq is 1 (consistent with freq determination above)
    if (freq <= 1) s_comp <- "N"
    # Validate multiplicative with non-positive data
    if (has_nonpos && (e_comp == "M" || t_comp == "M" || s_comp == "M")) {
      warning("Manual selection includes Multiplicative component(s) but data is non-positive. Check results.")
    }
    # Construct model code only if not all Z
    if (e_comp != "Z" || t_comp != "Z" || s_comp != "Z"){ ets_model_code <- paste0(e_comp, t_comp, s_comp) }

    # Handle damped argument string
    damped_input <- config$ets_damped_str
    if (damped_input == "TRUE") damped_arg <- TRUE
    else if (damped_input == "FALSE") damped_arg <- FALSE

    message(paste("Manual settings: Model Code=", ets_model_code %||% "Auto", ", Damped=", damped_input %||% "Auto"))

    # Set args for ets/stlm
    if (ets_model_code != "ZZZ") {
      ets_call_args$model <- ets_model_code
      stlm_ets_args$model <- ets_model_code
    }
    if (!is.null(damped_arg)) {
      ets_call_args$damped <- damped_arg
      stlm_ets_args$damped <- damped_arg
    }
  } else { # Automatic
    message("Automatic ETS configuration selected (model='ZZZ', damped=NULL).")
    ets_call_args$model <- "ZZZ"
    ets_call_args$damped <- NULL
    stlm_ets_args$model <- "ZZZ" # Also pass ZZZ to underlying ETS in stlm
    stlm_ets_args$damped <- NULL
    # Handle non-positive restriction for direct ets call
    if(has_nonpos && freq > 1) {
      message("Restricting direct ets() auto-selection: setting allow.multiplicative.trend = FALSE.")
      ets_call_args$allow.multiplicative.trend <- FALSE
    }
  }
  model_or_fcst <- NULL # Can store ets model or stlf forecast object
  tryCatch({
    # --- Choose method based on frequency ---
    # Use stlf for freq > 24
    # Use 24 as threshold based on ets warning
    if (freq > 24 && freq <= nrow(train_df)/2) {

      message(paste("Frequency is high (", freq,"). Using forecast::stlf()..."))
      # Prepare arguments for stlf (which passes them to ets for the non-seasonal fit)
      stlf_args <- list(y = y_ts)
      # Arguments for the underlying ETS fit: etsmodel, damped
      if (ets_model_code != "ZZZ") stlf_args$etsmodel <- ets_model_code
      if (!is.null(damped_arg)) stlf_args$damped <- damped_arg
      # Add Box-Cox lambda if needed later: stlf_args$lambda = ...
      # Add biasadj if needed later: stlf_args$biasadj = TRUE/FALSE
      stlf_args$h <- total_periods_needed
      stlf_args <- stlf_args[!sapply(stlf_args, is.null)] # Remove NULLs
      message("Arguments for stlf():")
      print(str(stlf_args))
      # stlf directly returns a forecast object
      model_or_fcst <- do.call(forecast::stlf, args = stlf_args)

      message("stlf() finished.")
      if(!is.null(model_or_fcst$model$method)){
        message("Underlying ETS model fitted by stlf: ", model_or_fcst$model$method)
        }
      message("ETS model fitted successfully.") # Added success message



      # message(paste("Frequency is high (", freq,"). Using forecast::stlm() [may warn]..."))
      # stlm_args <- list(y = y_ts, modelfunction = forecast::ets)
      # stlm_model_args <- list()
      # if (ets_model_code != "ZZZ") stlm_model_args$model <- ets_model_code
      # if (!is.null(damped_arg)) stlm_model_args$damped <- damped_arg
      # if (length(stlm_model_args) > 0) stlm_args$model.args <- stlm_model_args
      # message("Arguments for stlm(modelfunction=ets):"); print(str(stlm_args))
      #
      # # Suppress warning and check result
      # model_or_fcst <- suppressWarnings(do.call(forecast::stlm, args = stlm_args))
      #
      # # Check if underlying ETS fit failed
      # if (!inherits(model_or_fcst, "stlm") || is.null(model_or_fcst$model)) {
      #   stop("STLM model fitting failed, possibly due to internal ETS failure (check frequency/data length).")
      # }
      # message("stlm() finished.")


      # message("Arguments for stlf():")
      # print(str(stlf_args))
      #
      # # stlf directly returns a forecast object
      # model_or_fcst <- do.call(forecast::stlf, args = stlf_args)
      #
      # message("stlf() finished.")
      # # Check the underlying model method if possible
      # if(!is.null(model_or_fcst$model$method)){
      #   message("Underlying ETS model fitted by stlf: ", model_or_fcst$model$method)
      # } else {
      #   # If ARIMA was used as method='arima' (not implemented here)
      #   if(!is.null(model_or_fcst$model$arma)){message("Underlying ARIMA model fitted by stlf")}
      # }

    } else { # Use direct ETS for lower seasonal freq or non-seasonal
      message(paste("Frequency is", freq, ". Using forecast::ets()..."))
      # Prepare arguments for direct ets call
      # ets_call_args <- list(y = y_ts, model = ets_model_code, damped = damped_arg)
      ets_call_args <- list(model = ets_model_code, damped = damped_arg)
      if(has_nonpos && freq > 1 && !config$manual) { # Restrict auto selection only
        message("Restricting direct ets() auto-selection: allow.multiplicative.trend = FALSE.")
        ets_call_args$allow.multiplicative.trend <- FALSE
      }
      ets_call_args <- ets_call_args[!sapply(ets_call_args, is.null)]
      ets_call_args$y <- y_ts
      message("Arguments for direct ets() call:"); print(str(ets_call_args))
      model_or_fcst <- do.call(forecast::ets, args = ets_call_args)
      message("ets() finished. Final model: ", model_or_fcst$method)
      message("ETS model fitted successfully.") # Added success message

    }
    # --- End Choose method ---

  }, error = function(e) {
    warning(paste("ETS/STLF model training failed:", conditionMessage(e)))
    model_or_fcst <<- NULL
  })

  message("Finished train_ets")
  return(model_or_fcst) # Return either ets model or forecast object from stlf

  # model <- NULL
  # tryCatch({
  #   # --- Choose method based on FINAL freq value ---
  #   if (freq >= 52 && freq <= nrow(train_df)/2) {
  #     message(paste("Frequency is high (", freq,"). Using forecast::stlm() with underlying ETS model..."))
  #     # Remove NULLs from underlying ETS args
  #     stlm_ets_args <- stlm_ets_args[!sapply(stlm_ets_args, is.null)]
  #     message("Arguments for underlying ETS fit in STLM:")
  #     print(str(stlm_ets_args))
  #     model <- forecast::stlm(y = y_ts, modelfunction = forecast::ets, model.args = stlm_ets_args)
  #     message("stlm() finished.")
  #     if(!is.null(model$model$method)){ message("Underlying ETS model selected by stlm: ", model$model$method)}
  #
  #   } else { # Use direct ETS for lower seasonal freq or non-seasonal
  #     message(paste("Frequency is", freq, ". Using forecast::ets()..."))
  #     # Force S='N' in model code if freq=1 (already handled if manual s_comp='N')
  #     if(freq <= 1 && !is.null(ets_call_args$model) && ets_call_args$model != "ZZZ") {
  #       current_model_code <- ets_call_args$model
  #       non_seasonal_code <- paste0(substr(current_model_code,1,2), "N")
  #       if (current_model_code != non_seasonal_code) {
  #         message(paste("Forcing Season='N' in manual model code due to freq=1. New code:", non_seasonal_code))
  #         ets_call_args$model <- non_seasonal_code
  #       }
  #     }
  #     # Add y_ts to args and remove NULLs
  #     ets_call_args$y <- y_ts
  #     ets_call_args <- ets_call_args[!sapply(ets_call_args, is.null)]
  #     message("Arguments for direct ets() call:")
  #     print(str(ets_call_args))
  #     model <- do.call(forecast::ets, args = ets_call_args)
  #     message("ets() finished. Final model: ", model$method)
  #   }
  #   # --- End Choose method ---
  #
  # }, error = function(e) {
  #   warning(paste("ETS/STLM model training failed:", conditionMessage(e)))
  #   model <<- NULL
  # })

  # if (config$manual) {
  #   message("Manual ETS configuration selected.")
  #   e_comp <- config$ets_e; t_comp <- config$ets_t; s_comp <- config$ets_s
  #   # Validate manual selections
  #   if (has_nonpos && (e_comp == "M" || t_comp == "M" || s_comp == "M")) {
  #     warning("Manual selection includes Multiplicative component(s) but data is non-positive. Check results.")
  #   }
  #   if (freq <= 1 && s_comp != "N" && s_comp != "Z") { s_comp <- "N" }
  #   # Construct model code only if NOT ZZZ
  #   if (e_comp != "Z" || t_comp != "Z" || s_comp != "Z"){ ets_model_code <- paste0(e_comp, t_comp, s_comp) }
  #   # Handle damped argument string
  #   damped_input <- config$ets_damped_str
  #   if (damped_input == "TRUE") ets_damped_arg <- TRUE
  #   else if (damped_input == "FALSE") ets_damped_arg <- FALSE
  #   message(paste("Manual settings: Model Code=", ets_model_code, ", Damped=", damped_input))
  # } else { # Automatic
  #   message("Automatic ETS configuration selected.")
  #   if(has_nonpos && freq > 1) {
  #     message("Restricting auto-selection: allow.multiplicative.trend = FALSE for ets().")
  #     allow_mult <- FALSE # Specifically for direct ets call
  #   }
  # }

  # model_code <- "ZZZ" # Default: auto everything
  # damped_arg <- NULL # Default: auto damping
  # ets_call_args <- list(y = y_ts) # Start list of arguments for ets()

  # if (config$manual) {
  #   message("Manual ETS configuration selected.")
  #   e_comp <- config$ets_e
  #   t_comp <- config$ets_t
  #   s_comp <- config$ets_s
  #
  #   # Validate manual selections
  #   if (has_nonpos && (e_comp == "M" || t_comp == "M" || s_comp == "M")) {
  #     warning("Manual selection includes Multiplicative component(s) but data is non-positive. Check results carefully or use Additive/Auto.")
  #     # Allow ets() to handle it, but warn user
  #   }
  #   if (freq <= 1 && s_comp != "N" && s_comp != "Z") {
  #     warning("Seasonal component specified manually but data frequency is 1. Setting Seasonality to 'N'.")
  #     s_comp <- "N"
  #   }
  #
  #   # Only construct model code if NOT ZZZ
  #   if (e_comp != "Z" || t_comp != "Z" || s_comp != "Z"){
  #     model_code <- paste0(e_comp, t_comp, s_comp)
  #     ets_call_args$model <- model_code # Add specific model code to args
  #     message(paste("Manual model code constructed:", model_code))
  #   } else {
  #     message("Manual selection set all components to Auto ('Z'). Using default ZZZ.")
  #     ets_call_args$model <- "ZZZ" # Explicitly set ZZZ if all were Z
  #   }
  #
  #
  #   # Handle damped argument string
  #   damped_input <- config$ets_damped_str
  #   if (damped_input == "TRUE") damped_arg <- TRUE
  #   else if (damped_input == "FALSE") damped_arg <- FALSE
  #   # Only add damped argument if not NULL (Auto)
  #   if (!is.null(damped_arg)) {
  #     ets_call_args$damped <- damped_arg
  #   }
  #   message(paste("Manual damping selection:", damped_input))
  #
  # } else {
  #   message("Automatic ETS configuration selected (model='ZZZ', damped=NULL).")
  #   ets_call_args$model <- "ZZZ"
  #   ets_call_args$damped <- NULL
  #   # Force additive if needed for automatic selection
  #   if(has_nonpos && freq > 1) {
  #     message("Forcing additive methods for auto-selection due to non-positive data.")
  #     ets_call_args$allow.multiplicative.trend <- FALSE # Explicitly disallow
  #     # Note: ets() might still pick 'M' for error if model="ZZZ",
  #     # consider additive.only=TRUE if stricter control needed
  #   }
  # }

  # model <- NULL
  # tryCatch({
  #   # --- Choose method based on frequency ---
  #   # Use stlm for high frequency (e.g., >= 52) or if direct ets fails? Let's try direct first for lower freq.
  #   if (freq >= 52) {
  #     message(paste("Frequency is high (", freq,"). Using forecast::stlm() with underlying ETS model..."))
  #     # Arguments for the underlying ets fit within stlm
  #     ets_args_for_stlm <- list(model = ets_model_code, damped = ets_damped_arg)
  #     # Remove NULLs
  #     ets_args_for_stlm <- ets_args_for_stlm[!sapply(ets_args_for_stlm, is.null)]
  #     # Add allow_mult if relevant for auto 'ZZZ' case (stlm might handle non-positivity)
  #     if (ets_model_code == "ZZZ" && !allow_mult) {
  #       # This might not be directly supported via stlm args, stlm's ETS fit might handle it.
  #       # Let's rely on stlm's internal handling for now.
  #       message("Note: Relying on stlm's internal ETS fit to handle non-positive data.")
  #     }
  #
  #     model <- forecast::stlm(y = y_ts,
  #                             modelfunction = forecast::ets, # Specify ets as the function
  #                             model.args = ets_args_for_stlm # Pass args for ets
  #                             # s.window = "periodic" # Can set STL window here
  #     )
  #     message("stlm() finished.")
  #     if(!is.null(model$model$method)){ message("Underlying ETS model selected by stlm: ", model$model$method)}
  #
  #   } else { # Use direct ETS for lower seasonal frequencies or non-seasonal
  #     message(paste("Frequency is", freq, ". Using forecast::ets()..."))
  #     # Prepare arguments for direct ets call
  #     ets_call_args <- list(y = y_ts, model = ets_model_code, damped = ets_damped_arg)
  #     # Only add allow_mult if FALSE (default is TRUE)
  #     if (!allow_mult) { ets_call_args$allow.multiplicative.trend <- FALSE }
  #     # Remove NULLs
  #     ets_call_args <- ets_call_args[!sapply(ets_call_args, is.null)]
  #
  #     model <- do.call(forecast::ets, args = ets_call_args)
  #     message("ets() finished. Final model: ", model$method)
  #   }
  #   # --- End Choose method ---
  #
  # }, error = function(e) {
  #   warning(paste("ETS/STLM model training failed:", conditionMessage(e)))
  #   model <<- NULL
  # })

  # model <- NULL
  # tryCatch({
  #   # Use automatic model selection (ZZZ) by default
  #   # Force additive methods if data has non-positive values and freq > 1
  #   model <- do.call(forecast::ets, args = ets_call_args)
  #   message("ets() finished. Final model: ", model$method)
  #
  # }, error = function(e) {
  #   warning(paste("ETS model training failed:", conditionMessage(e)))
  #   model <<- NULL
  # })

  # return(model)
}



#' Forecast using ETS Model or Extract from STLF Result
#'
#' Generates forecasts from a trained ETS model or extracts forecast and fitted
#' values if the input is already a `forecast` object (e.g., from `stlf`).
#'
#' @param model_or_fcst A fitted `ets` model object (from `forecast::ets`) or a `forecast`
#'   object (typically returned by `train_ets` if `forecast::stlf` was used).
#' @param total_periods_needed Integer. The total number of periods for which the forecast is required.
#'   If `model_or_fcst` is an `ets` model, `forecast()` is called with `h = total_periods_needed`.
#'   If `model_or_fcst` is a `forecast` object, this parameter is used to validate that the
#'   existing forecast horizon matches the requirement.
#' @param train_end_date Date. The last date of the training data, used to generate the
#'   date sequence for the forecast tibble.
#' @param freq_str Character string. The frequency of the time series ('day', 'week'),
#'   used for generating the date sequence.
#'
#' @return A list containing two elements:
#'   \itemize{
#'     \item `forecast`: A tibble with columns 'ds' (Date), 'yhat' (point forecast),
#'           'yhat_lower_80', 'yhat_upper_80', 'yhat_lower_95', 'yhat_upper_95' (confidence intervals).
#'     \item `fitted`: A numeric vector of fitted values from the model.
#'   }
#'   Returns `NULL` if `model_or_fcst` is invalid or if an error occurs during processing.
#'
#' @noRd
#' @import forecast dplyr tibble lubridate stats
forecast_ets <- function(model_or_fcst, total_periods_needed, train_end_date, freq_str = "day") {
  message("Starting forecast_ets")
  # Check if model is ets OR stlm
  # if (is.null(model) || (!inherits(model, "ets") && !inherits(model, "stlm"))) {
  #   warning("Invalid ETS/STLM model object provided. Returning NULL.")
  #   return(NULL)
  # }
  # # Basic validation
  # if (is.null(model) || !inherits(model, "ets")) {
  #   warning("Invalid ETS model object provided. Returning NULL.")
  #   return(NULL)
  # }
  # Check input object type
  if (is.null(model_or_fcst)) {
    message("Starting forecast_ets - Invalid model/forecast object (NULL)") # Added message
    warning("Invalid model/forecast object provided (NULL).")
    return(NULL)
  }

  is_ets_model <- inherits(model_or_fcst, "ets")
  is_stlf_fcst <- inherits(model_or_fcst, "forecast") # stlf returns this class

  if (!is_ets_model && !is_stlf_fcst) {
    message("Starting forecast_ets - Invalid object class") # Added message
    warning("Input object is not class 'ets' or 'forecast' (from stlf).")
    return(NULL)
  }
  if (!is.numeric(total_periods_needed) || total_periods_needed < 1) {
    warning("Invalid total_periods_needed provided. Returning NULL.")
    return(NULL)
  }
  if (!is.Date(train_end_date)) {
    warning("Invalid train_end_date provided. Returning NULL.")
    return(NULL)
  }
  message(paste("Input object class:", class(model_or_fcst))) # Added message
  

  fcst <- NULL
  fitted_vals <- NULL
  fcst_df <- NULL

  # tryCatch({
  #   message("Calling forecast() for ETS/STLM object...")
  #   # Use forecast generic, works for both ets and stlm objects
  #   fcst <- forecast::forecast(model, h = total_periods_needed, level = c(80, 95))
  #   # Fitted values from forecast object should be on original scale for both
  #   fitted_vals <- stats::fitted(fcst)
  #   message("forecast() finished.")
  # }, error = function(e) {
  #   warning(paste("ETS/STLM forecast generation failed:", conditionMessage(e)))
  #   fcst <<- NULL; fitted_vals <<- NULL
  # })


  # fcst <- NULL
  # tryCatch({
  #   fcst <- forecast::forecast(model, h = total_periods_needed, level = c(80, 95))
  #   message("forecast.ets() finished.")
  # }, error = function(e) {
  #   warning(paste("ETS forecast generation failed:", conditionMessage(e)))
  #   fcst <<- NULL
  # })

  # if(is.null(fcst)) return(NULL)

  # Convert forecast object to a tibble with dates
  # fcst_df <- NULL
  # tryCatch({
  #   by_period <- switch(freq_str, "week" = lubridate::weeks(1), lubridate::days(1))
  #   start_forecast_date <- train_end_date + by_period
  #   forecast_dates <- seq.Date(from = start_forecast_date, by = freq_str, length.out = total_periods_needed)
  #
  #   if(length(forecast_dates) != length(fcst$mean)) {
  #     stop("Generated forecast dates length does not match forecast periods.")
  #   }
  #
  #   fcst_df <- tibble::tibble(
  #     ds = forecast_dates,
  #     yhat = as.numeric(fcst$mean),
  #     yhat_lower_80 = as.numeric(fcst$lower[, 1]),
  #     yhat_upper_80 = as.numeric(fcst$upper[, 1]),
  #     yhat_lower_95 = as.numeric(fcst$lower[, 2]),
  #     yhat_upper_95 = as.numeric(fcst$upper[, 2])
  #   )
  #   message("ETS forecast tibble created.")
  #
  # }, error = function(e) {
  #   warning(paste("Failed to convert ETS forecast to tibble:", conditionMessage(e)))
  #   fcst_df <<- NULL # Ensure it's NULL on error
  # })
  #
  # # Return list similar to forecast_arima
  # return(list(
  #   forecast = fcst_df,
  #   fitted = fitted_vals # Fitted values from the forecast object
  # ))
  # ))
  tryCatch({
    if (is_ets_model) {
      # Forecast the ETS model if that's what we have
      message("Calling forecast() on ETS object...")
      fcst <- forecast::forecast(model_or_fcst, h = total_periods_needed, level = c(80, 95))
      fitted_vals <- stats::fitted(fcst)
      message("forecast() for ETS finished.")
    } else {
      # If we have an stlf forecast object, it already contains the forecast
      message("Input is forecast object (from stlf). Extracting forecast/fitted values.")
      fcst <- model_or_fcst # The object *is* the forecast
      # Check if forecast horizon matches needed periods
      # stlf might have been called with h=total_periods_needed already, or maybe not?
      # Need to ensure stlf inside train_ets is called with correct horizon 'h'
      # --- Correction Needed in train_ets for stlf horizon ---
      # Let's assume train_ets passes correct h to stlf for now.
      if (length(fcst$mean) != total_periods_needed) {
        warning(paste("STLF forecast object has length", length(fcst$mean),
                      "but needed", total_periods_needed, "periods. Refitting/forecasting within forecast_ets."))
        # Option: Refit/reforecast here (complex) or ensure train_ets passes h
        # Let's try ensuring train_ets passes h to stlf.
        # If still mismatch, error out for now.
        stop(paste("STLF forecast horizon mismatch. Expected", total_periods_needed, "got", length(fcst$mean)))
      }

    }

    if(is.null(fcst)) stop("Forecast object generation failed.")
    message(paste("forecast_ets: About to call stats::fitted() on object of class:", class(fcst))) # Added message

    # Extract fitted values (works for both forecast objects from ets/stlf)
    fitted_vals <- stats::fitted(fcst)
    if(is.null(fitted_vals)) stop("Could not extract fitted values.")

    # Convert forecast object (fcst) to tibble with dates
    message("Converting forecast object to tibble...")
    by_period <- switch(freq_str, "week" = lubridate::weeks(1), lubridate::days(1))
    start_forecast_date <- train_end_date + by_period
    forecast_dates <- seq.Date(from = start_forecast_date, by = freq_str, length.out = total_periods_needed)

    if(length(forecast_dates) != length(fcst$mean)) stop("Generated dates length mismatches forecast length.")

    fcst_df <- tibble::tibble(
      ds = forecast_dates,
      yhat = as.numeric(fcst$mean),
      yhat_lower_80 = as.numeric(fcst$lower[, 1]),
      yhat_upper_80 = as.numeric(fcst$upper[, 1]),
      yhat_lower_95 = as.numeric(fcst$lower[, 2]),
      yhat_upper_95 = as.numeric(fcst$upper[, 2])
    )
    message("Forecast tibble created.")

  }, error = function(e) {
    warning(paste("Failed during ETS/STLF forecast processing/conversion:", conditionMessage(e)))
    fcst_df <<- NULL; fitted_vals <<- NULL # Reset on error
  })

  message(paste("forecast_ets: Length of fitted values:", length(fitted_vals))) # Added message
  message(paste("forecast_ets: Any NAs in fitted values?", anyNA(fitted_vals))) # Added message

  message("Finished forecast_ets")

  # Return list
  return(list(forecast = fcst_df, fitted = fitted_vals))
}


#' Train TBATS Model
#'
#' Trains a TBATS (Trigonometric Box-Cox transform, ARMA errors, Trend, and Seasonal components)
#' model using `forecast::tbats`. This model is suitable for complex time series with
#' multiple seasonalities.
#'
#' @param train_df A tibble with 'ds' (Date) and 'y' (numeric) columns representing the training data.
#' @param config A list for potential future TBATS configurations (currently unused).
#'   The function currently uses automatic model selection within `forecast::tbats`.
#' @param aggregation_level Character string. Indicates data frequency ("Daily" or "Weekly").
#'   This influences the primary `frequency` of the `ts` object (7 for "Daily", 52 for "Weekly")
#'   and can provide a hint for the `seasonal.periods` argument in `forecast::tbats`
#'   (e.g., `c(7, 365.25)` for daily data if series is long enough).
#'
#' @return A fitted TBATS model object (class `tbats` from the `forecast` package).
#'   Returns `NULL` on error (e.g., insufficient data).
#'
#' @noRd
#' @import forecast dplyr lubridate stats
train_tbats <- function(train_df, config, aggregation_level) { # config currently unused
  message("Starting train_tbats")
  # Basic validation
  if (!is.data.frame(train_df) || !all(c("ds", "y") %in% names(train_df))) {
    stop("train_df must be a dataframe with 'ds' and 'y' columns.")
  }
  if(nrow(train_df) < 5) { # TBATS likely needs more data than basic ETS
    warning("TBATS training requires sufficient data points. Returning NULL.")
    return(NULL)
  }
  if(!aggregation_level %in% c("Daily", "Weekly")) {
    warning("Unknown aggregation level provided to train_tbats.")
    aggregation_level <- "Unknown"
  }

  # --- Determine Frequency (Period) ---
  # Pass primary frequency to tbats to potentially guide seasonal period detection
  freq <- 1
  seasonal_periods_hint <- NULL
  if (aggregation_level == "Daily") {
    freq <- 7
    # Only hint yearly seasonality if enough data (e.g., > 2 years)
    if (nrow(train_df) > 2 * 365) {
      seasonal_periods_hint <- c(7, 365.25) # Hint for weekly and yearly seasonality
      message("TBATS: Setting ts frequency=7 for daily data. Hinting seasonal_periods: c(7, 365.25)")
    } else {
      seasonal_periods_hint <- c(7) # Only hint weekly
      message("TBATS: Setting ts frequency=7 for daily data (short series). Hinting seasonal_periods: c(7)")
    }
  } else if (aggregation_level == "Weekly") {
    freq <- 52
    seasonal_periods_hint <- c(365.25 / 7) # Hint for yearly seasonality
    message("TBATS: Setting ts frequency=52 for weekly data. Hinting seasonal_periods: c(52.17857)")
  } else {
    message("TBATS: Frequency=1 (non-seasonal or unknown aggregation). seasonal_periods_hint remains NULL.")
  }
  # --- End Frequency Determination ---

  # Check for sufficient data points for the frequency
  if (freq > 1 && nrow(train_df) < 3 * freq) { # TBATS might need more data (e.g., 3 cycles)
    warning(paste("Potentially insufficient data (", nrow(train_df), ") for frequency =", freq," for TBATS. Need >= 3*freq ideally. Attempting fit..."))
    # Don't revert freq, let tbats try and fail if needed
  }

  # Create time series object
  y_ts <- NULL
  tryCatch({ # ... create ts object using freq ...
    start_date <- min(train_df$ds); start_year <- lubridate::year(start_date)
    ts_start <- if (freq > 1) {
      day_in_cycle <- switch(as.character(freq),
                             "7" = lubridate::wday(start_date, week_start = 1),
                             "52" = lubridate::week(start_date),
                             floor(lubridate::yday(start_date) / (365.25 / freq)) + 1)
      c(start_year, day_in_cycle)
    } else { start_year }
    y_ts <- stats::ts(train_df$y, frequency = freq, start = ts_start)
    }, error = function(e){
      warning(paste("Failed to create ts object for TBATS:", conditionMessage(e)))
      return(NULL) # Return NULL if ts creation fails
    })
  if(is.null(y_ts)) return(NULL)
 message("TS object created successfully.") # Added message
  message(paste("Created ts object with frequency:", stats::frequency(y_ts)))

  # --- Train TBATS ---
  model <- NULL
  tryCatch({
 message("Calling forecast::tbats() for training...") # Updated message
    # Using default arguments for automatic detection:
    # use.box.cox=NULL, use.trend=NULL, use.damped.trend=NULL,
 # use.parallel=TRUE, # Set to TRUE in call below
    # seasonal.periods=NULL (auto-detect based on ts frequency & data)
    # Pass seasonal_periods_hint if not NULL
    tbats_args <- list(y = y_ts, use.parallel = TRUE)
    if (!is.null(seasonal_periods_hint)) {
      tbats_args$seasonal.periods <- seasonal_periods_hint
    }
    message("Arguments for tbats():")
    print(str(tbats_args))

    model <- tryCatch({
      do.call(forecast::tbats, args = tbats_args)
    }, error = function(e_inner) { # Changed error variable name
      # Simplified warning message to avoid potential scoping issues with seasonal_periods_hint
      warning(paste0("Inner tbats() call failed. Error: ", conditionMessage(e_inner),
                    ". Data rows: ", nrow(train_df),
                    ", TS Freq: ", stats::frequency(y_ts)))
      return(NULL) # Return NULL from inner tryCatch
    })

    if (is.null(model)) {
      message("TBATS: forecast::tbats() call resulted in NULL. Model fitting failed.")
    } else if (!inherits(model, "tbats")) {
      warning("TBATS: forecast::tbats() returned an invalid object (not class 'tbats'). Model fitting failed.")
      model <- NULL # Ensure model is NULL if not a valid tbats object
    } else {
      message("TBATS: forecast::tbats() call finished successfully.")
      message("TBATS model fitted successfully. Method: ", model$method)
    }

  }, error = function(e_outer) { # Outer tryCatch
    warning(paste("Outer TBATS model training tryCatch failed:", conditionMessage(e_outer)))
    model <<- NULL
  })

  message("Finished train_tbats")
  return(model)
  }


#' Forecast using TBATS Model
#'
#' Generates forecasts from a trained TBATS model.
#'
#' @param model A fitted `tbats` model object from `train_tbats`.
#' @param total_periods_needed Integer. The total number of periods for which the forecast is required.
#' @param train_end_date Date. The last date of the training data, used to generate the
#'   date sequence for the forecast tibble.
#' @param freq_str Character string. The frequency of the time series ('day', 'week'),
#'   used for generating the date sequence.
#'
#' @return A list containing two elements:
#'   \itemize{
#'     \item `forecast`: A tibble with columns 'ds' (Date), 'yhat' (point forecast),
#'           'yhat_lower_80', 'yhat_upper_80', 'yhat_lower_95', 'yhat_upper_95' (confidence intervals).
#'     \item `fitted`: A numeric vector of fitted values from the model.
#'   }
#'   Returns `NULL` if `model` is invalid or an error occurs during processing.
#'
#' @noRd
#' @import forecast dplyr tibble lubridate stats
forecast_tbats <- function(model, total_periods_needed, train_end_date, freq_str = "day") {
  # Basic validation
  message("Starting forecast_tbats")
  if (is.null(model) || !inherits(model, "tbats")) {
    message("Starting forecast_tbats - Invalid model (NULL or wrong class)") # Added message
    warning("Invalid TBATS model object provided. Returning NULL.")
    return(NULL)
  } else {
    message(paste("Input object class:", class(model))) # Added message
  }
  # ... rest of validation (periods_needed, train_end_date)...

  if (!is.numeric(total_periods_needed) || total_periods_needed < 1) {
    warning("Invalid total_periods_needed provided. Returning NULL.")
    return(NULL)
  }
  if (!is.Date(train_end_date)) {
    warning("Invalid train_end_date provided. Returning NULL.")
    return(NULL)
  }


  fcst <- NULL
  fitted_vals <- NULL
  fcst_df <- NULL

  if (is.null(model)) { # Double check after initial validation
    warning("Model is NULL inside forecast_tbats tryCatch.")
    return(NULL)
  }

  tryCatch({
    message("Calling forecast() on TBATS object...")
    fcst <- forecast::forecast(model, h = total_periods_needed, level = c(80, 95)) # Generate forecast
    if(is.null(fcst)) { # Check if forecast call itself returned NULL
      stop("forecast::forecast(model, h = total_periods_needed, ...) returned NULL")
    }
    fitted_vals <- stats::fitted(fcst) # Fitted values from forecast object, AFTER fcst is created
    message("forecast.tbats() finished.")
  }, error = function(e) {
    warning(paste("TBATS forecast generation failed:", conditionMessage(e)))
    fcst <<- NULL; fitted_vals <<- NULL # Ensure both are NULL on error
  })

  if(is.null(fcst)) { # This check is now more robust
    warning("TBATS forecast generation failed, fcst object is NULL.") # Added warning
    return(NULL)
  }
  message(paste("forecast_tbats: About to call stats::fitted() on object of class:", class(fcst))) # Added message

  # Convert forecast object to tibble with dates
  tryCatch({
    # ... generate forecast_dates using train_end_date, freq_str, total_periods_needed ...
    message("Converting forecast object to tibble...")
    by_period <- switch(freq_str, "week" = lubridate::weeks(1), lubridate::days(1))
    start_forecast_date <- train_end_date + by_period
    forecast_dates <- seq.Date(from = start_forecast_date, by = freq_str, length.out = total_periods_needed)

    if(length(forecast_dates) != length(fcst$mean)) stop("Generated dates length mismatches forecast length.")

    fcst_df <- tibble::tibble( # Create tibble from fcst$mean, fcst$lower, fcst$upper
      ds = forecast_dates,
      yhat = as.numeric(fcst$mean),
      yhat_lower_80 = as.numeric(fcst$lower[, 1]),
      yhat_upper_80 = as.numeric(fcst$upper[, 1]),
      yhat_lower_95 = as.numeric(fcst$lower[, 2]),
      yhat_upper_95 = as.numeric(fcst$upper[, 2])
    )
    message("TBATS forecast tibble created.")

  }, error = function(e) {
    warning(paste("Failed to convert TBATS forecast to tibble:", e$message))
    fcst_df <<- NULL; fitted_vals <<- NULL # Reset on error
  })

  message(paste("forecast_tbats: Length of fitted values:", length(fitted_vals))) # Added message
  message(paste("forecast_tbats: Any NAs in fitted values?", anyNA(fitted_vals))) # Added message

  # Return list
  message("Finished forecast_tbats")
  return(list(forecast = fcst_df, fitted = fitted_vals))
}


#' Train Prophet Model
#'
#' Trains a Prophet model using `prophet::prophet` and `prophet::fit.prophet`.
#' The model can incorporate custom seasonalities, holidays, and external regressors.
#'
#' @param train_df A tibble with 'ds' (Date) and 'y' (numeric) columns.
#'   If `config$growth` is 'logistic', `train_df` must also contain a 'cap' column
#'   representing the carrying capacity.
#' @param config A list containing Prophet parameters:
#'   \itemize{
#'     \item `yearly.seasonality`: Logical or character ('auto', TRUE, FALSE).
#'     \item `weekly.seasonality`: Logical or character ('auto', TRUE, FALSE).
#'     \item `daily.seasonality`: Logical or character ('auto', TRUE, FALSE).
#'     \item `growth`: Character. 'linear' or 'logistic'.
#'     \item `changepoint.prior.scale`: Numeric. Parameter modulating the flexibility of the automatic changepoint selection.
#'   }
#' @param holidays_df Optional. A tibble for specifying holidays, with columns 'holiday' (character/factor, name of the holiday)
#'   and 'ds' (Date). Can optionally include 'lower_window' and 'upper_window' to extend holiday effects.
#' @param regressors_df Optional. A tibble for external regressors. Must include 'ds' (Date) and columns for each regressor
#'   named in `regressor_names`. These regressor columns must be present for all training dates.
#' @param regressor_names Optional. A character vector of column names from `regressors_df` to be used as external regressors.
#'   Required if `regressors_df` is provided.
#'
#' @return A fitted Prophet model object (class `prophet`). Returns `NULL` on error.
#'
#' @noRd
#'
#' @import prophet dplyr
train_prophet <- function(train_df, config, holidays_df = NULL, regressors_df = NULL, regressor_names = NULL) {
  # Basic validation
  if (!is.data.frame(train_df) || !all(c("ds", "y") %in% names(train_df))) {
    stop("train_df must be a dataframe with 'ds' and 'y' columns.")
  }
  if(nrow(train_df) < 2) {
    warning("Prophet training requires at least 2 data points. Returning NULL.")
    return(NULL)
  }
  # Check for 'cap' column if growth is logistic
  if (config$growth == 'logistic' && !"cap" %in% names(train_df)) {
    stop("Logistic growth requires a 'cap' column in train_df.")
  }

  # Validate holidays_df if provided
  if (!is.null(holidays_df)) {
    req(is.data.frame(holidays_df), all(c("holiday", "ds") %in% names(holidays_df)))
  }
  # Validate regressors_df and names if provided
  add_regressors_flag <- FALSE
  if (!is.null(regressors_df)) {
    req(is.data.frame(regressors_df), "ds" %in% names(regressors_df))
    req(!is.null(regressor_names), all(regressor_names %in% names(regressors_df)))
    add_regressors_flag <- TRUE
    # Join regressors to training data - MUST contain training dates
    train_df <- train_df %>%
      dplyr::inner_join(regressors_df %>% select(ds, all_of(regressor_names)), by = "ds")
    validate(need(nrow(train_df) > 0, "No matching dates found between train data and regressors."))
  }

  model_fit <- NULL
  tryCatch({
    # Initialize model
    m <- prophet::prophet(
      yearly.seasonality = config$yearly,
      weekly.seasonality = config$weekly,
      daily.seasonality = config$daily,
      growth = config$growth,
      changepoint.prior.scale = config$changepoint_scale,
      holidays = holidays_df # Pass holidays df to prophet constructor
      # Can add holidays etc. here if needed later
    )

    if (add_regressors_flag) {
      for (reg_name in regressor_names) {
        m <- prophet::add_regressor(m, name = reg_name) # Add PRIOR to fitting
      }
    }
    # Fit model
    model_fit <- prophet::fit.prophet(m, train_df)

  }, error = function(e) {
    warning(paste("Prophet model training failed:", e$message))
    model_fit <<- NULL
  })

  return(model_fit)
}

#' Forecast using Prophet Model
#'
#' Generates forecasts from a trained Prophet model using `prophet::make_future_dataframe`
#' and `predict`.
#'
#' @param model A fitted `prophet` model object from `train_prophet`.
#' @param periods_to_generate Integer. The total number of future periods to generate,
#'   starting from the day after the last date in the training data. This should cover
#'   any test/validation set and the desired future forecast horizon.
#' @param freq Character string. The frequency for generating future dates
#'   (e.g., 'day', 'week', 'month'). Passed to `prophet::make_future_dataframe`.
#' @param capacity Optional numeric. The carrying capacity value for logistic growth forecasts.
#'   Required if the model was trained with `growth = 'logistic'`. This value should be
#'   provided for all dates in `future_df` (historical and future).
#' @param regressors_df Optional. A tibble containing *future* values for any external regressors
#'   used during model training. Must include 'ds' (Date) and all regressor columns specified
#'   in `regressor_names`. These values must cover the entire forecast horizon.
#' @param regressor_names Optional. Character vector of regressor names. Must match those
#'   used in training if `regressors_df` is provided.
#'
#' @return A tibble with columns: 'ds' (Date), 'yhat' (point forecast),
#'   'yhat_lower' (lower bound of uncertainty interval), and 'yhat_upper' (upper bound).
#'   Prophet's default uncertainty intervals are typically 80%.
#'   Returns `NULL` on error.
#'
#' @noRd
#'
#' @import prophet dplyr tibble lubridate
forecast_prophet <- function(model, periods_to_generate, freq = "day", capacity = NULL,
                             regressors_df = NULL, regressor_names = NULL) { # Renamed horizon
  if (is.null(model) || !inherits(model, "prophet")) {
    warning("Invalid Prophet model object provided. Returning NULL.")
    return(NULL)
  }
  if (!is.numeric(periods_to_generate) || periods_to_generate < 1) { # Check new param name
    warning("Invalid periods_to_generate provided. Returning NULL.")
    return(NULL)
  }
  # Check if capacity is needed
  if (model$growth == 'logistic' && is.null(capacity)) {
    warning("Capacity value must be provided for logistic growth forecast. Returning NULL.")
    return(NULL)
  }
  # Validate regressors if model was trained with them
  needs_regressors <- length(model$extra_regressors) > 0
  if (needs_regressors && is.null(regressors_df)) {
    stop("Model was trained with regressors, but no future regressor values provided.")
  }
  if (needs_regressors && !is.null(regressors_df)) {
    req(is.data.frame(regressors_df), "ds" %in% names(regressors_df))
    req(!is.null(regressor_names), all(regressor_names %in% names(regressors_df)))
    # Check if regressor names match model's regressors
    req(all(regressor_names %in% names(model$extra_regressors)))
  }

  fcst_df <- NULL
  tryCatch({
    # Create future dataframe using the total periods needed
    future_df <- prophet::make_future_dataframe(
      model,
      periods = periods_to_generate, # Use the new argument here
      freq = freq
    )
    # Add capacity if logistic growth
    if (model$growth == 'logistic') { future_df$cap <- capacity }
    # --- Join Future Regressors ---
    if (needs_regressors) {
      # Select only needed columns (ds + specified regressors)
      future_regressors <- regressors_df %>% select(ds, all_of(regressor_names))
      # Join future dates with future regressor values
      future_df <- future_df %>% dplyr::left_join(future_regressors, by = "ds")
      # Check if join resulted in NAs for regressors in forecast period
      if(any(is.na(future_df %>% dplyr::filter(ds > model$history.dates[length(model$history.dates)]) %>% select(all_of(regressor_names))))){
        stop("Future regressor values are missing for some dates in the forecast horizon.")
      }
    }
    # --- End Join ---

    # Predict
    forecast_result <- predict(model, future_df)

    # Select and rename columns, ensure ds is Date
    fcst_df <- forecast_result %>%
      dplyr::select(ds, yhat, yhat_lower, yhat_upper) %>%
      dplyr::mutate(ds = lubridate::as_date(ds)) # Ensure ds is Date type here


  }, error = function(e) {
    warning(paste("Prophet forecast generation failed:", e$message))
    fcst_df <<- NULL
  })

  return(fcst_df)
}


#' Train XGBoost Model
#'
#' Trains an XGBoost model using a *prepared* `recipes::recipe` object.
#' The function extracts the outcome (`y`) and predictors (`train_x_df`) from the
#' prepared recipe, converts them to the `xgb.DMatrix` format, and then trains
#' the model using `xgboost::xgb.train`.
#'
#' @param prep_recipe A *prepared* `recipes::recipe` object. This recipe should have
#'   been prepared (i.e., `recipes::prep()` called) on the training data. It defines
#'   all feature engineering steps (e.g., lags, rolling windows, date components, dummy variables).
#' @param config A list containing XGBoost hyperparameters:
#'   \itemize{
#'     \item `nrounds`: Integer. Number of boosting rounds.
#'     \item `eta`: Numeric. Learning rate (shrinkage).
#'     \item `max_depth`: Integer. Maximum depth of a tree.
#'     \item `subsample`: Numeric. Subsample ratio of the training instances.
#'     \item `colsample_bytree`: Numeric. Subsample ratio of columns when constructing each tree.
#'     \item `gamma`: Numeric. Minimum loss reduction required to make a further partition on a leaf node.
#'   }
#'
#' @return A fitted XGBoost model object (class `xgb.Booster`). Returns `NULL` on error.
#'
#' @noRd
#'
#' @import xgboost recipes dplyr
train_xgboost <- function(prep_recipe, config) {

  if (is.null(prep_recipe) || !inherits(prep_recipe, "recipe")) {
    warning("train_xgboost received NULL or invalid recipe object.")
    return(NULL)
  }
  # Check if recipe looks prepared (has steps, train_info)
  if(is.null(prep_recipe$steps) || is.null(prep_recipe$term_info)) warning("train_xgboost received a recipe that might not be prepared.")

  model <- NULL
  train_y <- NULL # Initialize
  tryCatch({

    # Explicitly select the outcome column 'y'
    juiced_outcome_tibble <- recipes::juice(prep_recipe, y) # Select 'y' by name

    if (is.null(juiced_outcome_tibble)) stop("Juicing the outcome ('y') returned NULL.")
    if (nrow(juiced_outcome_tibble) == 0) stop("Juicing the outcome ('y') returned zero rows.")
    if (!"y" %in% names(juiced_outcome_tibble)) stop("Juiced outcome tibble does not contain the 'y' column after selecting by name.")

    train_y <- juiced_outcome_tibble %>% dplyr::pull(y)
    train_x_df <- recipes::bake(prep_recipe, new_data = NULL, has_role("predictor"))

    if(nrow(train_x_df) == 0 || ncol(train_x_df) == 0) stop(paste("Baked predictor data frame has zero rows or columns. Dims:", paste(dim(train_x_df), collapse=" x ")))

    # Ensure no non-numeric columns remain (safety check)
    is_numeric_col <- sapply(train_x_df, is.numeric)
    if(!all(is_numeric_col)){
      warning("Non-numeric columns found after baking! Selecting only numeric.")
      train_x_df <- train_x_df[, is_numeric_col, drop=FALSE]
    }

    # Check Dimensions before matrix conversion
    if(nrow(train_x_df) != length(train_y)) stop(paste("Dimension mismatch AFTER processing: Predictors rows =", nrow(train_x_df), "vs Outcome length =", length(train_y)))

    train_x <- as.matrix(train_x_df)

    message("train_xgboost 1")

    # Create DMatrix
    dtrain <- xgboost::xgb.DMatrix(data = train_x, label = train_y)

    # Define parameters
    parameters <- list(
      objective = "reg:squarederror", # for regression
      eta = config$eta,
      max_depth = config$max_depth,
      subsample = config$subsample,
      colsample_bytree = config$colsample,
      gamma = config$gamma
      # Add other params like lambda, alpha if needed
    )

    # --- XGBoost Train Debug ---
    message("--- XGBoost Train: Debug ---")
    message("Structure of dtrain (xgb.DMatrix):")
    print(str(dtrain))
    message("Parameters passed to xgb.train:")
    print(str(parameters))
    message("--- End XGBoost Train: Debug ---")

    # Train the model within a tryCatch
    model <- tryCatch({
        xgboost::xgb.train(
          params = parameters,
          data = dtrain,
          nrounds = config$nrounds,
          verbose = 1 # Set to 1 or 2 for training progress messages
        )
      }, error = function(e_xgb) {
        warning(paste("Error specifically during xgb.train call:", conditionMessage(e_xgb)))
        print("--- xgb.train Error Object ---")
        print(e_xgb)
        print("--- End xgb.train Error Object ---")
        return(NULL) # Return NULL if xgb.train fails
      })

    if(is.null(model)) {
      stop("xgb.train failed or returned NULL.") # Stop execution if training failed
    }

    message("train_xgboost 2: Model training supposedly finished.")

    # --- Add Prediction Test on Training Data ---
    message("--- XGBoost Train: Testing predict() on training matrix ---")
    # --- Sanity check train_x before prediction test ---
    if (!is.matrix(train_x)) {
      warning("train_x is not a matrix before prediction test!")
    } else if (!is.numeric(train_x)) {
      warning("train_x is not numeric before prediction test!")
    } else if (anyNA(train_x)) {
      warning("NAs found in train_x before prediction test!")
    } else if (any(!is.finite(train_x))) {
      warning("Non-finite values (Inf/-Inf) found in train_x before prediction test!")
    } else {
      message("train_x checks passed before prediction test.")
    }
    # --- End Sanity check ---
    tryCatch({
      # Explicitly set ntreelimit to default NULL
      train_preds <- predict(model, train_x, ntreelimit = NULL)
      message(paste("Successfully predicted on training matrix. Length:", length(train_preds)))
    }, error = function(e_pred_test) {
      warning(paste("Error when trying to predict on TRAINING matrix:", conditionMessage(e_pred_test)))
      print("--- predict(model, train_x) Error Object ---")
      print(e_pred_test)
      print("--- End predict(model, train_x) Error Object ---")
    })
    message("--- End XGBoost Train: Prediction Test ---")
    # --- End Prediction Test ---


  }, error = function(e) {
    message(paste("XGBoost model training failed inside train_xgboost:", conditionMessage(e)))
    # Print the error caught by *this* tryCatch
    print("--- train_xgboost Error Object ---")
    print(e) # Print the specific error object from this context
    print("--- End train_xgboost Error Object ---")
    model <<- NULL
  })

  return(model)
}


#' Forecast using XGBoost Model
#' @describeIn forecast_xgboost Generates forecasts from a trained XGBoost model.
#' The function prepares future data by creating lag and window features based on the
#' provided recipe, then predicts using the trained `xgb.Booster` model.
#'
#' @param model A fitted `xgb.Booster` object from `train_xgboost` or a similar process
#'   (e.g., direct `xgboost::xgb.train` or from a `parsnip` fit object that has been finalized
#'   and the underlying `xgb.Booster` extracted).
#' @param prep_recipe The *prepared* `recipes::recipe` object that was used for training the model.
#'   This recipe defines how features (like lags, rolling window statistics, date components) are created.
#' @param full_df The original, complete dataframe (post initial cleaning and aggregation, but pre-train/test split)
#'   containing 'ds' (Date) and 'y' (numeric value) columns. This dataframe is used as the historical basis
#'   to generate features (especially lags and rolling window features) for the future points to be forecasted.
#' @param train_end_date The last date of the training set. This is used as a reference point to start
#'   generating the sequence of future dates for which forecasts are required.
#' @param total_periods_needed Integer. The total number of periods to forecast ahead. This should typically
#'   cover any test/validation set periods plus the actual future horizon desired.
#' @param freq Character string indicating the frequency of the time series (e.g., "day", "week").
#'   This is used for generating the sequence of future dates.
#'
#' @return A tibble with 'ds' (Date) and 'yhat' (numeric forecast) columns.
#'   Returns `NULL` on error (e.g., if feature generation fails or prediction errors occur).
#'   Standard XGBoost models do not inherently produce prediction intervals; thus, the output
#'   does not include confidence interval columns (e.g., `yhat_lower_95`, `yhat_upper_95`).
#'
#' @details
#' The forecasting process involves several steps:
#' \enumerate{
#'   \item Determination of `max_lag_needed` from the `prep_recipe` to ensure sufficient historical data is used.
#'   \item Creation of a `future_dates` sequence starting from `train_end_date + 1 period`.
#'   \item Construction of a `combined_df` by appending a `future_template` (with `NA` for `y`) to `recent_data` (tail of `full_df`).
#'   \item Baking the `prep_recipe` with `combined_df` to generate features for both historical and future points.
#'   \item Extraction of `future_features_baked` rows corresponding to the `future_dates`.
#'   \item Alignment of these baked features with the feature names expected by the `model` (from `model$feature_names`).
#'   \item Prediction using `predict(model, future_matrix)`.
#' }
#'
#' @noRd
#'
#' @import xgboost recipes dplyr tibble lubridate utils
forecast_xgboost <- function(model, prep_recipe, full_df, train_end_date, total_periods_needed, freq = "day") {

  if (is.null(model) || !inherits(model, "xgb.Booster")) {
    warning("Invalid XGBoost model object provided. Returning NULL.")
    return(NULL)
  }
  if (is.null(prep_recipe) || !inherits(prep_recipe, "recipe")) {
    warning("Invalid recipe object provided. Returning NULL.")
    return(NULL)
  }
  if (!is.data.frame(full_df) || !all(c("ds", "y") %in% names(full_df))) {
    stop("full_df must be a dataframe with 'ds' and 'y' columns.")
  }
  if (!is.numeric(total_periods_needed) || total_periods_needed < 1) {
    warning("Invalid horizon provided. Returning NULL.")
    return(NULL)
  }

  fcst_df <- NULL
  tryCatch({
    # 1. Determine max lag from recipe steps (approximation)
    max_lag_needed <- 0
    for (step in prep_recipe$steps) {
      if (inherits(step, "step_lag")) {
        max_lag_needed <- max(max_lag_needed, max(step$lag))
      }
      # Add checks for window steps if they were implemented more complexly
    }

    # Ensure enough historical data is available
    if(nrow(full_df) < max_lag_needed) {
      stop(paste("Need at least", max_lag_needed, "rows in full_df for forecast lags."))
    }

    # 2. Create future dates sequence
    last_actual_date <- max(full_df$ds)
    by_period <- switch(freq,
                        # "day" = lubridate::days(1),
                        "week" = lubridate::weeks(1),
                        # "month" = lubridate::months(1),
                        # "quarter" = lubridate::months(3),
                        # "year" = lubridate::years(1),
                        lubridate::days(1) # Default to day
    )
    start_forecast_date <- train_end_date + by_period
    future_dates <- seq.Date(from = start_forecast_date, by = freq, length.out = total_periods_needed)
    # if(length(future_dates) > 0) { # Add check and print range
    #   message(paste("Generated dates range:", min(future_dates), "to", max(future_dates)))
    # } else { stop("Generated future_dates has length 0.")}
    # if(length(future_dates) != total_periods_needed) { stop(...) }



    # 3. Create template for future data features
    # Need recent actual data + future dates template
    recent_data <- utils::tail(full_df, max_lag_needed) # Get last N rows needed for lags
    future_template <- tibble::tibble(ds = future_dates, y = NA_real_) # y is unknown

    # Combine for baking (order matters: history first, then future)
    combined_df <- dplyr::bind_rows(recent_data, future_template)

    # 4. Bake the recipe using combined data to get features for future dates
    baked_combined <- recipes::bake(prep_recipe, new_data = combined_df, all_predictors())

    # 5. Extract feature rows corresponding *only* to the horizon dates
    # Match based on the 'ds' column created by time series signature (if kept) or index
    # Assuming 'bake' keeps rows in order:
    future_features_baked <- utils::tail(baked_combined, total_periods_needed)
    if(nrow(future_features_baked) != total_periods_needed) stop("Baked feature rows do not match total periods needed.")


    # Ensure no non-numeric columns remain (should be handled by recipe/bake)
    is_numeric_col_fcst <- sapply(future_features_baked, is.numeric)
    if(!all(is_numeric_col_fcst)){
      warning("Non-numeric columns found after baking recipe for XGBoost forecast.")
      future_features_baked <- future_features_baked[, is_numeric_col_fcst, drop=FALSE]
    }

    # 6. Convert to matrix (ensure same column order as training)
    # Get feature names from the booster object (safer)
    model_features <- model$feature_names
    message("--- XGBoost Forecast: Debug ---")
    message("Names of baked future features (future_features_baked):")
    print(names(future_features_baked))
    message("Feature names from trained XGBoost model (model$feature_names):")
    print(model_features)
    message("--- End XGBoost Forecast: Debug ---")

    # Ensure baked features have all model features (might need imputation if bake drops some?)
    missing_cols <- setdiff(model_features, names(future_features_baked))
    if (length(missing_cols) > 0) {
      stop(paste("Features missing after baking for forecast:", paste(missing_cols, collapse=", ")))
    }
    selected_future_features_df <- future_features_baked[, model_features, drop = FALSE]

    # Ensure all selected columns are numeric before converting to matrix
    are_numeric_check <- sapply(selected_future_features_df, is.numeric)
    if (!all(are_numeric_check)) {
        warning("XGBoost Forecast: Not all selected model_features are numeric in baked future data. Coercing non-numeric columns to numeric. This might indicate an issue with the recipe's consistency for future data or an unexpected column type.")
        for (col_name in names(selected_future_features_df)[!are_numeric_check]) {
            # Attempt direct coercion, NAs will be introduced if not directly coercible
            original_class <- class(selected_future_features_df[[col_name]])
            selected_future_features_df[[col_name]] <- as.numeric(selected_future_features_df[[col_name]])
            new_class <- class(selected_future_features_df[[col_name]])
            message(paste0("Coerced column '", col_name, "' from '", original_class, "' to '", new_class, "'."))
        }
    }

    future_matrix_raw <- as.matrix(selected_future_features_df)

    # Ensure the matrix is purely numeric before prediction.
    # If future_matrix_raw was already numeric, this step is just a reassignment.
    # If future_matrix_raw became character (e.g. as.matrix on mixed types), this converts to numeric NAs.
    if (!is.numeric(future_matrix_raw)) {
        message("XGBoost Forecast: future_matrix_raw is not numeric. Applying as.numeric to columns.")
        future_matrix <- apply(future_matrix_raw, 2, as.numeric)
        # Check if NAs were introduced by 'apply' that weren't in 'future_matrix_raw' (e.g. char -> NA)
        # This condition might be tricky if future_matrix_raw itself could have NAs.
        # A simpler check: if as.numeric was needed and resulted in NAs.
        if(anyNA(future_matrix) && !all(sapply(selected_future_features_df, function(col) all(is.na(col) | is.numeric(col)))) ) {
           warning("XGBoost Forecast: NAs potentially introduced or already present after ensuring numeric matrix. Review recipe steps if unexpected.")
        }
    } else {
        future_matrix <- future_matrix_raw # Already numeric
        message("XGBoost Forecast: future_matrix_raw is already numeric.")
    }
    
    # Final check for NAs before prediction
    if(anyNA(future_matrix)) {
       message("XGBoost Forecast: NAs are present in the final future_matrix before calling predict(). XGBoost should handle these if 'missing=NA' was used in training (default).")
    } else {
       message("XGBoost Forecast: No NAs in the final future_matrix before calling predict().")
    }

    # message("forecast XGBoost 1")

    # 7. Predict
    predictions <- NULL # Initialize predictions
    tryCatch({
      # --- Debugging right before predict (COMMENTED OUT) ---
      # message("--- XGBoost Predict: Pre-call Debug ---")
      # message("Class of model: ", class(model))
      # message("Structure of model (first level):")
      # print(str(model, max.level = 1))
      # message("Model feature names (model$feature_names):")
      # print(model$feature_names)
      # message("Class of future_matrix: ", class(future_matrix))
      # message("Dimensions of future_matrix: ", paste(dim(future_matrix), collapse = " x "))
      # message("First 5 rows of future_matrix (if rows > 0):")
      # if(nrow(future_matrix) > 0) print(head(future_matrix, 5))
      # message("Summary of future_matrix (if rows > 0):")
      # if(nrow(future_matrix) > 0) print(summary(future_matrix))
      # message("Any NAs in future_matrix: ", anyNA(future_matrix))
      # message("Are all columns in future_matrix numeric? ", all(sapply(as.data.frame(future_matrix), is.numeric)))
      # message("--- End XGBoost Predict: Pre-call Debug ---")
      
      predictions <- predict(model, future_matrix)
      
    }, error = function(e_pred) {
      # Keep error handling but remove detailed printing for now
      warning(paste("Error during XGBoost predict() call:", conditionMessage(e_pred)))
      # message("--- XGBoost predict() Full Error Object ---")
      # print(e_pred)
      # message("--- End XGBoost predict() Full Error Object ---")
      # Dejar predictions como NULL
    })

    if (is.null(predictions)) {
      stop("XGBoost predictions are NULL after tryCatch.") # Detener si la predicción falló
    }
    
    if (length(predictions) != total_periods_needed) stop("Predictions length different from total_periods_needed")

    # 8. Combine dates and predictions
    fcst_df <- tibble::tibble(
      ds = future_dates,
      yhat = as.numeric(predictions)
    )
    ### revisar resultados
    message("forecast XGBoost 2")

  }, error = function(e) {
    warning(paste("XGBoost forecast generation failed:", e$message))
    fcst_df <<- NULL
  })

  return(fcst_df)
}

#' Prepare Features for GAM Model
#'
#' Prepares a dataframe with additional time-based features for use in GAM models.
#' These features include a time index and various date components.
#'
#' @param df A tibble or dataframe with at least a 'ds' (Date) column.
#'   It's assumed 'ds' is already in Date format.
#'
#' @return A tibble with the original columns plus added features:
#'   \itemize{
#'     \item `time_index`: Integer. A simple numeric sequence representing the time order (1, 2, 3,...).
#'     \item `yday`: Integer. Day of the year (1-366).
#'     \item `wday`: Factor. Day of the week (e.g., "Mon", "Tue"), with levels ordered starting from Monday.
#'   }
#'   The function arranges the dataframe by 'ds' before creating `time_index`.
#'
#' @noRd
#' @import dplyr lubridate
prepare_gam_features <- function(df) {
  df %>%
    dplyr::arrange(ds) %>%
    dplyr::mutate(
      time_index = dplyr::row_number(), # Simple numeric time trend
      # Date components (ensure factors for some)
      # year = lubridate::year(ds), # Often better handled by time_index spline
      # month = factor(lubridate::month(ds, label = TRUE)), # Handled by yday spline
      # week = lubridate::week(ds), # Handled by yday spline
      yday = lubridate::yday(ds), # Day of year (1-366)
      wday = factor(lubridate::wday(ds, label = TRUE, week_start = 1)) # Monday=1
      # Add other features if needed (e.g., holidays as factors)
    )
}

#' Train GAM Model
#'
#' Trains a Generalized Additive Model (GAM) using `mgcv::gam`.
#' The function prepares features like time index, day of year, and day of week,
#' and can optionally include holiday effects as a factor.
#'
#' @param train_df A tibble with 'ds' (Date) and 'y' (numeric) columns for training.
#' @param config A list containing GAM parameters:
#'   \itemize{
#'     \item `smooth_trend`: Logical. If `TRUE`, uses a smooth term `s(time_index)` for the trend.
#'           If `FALSE`, uses a linear term `time_index`.
#'     \item `use_season_y`: Logical. If `TRUE`, adds a cyclic cubic spline `s(yday, bs='cc', k=...)`
#'           for yearly seasonality (day of year). `k` is capped at 10.
#'     \item `use_season_w`: Logical. If `TRUE`, adds the `wday` (day of week factor) term for
#'           weekly seasonality.
#'   }
#' @param holidays_df Optional. A tibble with 'ds' (Date) and 'holiday' (character/factor) columns.
#'   If provided, a factor column named `is_holiday` is created and added to the model,
#'   with levels including all unique holiday names and "NoHoliday".
#'
#' @return A fitted GAM model object (class `gam` from the `mgcv` package).
#'   Returns `NULL` on error. The model object will have an attribute `holiday_levels`
#'   if holidays were processed.
#'
#' @noRd
#' @import mgcv dplyr lubridate
#' @importFrom stats as.formula
train_gam <- function(train_df, config, holidays_df = NULL) {
  # Basic validation
  if (!is.data.frame(train_df) || !all(c("ds", "y") %in% names(train_df)) || nrow(train_df) < 10) {
    warning("train_df for GAM invalid or too short. Need >= 10 rows.")
    return(NULL)
  }

  # --- Initial Debug Prints ---
  message("--- train_gam: Initial Inputs ---")
  message("GAM Config:")
  print(config)
  message(paste("GAM train_df dimensions:", paste(dim(train_df), collapse="x")))
  message("--- End train_gam: Initial Inputs ---")

  model <- NULL
  tryCatch({
    message("Preparing features for GAM...")
    feature_df <- prepare_gam_features(train_df)

    holiday_col_name <- "is_holiday"
    all_holiday_names <- character(0) # Initialize as empty character vector

    if (!is.null(holidays_df) && nrow(holidays_df) > 0 && all(c("ds", "holiday") %in% names(holidays_df))) {
      all_holiday_names <- unique(as.character(holidays_df$holiday))
      # Filter out any NA or empty string holiday names if they exist
      all_holiday_names <- all_holiday_names[!is.na(all_holiday_names) & all_holiday_names != ""]
    }

    # Define complete holiday levels
    holiday_levels <- c("NoHoliday", all_holiday_names)
    # Ensure "NoHoliday" is first and unique
    holiday_levels <- unique(holiday_levels) 
    message(paste("GAM: Defined holiday levels:", paste(holiday_levels, collapse=", ")))

    if (!is.null(holidays_df) && nrow(holidays_df) > 0 &&
        all(c("ds", "holiday") %in% names(holidays_df))) {

      holidays_for_gam <- holidays_df %>%
        dplyr::mutate(ds = as.Date(ds)) %>%
        # Create the factor using the predefined holiday_levels
        dplyr::mutate({{holiday_col_name}} := factor(holiday, levels = holiday_levels)) %>%
        dplyr::distinct(ds, .keep_all = TRUE)

      feature_df <- feature_df %>%
        dplyr::left_join(holidays_for_gam %>% dplyr::select(ds, all_of(holiday_col_name)), by = "ds")

      # Convert NAs (days that are not holidays) to "NoHoliday" level
      # This also handles cases where a holiday name in data might not be in holiday_levels (though less likely now)
      if (holiday_col_name %in% names(feature_df) && is.factor(feature_df[[holiday_col_name]])) {
        feature_df[[holiday_col_name]][is.na(feature_df[[holiday_col_name]])] <- "NoHoliday"
        message("GAM: Added holiday factor column '", holiday_col_name, "' and processed NAs to 'NoHoliday'.")
      } else {
         # This case should ideally not be reached if holiday_col_name is correctly created
        warning(paste("GAM: Holiday column '", holiday_col_name, "' not found as factor after join or creation. Initializing as 'NoHoliday' factor."))
        feature_df[[holiday_col_name]] <- factor("NoHoliday", levels = holiday_levels)
      }
    } else {
      # If no holidays_df, create the column with "NoHoliday" using the defined levels
      feature_df[[holiday_col_name]] <- factor("NoHoliday", levels = holiday_levels)
      if (!is.null(holidays_df) && nrow(holidays_df) > 0) {
        warning("train_gam: holidays_df provided but missing 'ds'/'holiday' or empty after processing. Using 'NoHoliday'.")
      }
       message("GAM: No holidays processed. '", holiday_col_name, "' column initialized with 'NoHoliday'.")
    }

    # --- DEBUG: Check feature_df ---
    message("Summary of feature_df prepared for GAM:")
    print(summary(feature_df))
    if(anyNA(feature_df)) {
      warning("NAs found in feature_df just before GAM fitting!")
      print(sapply(feature_df, function(x) sum(is.na(x)))) # Show NAs per column
    } else {
      message("Verified: No NAs in feature_df.")
    }
    # --- END DEBUG -

    # --- Build Formula Dynamically ---
    message("DEBUG: Building GAM formula...")
    formula_str <- "y ~ "
    # Trend term (keep selection logic)
    if (config$smooth_trend) {
      formula_str <- paste(formula_str, "s(time_index)")
      message("  + Added smooth trend: s(time_index)")
    } else {
      formula_str <- paste(formula_str, "time_index")
      message("  + Added linear trend: time_index")
    }

    if (config$use_season_y && length(unique(feature_df$yday)) > 1) {
      # Calculate k, ensuring it's at least 3
      # Ensure k < num unique days - 1
      k_yearly <- min(length(unique(feature_df$yday)) - 1, 10) # Use k=10 as specified
      if (k_yearly < 3) {
        warning("Not enough unique yday values for yearly spline (k<3). Skipping.")
      } else {
        term_y <- paste0("s(yday, bs='cc', k=", k_yearly, ")")
        formula_str <- paste(formula_str, "+", term_y)
        message(paste("  + Added yearly seasonality:", term_y))
      }
    } else if (config$use_season_y) {
      warning("Yearly seasonality requested but only 1 unique yday value found.")
    }

    # --- UNCOMMENT Weekly Seasonality ---
    # Use wday factor directly if requested and available
    if (config$use_season_w && length(unique(feature_df$wday)) > 1) {
      if("wday" %in% names(feature_df) && is.factor(feature_df$wday)) {
        term_w <- "wday" # Add the factor name directly
        formula_str <- paste(formula_str, "+", term_w)
        message(paste("  + Added weekly seasonality as factor:", term_w))
      } else {
        warning("Weekly seasonality requested but 'wday' factor not found or invalid in feature_df.")
      }
    }
    # --- END Weekly Seasonality ---

    # Note: The following block was commented out previously, keeping it commented.
    if (config$use_season_w && length(unique(feature_df$wday)) > 1) {
      # Ensure wday factor exists from prepare_gam_features
      if("wday" %in% names(feature_df) && is.factor(feature_df$wday)) {
          term_w <- "wday" # Add the factor name directly
          formula_str <- paste(formula_str, "+", term_w)
          message(paste("  + Added weekly seasonality as factor:", term_w))
        } else {
          warning("Weekly seasonality requested but 'wday' factor not found or invalid in feature_df.")
        }
    }

    if (holiday_col_name %in% names(feature_df)) {
      formula_str <- paste(formula_str, "+", holiday_col_name)
      message(paste("  + Added holiday term:", holiday_col_name))
    }

    # # Seasonal terms (using cyclic cubic splines 'cc')
    # if (config$use_season_w && length(unique(feature_df$wday)) > 1) { # Check if wday varies
    #   # Check if enough unique days for default k (~4)
    #   k_weekly <- min(length(unique(feature_df$wday)) - 1, 4)
    #   if (k_weekly >= 3) { # Need at least k=3 for s()
    #     formula_str <- paste0(formula_str, " + s(wday, bs='cc', k=", k_weekly, ")")
    #     # Alternatively, treat weekday as factor: + wday
    #     # formula_str <- paste(formula_str, "+ wday")
    #     message(paste("Adding weekly seasonality: s(wday, k=", k_weekly, ")"))
    #   } else { warning("Not enough unique wday values for weekly spline.")}
    # }
    # Add regressors/holidays here if implemented later
    # formula_str <- paste(formula_str, "+ regressor1 + s(regressor2)")

    message(paste("GAM: Final formula string:", formula_str)) # Added
    gam_formula <- stats::as.formula(formula_str)
    # --- End Build Formula ---

    message(paste("GAM: Dimensions of feature_df before fitting:", paste(dim(feature_df), collapse="x"))) # Added

    # Fit the model
    model <- tryCatch({
        mgcv::gam(gam_formula, data = feature_df, method = "REML")
      }, error = function(e_gam_fit) {
        message("--- ERROR during mgcv::gam() call in train_gam ---")
        message(paste("GAM formula was:", deparse(gam_formula))) # Print the formula
        message("Summary of feature_df fed to gam():")
        print(summary(feature_df)) # Print summary of data
        message("Error message from mgcv::gam():")
        print(e_gam_fit) # Print the specific error from gam()
        message("--- END ERROR in mgcv::gam() ---")
        NULL # Return NULL if gam() fails
      })

    if (!is.null(model)) {
      attr(model, "holiday_levels") <- holiday_levels
      message("GAM: Stored 'holiday_levels' attribute in the model object.")
    }

    message("gam() finished.")
    # print(summary(model)) # Optional: print summary

  }, error = function(e) {
    warning(paste("GAM model training failed:", conditionMessage(e)))
    print("--- train_gam Error Object ---") # Keep error printing
    print(e)
    print("--- End train_gam Error Object ---")
    model <<- NULL
  }) # End tryCatch

  return(model)
}


#' Forecast using GAM Model
#'
#' Generates forecasts from a trained GAM model using `predict.gam`.
#' It prepares future data with necessary features (time index, date components, holidays)
#' consistent with the training setup.
#'
#' @param model A fitted `gam` model object from `train_gam`.
#' @param train_df The original training dataframe (used for context, e.g., last date,
#'   deriving factor levels for `wday`).
#' @param total_periods_needed Integer. The total number of periods to forecast ahead.
#' @param freq_str Character string. Frequency for date sequence generation ('day', 'week').
#' @param config A list containing GAM parameters used during training (e.g., `smooth_trend`,
#'   `use_season_y`, `use_season_w`). This is used to ensure consistency if feature
#'   creation for prediction depends on these settings (though current `prepare_gam_features`
#'   is mostly independent of `config` for future dates beyond `time_index`).
#' @param holidays_df Optional. A tibble with 'ds' and 'holiday' columns for the forecast period.
#'   Used to create the `is_holiday` factor for future dates, ensuring consistency with
#'   levels stored in `attr(model, "holiday_levels")`.
#'
#' @return A list containing two elements:
#'   \itemize{
#'     \item `forecast`: A tibble with columns 'ds' (Date), 'yhat' (point forecast),
#'           'yhat_lower_95', 'yhat_upper_95' (approximate 95% confidence intervals for the mean response).
#'           80% CIs are not currently generated by this function for GAM.
#'     \item `fitted`: A numeric vector of fitted values (predictions on training data).
#'   }
#'   Returns `NULL` if `model` is invalid or an error occurs.
#'
#' @noRd
#' @import mgcv dplyr tibble lubridate stats
forecast_gam <- function(model, train_df, total_periods_needed, freq_str = "day", config, holidays_df = NULL) {
  # Basic validation
  if (is.null(model) || !inherits(model, "gam")) { return(NULL) }
  # ... other validation ...

  # --- Initial Debug Prints ---
  message("--- forecast_gam: Initial Inputs ---")
  message(paste("GAM forecast_gam: train_df dimensions:", paste(dim(train_df), collapse="x")))
  message(paste("GAM forecast_gam: total_periods_needed:", total_periods_needed))
  message(paste("GAM forecast_gam: freq_str:", freq_str))
  message("--- End forecast_gam: Initial Inputs ---")

  fcst_df <- NULL
  fitted_vals <- NULL

  tryCatch({
    message("Preparing future data features for GAM forecast...")
    # --- Create Future Dataframe with Features ---
    last_train_date <- max(train_df$ds)
    last_time_index <- nrow(train_df) # Assuming simple 1:n index

    by_period <- switch(freq_str, "week" = lubridate::weeks(1), lubridate::days(1))
    start_forecast_date <- last_train_date + by_period
    future_dates <- seq.Date(from = start_forecast_date, by = freq_str, length.out = total_periods_needed)

    future_df <- tibble::tibble(ds = future_dates) %>%
      dplyr::mutate(time_index = seq(from = last_time_index + 1, length.out = total_periods_needed)) %>%
      dplyr::mutate(
        yday = lubridate::yday(ds),
        wday = factor(lubridate::wday(ds, label = TRUE, week_start = 1),
                      levels = levels(prepare_gam_features(train_df)$wday))
      )

    holiday_col_name_gam <- "is_holiday"
    retrieved_holiday_levels <- attr(model, "holiday_levels")

    # --- Holiday Levels Check ---
    if (is.null(retrieved_holiday_levels)) {
      warning("GAM Forecast: 'holiday_levels' attribute not found in model. Attempting to derive from holidays_df.")
      # Fallback: derive levels from holidays_df (less robust if holidays_df changes)
      all_holiday_names_fcst <- character(0)
      if (!is.null(holidays_df) && nrow(holidays_df) > 0 && all(c("ds", "holiday") %in% names(holidays_df))) {
        all_holiday_names_fcst <- unique(as.character(holidays_df$holiday))
        all_holiday_names_fcst <- all_holiday_names_fcst[!is.na(all_holiday_names_fcst) & all_holiday_names_fcst != ""]
      }
      retrieved_holiday_levels <- unique(c("NoHoliday", all_holiday_names_fcst))
      message("GAM Forecast: Derived holiday_levels for forecast:", paste(retrieved_holiday_levels, collapse=", "))
    } else {
      message(paste("GAM Forecast: Using holiday levels from model attribute:", paste(retrieved_holiday_levels, collapse=", ")))
    }
    # --- End Holiday Levels Check ---

    if (!is.null(holidays_df) && nrow(holidays_df) > 0 &&
        all(c("ds", "holiday") %in% names(holidays_df))) {

        holidays_for_gam_fcst <- holidays_df %>%
          dplyr::mutate(ds = as.Date(ds)) %>%
          # Use retrieved_holiday_levels for factor creation
          dplyr::mutate({{holiday_col_name_gam}} := factor(holiday, levels = retrieved_holiday_levels)) %>%
          dplyr::distinct(ds, .keep_all = TRUE)

        future_df <- future_df %>%
          dplyr::left_join(holidays_for_gam_fcst %>% dplyr::select(ds, all_of(holiday_col_name_gam)), by = "ds")

        if (holiday_col_name_gam %in% names(future_df) && is.factor(future_df[[holiday_col_name_gam]])) {
            # Convert NAs (days that are not holidays or holidays not in retrieved_holiday_levels) to "NoHoliday"
            future_df[[holiday_col_name_gam]][is.na(future_df[[holiday_col_name_gam]])] <- "NoHoliday"
            message("GAM Forecast: Processed holiday column in future_df, NAs set to 'NoHoliday'.")
        } else {
            warning(paste("GAM Forecast: Holiday column '", holiday_col_name_gam, "' not found as factor after join. Initializing."))
            future_df[[holiday_col_name_gam]] <- factor("NoHoliday", levels = retrieved_holiday_levels)
        }
    } else {
      # If no holidays_df for forecast period, or it's invalid, create the column with "NoHoliday"
      future_df[[holiday_col_name_gam]] <- factor("NoHoliday", levels = retrieved_holiday_levels)
      message("GAM Forecast: No future holidays processed. '", holiday_col_name_gam, "' column initialized with 'NoHoliday'.")
    }
    # --- End Create Future Dataframe ---
    message(paste("GAM Forecast: Dimensions of future_df for prediction:", paste(dim(future_df), collapse=" x "))) # Added

    # --- GAM Predict Summary ---
    message("GAM Forecast: Summary of future_df before prediction:") # Added
    print(summary(future_df)) # Added
    # --- End GAM Predict Summary ---

    # --- Predict with Confidence Intervals ---
    message("Predicting with GAM (se.fit=TRUE)...")
    # Use se.fit=TRUE to get standard errors for CI calculation
    preds <- tryCatch({
        predict(model, newdata = future_df, type = "response", se.fit = TRUE)
      }, error = function(e_gam_predict) {
        message("--- ERROR during predict.gam() call in forecast_gam ---")
        message("Dimensions of future_df fed to predict.gam(): ", paste(dim(future_df), collapse="x"))
        message("Summary of future_df fed to predict.gam():")
        print(summary(future_df))
        message("Error message from predict.gam():")
        print(e_gam_predict)
        message("--- END ERROR in predict.gam() ---")
        NULL # Return NULL if predict() fails
      })

    if(is.null(preds)){
      stop("predict.gam() failed and returned NULL.") # Propagate error to outer tryCatch in app_server
    }

    # --- DEBUG: Check Standard Errors and CIs ---
    message("Structure of prediction object:")
    print(str(preds))
    message("Summary of Standard Errors (preds$se.fit):")
    print(summary(preds$se.fit))
    message(paste("Any NAs in se.fit?", anyNA(preds$se.fit)))
    # --- END DEBUG ---

    # Calculate approximate 95% CI
    ci_factor <- 1.96
    lower_ci <- preds$fit - ci_factor * preds$se.fit
    upper_ci <- preds$fit + ci_factor * preds$se.fit

    # --- DEBUG: Check CI Values ---
    message("Summary of calculated 95% CI Lower Bound:")
    print(summary(lower_ci))
    message(paste("Any NAs in lower_ci?", anyNA(lower_ci)))
    message("Summary of calculated 95% CI Upper Bound:")
    print(summary(upper_ci))
    message(paste("Any NAs in upper_ci?", anyNA(upper_ci)))
    # --- END DEBUG ---
    fcst_df <- tibble::tibble(
      ds = future_dates,
      yhat = preds$fit,
      yhat_lower_95 = lower_ci, # Use calculated vectors
      yhat_upper_95 = upper_ci
    )

    # Calculate approximate 95% CI (adjust factor for different levels, e.g., 1.28 for 80%)
    # Note: these are CIs for the mean response, not full prediction intervals
    # ci_factor <- 1.96
    # fcst_df <- tibble::tibble(
    #   ds = future_dates,
    #   yhat = preds$fit,
    #   yhat_lower_95 = preds$fit - ci_factor * preds$se.fit,
    #   yhat_upper_95 = preds$fit + ci_factor * preds$se.fit
    # )
    # Add 80% CI if needed (factor approx 1.28)
    # fcst_df$yhat_lower_80 = preds$fit - 1.28 * preds$se.fit
    # fcst_df$yhat_upper_80 = preds$fit + 1.28 * preds$se.fit
    message("GAM forecast tibble created.")
    # --- End Predict ---

    # --- Get Fitted Values ---
    message("Getting GAM fitted values...")
    # Regenerate training features including the holiday column with consistent levels
    train_feature_df <- prepare_gam_features(train_df)

    # Add holiday column to train_feature_df consistent with how it was done in train_gam
    # This requires access to the same holiday_levels used during training (retrieved from model)
    if (!is.null(holidays_df) && nrow(holidays_df) > 0 && all(c("ds", "holiday") %in% names(holidays_df))) {
        holidays_for_gam_train_fitted <- holidays_df %>%
            dplyr::mutate(ds = as.Date(ds)) %>%
            dplyr::mutate({{holiday_col_name_gam}} := factor(holiday, levels = retrieved_holiday_levels)) %>%
            dplyr::distinct(ds, .keep_all = TRUE)
        
        train_feature_df <- train_feature_df %>%
            dplyr::left_join(holidays_for_gam_train_fitted %>% dplyr::select(ds, all_of(holiday_col_name_gam)), by = "ds")
        
        if (holiday_col_name_gam %in% names(train_feature_df) && is.factor(train_feature_df[[holiday_col_name_gam]])) {
            train_feature_df[[holiday_col_name_gam]][is.na(train_feature_df[[holiday_col_name_gam]])] <- "NoHoliday"
        } else {
            train_feature_df[[holiday_col_name_gam]] <- factor("NoHoliday", levels = retrieved_holiday_levels)
        }
    } else {
        train_feature_df[[holiday_col_name_gam]] <- factor("NoHoliday", levels = retrieved_holiday_levels)
    }
    
    # Ensure all necessary columns are present for prediction
    required_cols <- all.vars(model$formula) # Get variables from formula
    missing_cols_train_fitted <- setdiff(required_cols, names(train_feature_df))
    if(length(missing_cols_train_fitted) > 0 && !"y" %in% missing_cols_train_fitted) { # 'y' will be missing, that's ok
        # Attempt to add missing columns as NA or default if simple (e.g. time_index if somehow missed)
        # This part might need more robust handling depending on what could be missing.
        # For now, warn and proceed; GAM might handle some NAs in predictors depending on formula.
        warning(paste("GAM Fitted: Columns missing from regenerated training features for fitted values:", paste(missing_cols_train_fitted, collapse=", "), ". This might cause predict() to fail for fitted values."))
    }


    fitted_vals <- predict(model, newdata = train_feature_df, type = "response")
    message("GAM fitted values obtained.")
    # --- End Fitted Values ---

  }, error = function(e) {
    warning(paste("GAM forecast generation failed:", conditionMessage(e)))
    fcst_df <<- NULL; fitted_vals <<- NULL
  })

  return(list(forecast = fcst_df, fitted = fitted_vals))
}

#' Train NNETAR Model
#'
#' Trains a Neural Network Autoregressive (NNETAR) model using `forecast::nnetar`.
#' The model structure (number of non-seasonal lags `p`, seasonal lags `P`, and
#' hidden neurons `size`) can be automatically determined or manually specified.
#'
#' @param train_df A tibble with 'ds' (Date) and 'y' (numeric) columns for training.
#' @param config A list containing NNETAR parameters:
#'   \itemize{
#'     \item `nnetar_p`: Integer. Number of non-seasonal lags. If 0 and `nnetar_P` is 0,
#'           `nnetar` chooses `p`.
#'     \item `nnetar_P`: Integer. Number of seasonal lags. If 0 and `nnetar_p` is 0,
#'           `nnetar` chooses `P` (if data frequency > 1).
#'     \item `nnetar_size_method`: Character. 'auto' or 'manual'.
#'           If 'auto', size is calculated based on `p`, `P`, and data frequency.
#'           If `p` and `P` are 0, `nnetar` chooses size.
#'     \item `nnetar_size_manual`: Integer. Number of neurons in the hidden layer if `size_method` is 'manual'.
#'     \item `nnetar_repeats`: Integer. Number of networks to train, the best is kept.
#'     \item `nnetar_lambda_auto`: Logical. If `TRUE`, automatically select Box-Cox lambda.
#'     \item `nnetar_lambda_manual`: Numeric (0-1). Manual Box-Cox lambda if `lambda_auto` is `FALSE`.
#'   }
#' @param aggregation_level Character string. Data frequency ("Daily", "Weekly").
#'   Influences the `frequency` of the `ts` object (7 for "Daily", 52 for "Weekly").
#'
#' @return A fitted NNETAR model object (class `nnetar`). Returns `NULL` on error.
#'   The model object will have attributes `aggregation_level` and `frequency_used`.
#'
#' @noRd
#' @import forecast dplyr lubridate stats
#' @importFrom rlang `%||%`
train_nnetar <- function(train_df, config, aggregation_level) {
  message("Starting train_nnetar")
  # Basic validation
  if (!is.data.frame(train_df) || !all(c("ds", "y") %in% names(train_df)) || nrow(train_df) < 3) {
    warning("train_df for NNETAR invalid or too short (need at least 3 rows). Returning NULL.")
    return(NULL)
  }
  if(!aggregation_level %in% c("Daily", "Weekly")) {
    warning("Unknown aggregation level provided to train_nnetar. Seasonality might be incorrect.")
    aggregation_level <- "Unknown" 
  }

  # Determine frequency for ts object
  freq_ts <- 1
  if (aggregation_level == "Daily") {
    freq_ts <- 7
  } else if (aggregation_level == "Weekly") {
    freq_ts <- 52 
  }
  
  # Ensure enough data for chosen frequency, especially if P > 0
  if (config$nnetar_P > 0 && freq_ts > 1 && nrow(train_df) < 2 * freq_ts) {
    warning(paste("NNETAR: Insufficient data (", nrow(train_df), ") for seasonal P with frequency =", freq_ts, ". Needs at least 2*freq_ts. Fitting non-seasonally or returning NULL."))
    # Option: force P=0 or return NULL. Forcing P=0 might be safer.
    config$nnetar_P <- 0 
  }


  # Create time series object
  y_ts <- NULL
  tryCatch({
    start_date <- min(train_df$ds)
    start_year <- lubridate::year(start_date)
    ts_start_val <- if (freq_ts > 1) {
      day_in_cycle <- switch(as.character(freq_ts),
                             "7" = lubridate::wday(start_date, week_start = getOption("lubridate.week.start", 1)),
                             "52" = lubridate::isoweek(start_date), # isoweek might be better for 52
                             floor(lubridate::yday(start_date) / (365.25 / freq_ts)) + 1)
      c(start_year, day_in_cycle)
    } else { start_year }
    y_ts <- stats::ts(train_df$y, frequency = freq_ts, start = ts_start_val)
  }, error = function(e){
    warning(paste("Failed to create ts object for NNETAR:", conditionMessage(e)))
    return(NULL)
  })
  if(is.null(y_ts)) return(NULL)
  message(paste("NNETAR: Created ts object with frequency:", stats::frequency(y_ts)))

  # Construct arguments for nnetar()
  nnetar_args <- list(y = y_ts)
  
  # Lags (p, P)
  p_val <- as.integer(config$nnetar_p %||% 0)
  P_val <- as.integer(config$nnetar_P %||% 0)

  # Lambda Handling
  lambda_val <- NULL
  if (isTRUE(config$nnetar_lambda_auto)) {
    lambda_val <- "auto"
    message("NNETAR: Using lambda = 'auto'.")
  } else {
    if (!is.null(config$nnetar_lambda_manual) && !is.na(config$nnetar_lambda_manual) && nzchar(as.character(config$nnetar_lambda_manual))) {
      manual_lambda <- as.numeric(config$nnetar_lambda_manual)
      if (!is.na(manual_lambda) && manual_lambda >= 0 && manual_lambda <= 1) {
        lambda_val <- manual_lambda
        message(paste("NNETAR: Using manual lambda =", lambda_val))
      } else {
        message("NNETAR: Manual lambda value '", config$nnetar_lambda_manual, "' is invalid (must be 0-1). No transformation will be applied.")
      }
    } else {
      message("NNETAR: No Box-Cox transformation (manual lambda is NA, empty or NULL).")
    }
  }
  if (!is.null(lambda_val)) nnetar_args$lambda <- lambda_val

  # Size (Hidden Neurons) Handling
  size_val <- NULL
  if (config$nnetar_size_method == "auto") {
    message("NNETAR: Size method is 'auto'.")
    if (P_val > 0 && freq_ts > 1) { # Seasonal model with P specified
      # Heuristic: (p_eff + P_eff + 1)/2. If p_val=0, nnetar picks a p, so use 1 as placeholder.
      p_eff_for_size <- if(p_val > 0) p_val else 1 
      size_val <- max(1, floor(((p_eff_for_size) + P_val + 1) / 2))
      message(paste("NNETAR: Auto size for seasonal (P>0, freq>1), p_val=", p_val, ", P_val=", P_val, ", calculated size_val=", size_val))
    } else if (p_val > 0) { # Non-seasonal model, p specified
      size_val <- max(1, floor((p_val + 1) / 2))
      message(paste("NNETAR: Auto size for non-seasonal (p>0), p_val=", p_val, ", calculated size_val=", size_val))
    } else { # p_val=0 and (P_val=0 or freq_ts<=1) -> nnetar chooses p, P (if applicable), and size
      message("NNETAR: Auto size, p=0, P=0 (or non-seasonal P). Letting nnetar choose size.")
      size_val <- NULL 
    }
  } else { # Manual size
    size_val <- as.integer(config$nnetar_size_manual %||% 1) # Default to 1 if NULL/NA
    size_val <- max(1, size_val) # Ensure at least 1
    message(paste("NNETAR: Manual size specified: size_val=", size_val))
  }
  # Add size to args if determined
  if (!is.null(size_val)) nnetar_args$size <- size_val

  # p and P lag handling for nnetar call
  if (P_val > 0 && freq_ts <= 1) {
    warning(paste0("NNETAR: Seasonal lags (P=", P_val, ") specified but data frequency (", freq_ts, ") is not seasonal (>1). ",
                   "Treating as non-seasonal; P will be ignored by nnetar or cause an error. Consider setting P=0."))
    # nnetar itself will likely ignore P or error if m=1. We will proceed and let nnetar handle it.
    # For clarity in args, we could effectively set P_val = 0 here for the call if freq_ts <=1
    # However, the user did specify P > 0, so let nnetar decide.
  }

  if (p_val == 0 && P_val == 0) {
    message("NNETAR: p=0, P=0. Letting nnetar choose p, P (if freq>1), and size (if auto).")
    # Do not add p or P to nnetar_args if they are 0, nnetar will use its defaults.
    # Size is already handled: if auto and p=0,P=0, size_val is NULL. If manual, it's set.
  } else if (P_val > 0 && freq_ts > 1) { # Seasonal model, P specified
    nnetar_args$P <- P_val
    message(paste("NNETAR: Using P =", P_val, "for seasonal model."))
    if (p_val > 0) { # If p also specified
      nnetar_args$p <- p_val
      message(paste("NNETAR: Using p =", p_val, "for non-seasonal part of seasonal model."))
    } else {
      message("NNETAR: p=0 for seasonal model. nnetar will select non-seasonal AR order.")
    }
  } else { # Non-seasonal model (either P_val=0 or P_val>0 but freq_ts<=1)
    if (p_val > 0) {
      nnetar_args$p <- p_val
      message(paste("NNETAR: Using p =", p_val, "for non-seasonal model."))
    } else {
      message("NNETAR: p=0 for non-seasonal model. nnetar will select non-seasonal AR order.")
    }
    # If P_val > 0 but freq_ts <= 1, it's already warned above. nnetar will ignore P.
  }
  
  # Repeats
  nnetar_args$repeats <- as.integer(config$nnetar_repeats %||% 20) # Default 20 if NULL

  # scale.inputs is TRUE by default in nnetar.

  model <- NULL
  tryCatch({
    message("NNETAR: Final arguments for forecast::nnetar call:")
    print(str(nnetar_args))
    model <- do.call(forecast::nnetar, nnetar_args)
    if (!is.null(model)) {
        attr(model, "aggregation_level") <- aggregation_level
        attr(model, "frequency_used") <- stats::frequency(y_ts)
        message(paste("NNETAR model trained successfully. Model summary (first line):", capture.output(print(model))[1]))
    } else {
        message("NNETAR training returned NULL.")
    }
  }, error = function(e) {
    warning(paste("NNETAR model training failed:", conditionMessage(e)))
    print(e) # Print full error for debugging
    model <<- NULL
  })
  
  message("Finished train_nnetar")
  return(model)
}

#' Forecast using NNETAR Model
#'
#' Generates forecasts from a trained NNETAR model using `forecast::forecast`.
#'
#' @param model A fitted `nnetar` model object from `train_nnetar`.
#' @param total_periods_needed Integer. The total number of periods for which the forecast is required.
#' @param train_end_date Date. The last date of the training data, used to generate the
#'   date sequence for the forecast tibble.
#' @param freq_str Character string. The frequency of the time series ('day', 'week'),
#'   used for generating the date sequence.
#'
#' @return A list containing two elements:
#'   \itemize{
#'     \item `forecast`: A tibble with columns 'ds' (Date), 'yhat' (point forecast),
#'           and potentially 'yhat_lower_80', 'yhat_upper_80', 'yhat_lower_95', 'yhat_upper_95'
#'           (prediction intervals, if `PI=TRUE` was used in `forecast()` and model supports it).
#'           Intervals might be NA if not produced by the model.
#'     \item `fitted`: A numeric vector of fitted values from the model.
#'   }
#'   Returns `NULL` if `model` is invalid or an error occurs.
#'
#' @noRd
#' @import forecast dplyr tibble lubridate stats
forecast_nnetar <- function(model, total_periods_needed, train_end_date, freq_str = "day") {
  message("Starting forecast_nnetar")
  if (is.null(model) || !inherits(model, "nnetar")) {
    warning("Invalid NNETAR model object provided. Returning NULL.")
    return(NULL)
  }
  if (!is.numeric(total_periods_needed) || total_periods_needed < 1) {
    warning("Invalid total_periods_needed provided to forecast_nnetar. Returning NULL.")
    return(NULL)
  }

  fcst_nnetar_obj <- NULL
  fitted_vals <- NULL
  fcst_df <- NULL

  tryCatch({
    message("NNETAR: Calling forecast::forecast...")
    # NNETAR forecast might not always produce intervals if repeats=1 and certain conditions.
    # Use level argument to request them.
    fcst_nnetar_obj <- forecast::forecast(model, h = total_periods_needed, level = c(80, 95), PI = TRUE) 
    
    if(is.null(fcst_nnetar_obj)) stop("forecast::forecast for nnetar returned NULL")
    
    message("NNETAR: Extracting fitted values...")
    fitted_vals <- stats::fitted(fcst_nnetar_obj) 
    if(is.null(fitted_vals)) message("NNETAR: stats::fitted() returned NULL.")

    message("NNETAR: Generating forecast dates...")
    by_period <- switch(freq_str, "week" = lubridate::weeks(1), lubridate::days(1))
    # Correct start_forecast_date based on freq_str relative to train_end_date
    start_forecast_date <- if (freq_str == "week") {
        train_end_date + lubridate::weeks(1)
    } else {
        train_end_date + lubridate::days(1)
    }
    forecast_dates <- seq.Date(from = start_forecast_date, by = freq_str, length.out = total_periods_needed)
    
    if(length(forecast_dates) != length(fcst_nnetar_obj$mean)) {
        stop(paste0("NNETAR: Generated forecast_dates length (", length(forecast_dates), 
                    ") does not match forecast horizon mean length (", length(fcst_nnetar_obj$mean), ")."))
    }

    fcst_df <- tibble::tibble(
      ds = forecast_dates,
      yhat = as.numeric(fcst_nnetar_obj$mean)
    )
    
    # Add confidence intervals if they exist in the forecast object
    if (!is.null(fcst_nnetar_obj$lower) && !is.null(fcst_nnetar_obj$upper)) {
        if (all(c("80%", "95%") %in% colnames(fcst_nnetar_obj$lower))) {
            fcst_df$yhat_lower_80 = as.numeric(fcst_nnetar_obj$lower[, "80%"])
            fcst_df$yhat_upper_80 = as.numeric(fcst_nnetar_obj$upper[, "80%"])
            fcst_df$yhat_lower_95 = as.numeric(fcst_nnetar_obj$lower[, "95%"])
            fcst_df$yhat_upper_95 = as.numeric(fcst_nnetar_obj$upper[, "95%"])
        } else {
            message("NNETAR: 80% or 95% CI columns not found in forecast object. Intervals will be NA.")
            # Add NA columns if expected but not found, to maintain structure
            fcst_df$yhat_lower_80 = NA_real_
            fcst_df$yhat_upper_80 = NA_real_
            fcst_df$yhat_lower_95 = NA_real_
            fcst_df$yhat_upper_95 = NA_real_
        }
    } else {
        message("NNETAR: No confidence intervals (lower/upper) found in forecast object. Intervals will be NA.")
        fcst_df$yhat_lower_80 = NA_real_
        fcst_df$yhat_upper_80 = NA_real_
        fcst_df$yhat_lower_95 = NA_real_
        fcst_df$yhat_upper_95 = NA_real_
    }
    message("NNETAR: Forecast tibble created.")

  }, error = function(e) {
    warning(paste("NNETAR forecast generation failed:", conditionMessage(e)))
    print(e) # Print full error for debugging
    fcst_df <<- NULL # Ensure reset on error
    fitted_vals <<- NULL
  })
  
  message("Finished forecast_nnetar")
  return(list(forecast = fcst_df, fitted = fitted_vals))
}
#' Train Random Forest Model
#'
#' Trains a Random Forest model using the `ranger` package with a *prepared* `recipes::recipe`.
#'
#' @param prep_recipe A *prepared* `recipes::recipe` object. This recipe defines all
#'   feature engineering steps (lags, rolling windows, date components, etc.) and should
#'   have been prepared on the training data.
#' @param config A list containing Random Forest hyperparameters:
#'   \itemize{
#'     \item `rf_num_trees`: Integer. Number of trees to grow.
#'     \item `rf_mtry`: Integer. Number of variables randomly sampled as candidates at each split.
#'           If 0 or invalid, it's automatically set to `floor(sqrt(number_of_predictors))`.
#'     \item `rf_min_node_size`: Integer. Minimum size of terminal nodes.
#'   }
#'
#' @return A fitted `ranger` model object. Returns `NULL` on error.
#'
#' @noRd
#' @import ranger recipes dplyr tibble
#' @importFrom rlang `%||%`
train_rf <- function(prep_recipe, config) {
  message("Starting train_rf")
  # --- Input Validation ---
  if (is.null(prep_recipe) || !inherits(prep_recipe, "recipe") || is.null(prep_recipe$steps)) {
    warning("train_rf: Invalid or non-prepared recipe provided.")
    return(NULL)
  }
  message("--- Entering train_rf ---")

  model <- NULL
  tryCatch({
    # Extract processed training data
    message("Juicing recipe for outcome (y)...")
    train_y <- recipes::juice(prep_recipe, y) %>% dplyr::pull() # Use explicit 'y'
    message("Baking recipe for predictors (X)...")
    train_x_df <- recipes::bake(prep_recipe, new_data = NULL, has_role("predictor"))

    # --- Dimension Checks ---
    if(nrow(train_x_df) == 0 || ncol(train_x_df) == 0) stop("Baked predictor data has zero rows or columns.")
    if(nrow(train_x_df) != length(train_y)) stop("Dimension mismatch between predictors and outcome.")
    message(paste("Predictor dimensions:", paste(dim(train_x_df), collapse=" x ")))
    # --- End Checks ---

    # Combine into dataframe for ranger formula interface
    train_data_rf <- dplyr::bind_cols(train_x_df, tibble::tibble(y = train_y))

    # --- Determine Ranger Parameters ---
    num_predictors <- ncol(train_x_df)
    # Calculate default mtry (sqrt(p)) if user specified 0 or invalid
    mtry_val <- config$rf_mtry
    if (is.null(mtry_val) || is.na(mtry_val) || !is.numeric(mtry_val) || mtry_val < 1) {
      mtry_val <- max(1, floor(sqrt(num_predictors))) # Ensure at least 1
      message(paste("Using calculated mtry:", mtry_val))
    } else {
      mtry_val <- min(mtry_val, num_predictors) # Ensure mtry <= num_predictors
      message(paste("Using user-specified mtry:", mtry_val))
    }

    num_trees_val <- config$rf_num_trees %||% 500 # Default 500 if NULL
    min_node_size_val <- config$rf_min_node_size %||% 5 # Default 5 if NULL
    # --- End Parameters ---

    message("Starting ranger() training...")
    model <- ranger::ranger(
      formula = y ~ .,                 # Predict y using all other columns
      data = train_data_rf,
      num.trees = num_trees_val,
      mtry = mtry_val,
      min.node.size = min_node_size_val,
      importance = 'impurity',        # Or 'permutation', or 'none'
      num.threads = 1                  # Use 1 thread for safety in parallel Shiny
      # seed = 123                     # Optional: for reproducibility
    )
    message("ranger() training finished.")

  }, error = function(e) {
    warning(paste("Random Forest model training failed inside train_rf:", conditionMessage(e)))
    model <<- NULL
  })
  message("Finished train_rf")
  return(model)
}


#' Forecast using Random Forest Model
#'
#' Generates forecasts from a trained `ranger` model using a *prepared* `recipes::recipe`.
#' Similar to `forecast_xgboost`, this function creates future features based on the recipe
#' and then predicts.
#'
#' @param model A fitted `ranger` model object from `train_rf`.
#' @param prep_recipe The *prepared* `recipes::recipe` object used for training.
#' @param full_df The original complete dataframe (post-cleaning/aggregation, pre-split)
#'   with 'ds' and 'y', used as historical context for feature generation.
#' @param train_df The original training dataframe (with 'ds' and 'y'), used for calculating
#'   fitted values by baking the recipe on it and predicting.
#' @param train_end_date Date. The last date of the training set.
#' @param total_periods_needed Integer. Total number of future periods to forecast.
#' @param freq_str Character string. Frequency for date sequence generation ('day', 'week').
#'
#' @return A list containing two elements:
#'   \itemize{
#'     \item `forecast`: A tibble with 'ds' (Date) and 'yhat' (numeric forecast) columns.
#'           Random Forest models from `ranger` do not directly provide prediction intervals.
#'     \item `fitted`: A numeric vector of fitted values (predictions on the training data).
#'   }
#'   Returns `NULL` if an error occurs.
#'
#' @noRd
#' @import ranger recipes dplyr tibble lubridate stats utils
forecast_rf <- function(model, prep_recipe, full_df, train_df,
                        train_end_date, total_periods_needed, freq_str = "day") {
  message("Starting forecast_rf")
  # Basic validation
  if (is.null(model) || !inherits(model, "ranger")) { return(NULL) }
  if (is.null(prep_recipe) || !inherits(prep_recipe, "recipe")) { return(NULL) }
  if (!is.data.frame(train_df) || !all(c("ds", "y") %in% names(train_df))) { # Add validation for train_df
    stop("forecast_rf requires valid train_df input.")
  }
  # ... other validation ...
  if (is.null(model)) { # Double check after initial validation
    warning("Model is NULL inside forecast_rf tryCatch.")
    return(NULL)
  }

  fcst_df <- NULL
  fitted_vals <- NULL

  tryCatch({
    # --- Create Future Features (similar to forecast_xgboost) ---
    message("Preparing future data features for RF forecast...")
    max_lag_needed <- 0 # ... (calculate max_lag_needed from recipe) ...
    if(nrow(full_df) < max_lag_needed) {
      stop(paste("Need at least", max_lag_needed, "rows in full_df for forecast lags."))
    }



    by_period <- switch(freq_str, "week" = lubridate::weeks(1), lubridate::days(1))
    start_forecast_date <- train_end_date + by_period
    future_dates <- seq.Date(from = start_forecast_date, by = freq_str, length.out = total_periods_needed) # Generate future dates using train_end_date, freq_str, total_periods_needed
    message(paste("Generated future_dates length:", length(future_dates)))
    if(length(future_dates) != total_periods_needed) stop("Date sequence length mismatch!")

    recent_data <- utils::tail(full_df, max_lag_needed)
    future_template <- tibble::tibble(ds = future_dates, y = NA_real_)
    combined_df <- dplyr::bind_rows(recent_data, future_template)

    message("Baking combined data for RF...")
    baked_combined <- recipes::bake(prep_recipe, new_data = combined_df, has_role("predictor")) # Use has_role
    message(paste("Baked_combined dimensions:", paste(dim(baked_combined), collapse=" x ")))

    message("Extracting future features...")
    future_features_baked_df <- utils::tail(baked_combined, total_periods_needed) # This is a dataframe
    message(paste("Extracted future_features_baked_df dimensions:", paste(dim(future_features_baked_df), collapse=" x ")))
    if(nrow(future_features_baked_df) != total_periods_needed) stop("Baked feature rows do not match total periods needed.")
    # --- End Feature Creation ---

    # --- Predict ---
    message("Predicting using ranger model...")
    # Predict needs a data frame with same column names as training predictors
    # Check names - bake should guarantee this if recipe is consistent
    predictor_names <- model$forest$independent.variable.names
    missing_cols <- setdiff(predictor_names, names(future_features_baked_df))
    if(length(missing_cols) > 0) stop(paste("Missing predictor columns for forecast:", paste(missing_cols, collapse=", ")))

    predictions_obj <- predict(model, data = future_features_baked_df[, predictor_names, drop = FALSE]) # Pass data frame
    predictions <- predictions_obj$predictions # Extract prediction values
    message(paste("Generated predictions length:", length(predictions)))
    if (length(predictions) != total_periods_needed) stop("Predictions length different from total_periods_needed")
    # --- End Predict ---

    # --- Create Forecast Tibble ---
    message("Creating RF forecast tibble...")
    fcst_df <- tibble::tibble(
      ds = future_dates,
      yhat = as.numeric(predictions)
      # RF doesn't naturally give CIs like ETS/ARIMA/GAM(se.fit)
      # Could add yhat_lower/upper as NA or estimate via quantile regression forests later
    )
    message("RF forecast tibble created.")

    # # --- Get Fitted Values ---
    # message("Getting RF fitted values...")
    # # Predict on original training features (baked)
    # train_x_df <- recipes::bake(prep_recipe, new_data = NULL, has_role("predictor"))
    # if(nrow(train_x_df) > 0){
    #   fitted_preds_obj <- predict(model, data = train_x_df[, predictor_names, drop = FALSE])
    #   fitted_vals <- fitted_preds_obj$predictions
    #   message("RF fitted values obtained.")
    # } else {
    #   warning("Could not bake training predictors for fitted values.")
    #   fitted_vals <- NULL
    # }
    # # --- End Fitted Values ---
    # --- Get Fitted Values (Corrected) ---
    message("Getting RF fitted values on training data...")
    fitted_vals <- NULL # Initialize

    # --- BAKE USING train_df as new_data ---
    train_predictors_baked_df <- recipes::bake(
      prep_recipe,
      new_data = train_df, # Apply recipe to train_df
      has_role("predictor")
    )
    message(paste("Baked training predictors. Dimensions:", paste(dim(train_predictors_baked_df), collapse=" x ")))
    # --- End Bake ---

    # Check if baked result matches train_df rows
    if(nrow(train_predictors_baked_df) == nrow(train_df)){
      predictor_names <- model$forest$independent.variable.names
      missing_cols <- setdiff(predictor_names, names(train_predictors_baked_df))
      if(length(missing_cols) > 0) {
        stop(paste("Training data missing model features after baking:", paste(missing_cols, collapse=", ")))
      }

      # Predict on the baked training predictors
      fitted_preds_obj <- predict(model, data = train_predictors_baked_df[, predictor_names, drop = FALSE])
      fitted_vals <- fitted_preds_obj$predictions
      message("RF fitted values obtained.")
      # Final check
      if(length(fitted_vals) != nrow(train_df)){
        warning("Fitted values length still does not match training data rows after prediction!")
        fitted_vals <- NULL
      }

    } else {
      warning("Baked training predictors row count does not match original train_df rows. Cannot get fitted values.")
      # This could happen if recipe steps remove rows (e.g., NA imputation wasn't perfect, or filters)
    }
    # --- End Fitted Values ---

  }, error = function(e) {
    warning(paste("Random Forest forecast generation failed:", conditionMessage(e)))
    fcst_df <<- NULL; fitted_vals <<- NULL
  })

  # --- ADD DEBUG LOGGING BEFORE RETURN ---
  message("--- forecast_rf: Final checks before returning ---")
  message(paste("  Forecast df is NULL:", is.null(fcst_df)))
  if(!is.null(fcst_df)) message(paste("  Forecast df rows:", nrow(fcst_df)))
  message(paste("  Fitted values (fitted_vals) is NULL:", is.null(fitted_vals)))
  if(!is.null(fitted_vals)) message(paste("  Fitted values length:", length(fitted_vals)))
  if(!is.null(fitted_vals)) message(paste("  Any NAs in fitted values?", anyNA(fitted_vals)))
  message("-------------------------------------------------")
  message("Finished forecast_rf")

  return(list(forecast = fcst_df, fitted = fitted_vals))
}


