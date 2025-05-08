# R/utils_train_forecast.R

#' Train ARIMA Model
#'
#' Trains an ARIMA model using forecast::Arima or forecast::auto.arima.
#'
#' @param train_df Tibble with 'ds' (Date) and 'y' (numeric) columns.
#' @param config List containing ARIMA parameters: auto (logical), p, d, q,
#'   seasonal (logical), P, D, Q, period.
#' @param aggregation_level Character string indicating data frequency ('Daily', 'Weekly').
#'
#' @return A fitted ARIMA model object (from forecast package). Returns NULL on error.
#'
#' @noRd
#'
#' @import forecast dplyr tibble lubridate
#' @importFrom stats ts frequency start fitted cycle time
#' @importFrom lubridate year yday weeks
#' @importFrom rlang %||%
train_arima <- function(train_df, config, aggregation_level) {
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
                                    stepwise = TRUE, approximation = TRUE, trace = FALSE )
      message("auto.arima finished.")
      message("ARIMA model fitted successfully.") # Added success message
    } else {
      # Use determined freq as period for manual seasonal
      seasonal_list <- if(config$seasonal) list(order = c(config$P, config$D, config$Q), period = freq) else FALSE
      model <- forecast::Arima(y_ts,
                               order = c(config$p, config$d, config$q),
                               seasonal = seasonal_list)
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

  message("Finished train_arima")
  return(model)
}


#' Forecast using ARIMA Model
#'
#' Generates forecasts from a trained ARIMA model.
#'
#' @param model A fitted ARIMA model object from `train_arima`.
#' @param horizon Integer, number of periods to forecast ahead.
#'
#' @return A tibble with columns: 'ds', 'yhat', 'yhat_lower_80', 'yhat_upper_80',
#'   'yhat_lower_95', 'yhat_upper_95'. Returns NULL on error.
#'
#' @noRd
#'
#' @import forecast
#' @import dplyr
#' @import tibble
#' @importFrom stats time frequency cycle
forecast_arima <- function(model, total_periods_needed, train_end_date, freq_str = "day") {
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
  fcst <- NULL
  tryCatch({
    fcst <- forecast::forecast(model, h = total_periods_needed, level = c(80, 95))
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

    # Determine the time unit based on frequency string
    by_period <- switch(freq_str,
                        "day" = lubridate::days(1),
                        "week" = lubridate::weeks(1),
                        lubridate::days(1) # Default
    )
    # Calculate the first date of the forecast period
    start_forecast_date <- train_end_date + by_period

    # Generate the date sequence for the forecast horizon
    forecast_dates <- seq.Date(from = start_forecast_date, by = freq_str, length.out = total_periods_needed)

    # --- End Improved Date Calculation ---

    # Ensure dates vector length matches forecast length
    if(length(forecast_dates) != length(fcst$mean)) {
      stop("Generated forecast dates length does not match forecast horizon.")
    }

    tibble::tibble(
      ds = forecast_dates, # Use the reliably generated dates
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
#' Trains an ETS model using forecast::ets or forecast::stlm (for high freq).
#' Allows automatic or manual selection for the underlying ETS model.
#'
#' @param train_df Tibble with 'ds' (Date) and 'y' (numeric) columns.
#' @param config List containing ETS parameters: manual (logical), ets_e, ets_t, ets_s
#'   components ('Z','A','M','N'), ets_damped_str ('NULL', 'TRUE', 'FALSE').
#' @param aggregation_level Character string indicating data frequency ('Daily', 'Weekly').
#'
#' @return A fitted model object ('ets' or 'stlm'). Returns NULL on error.
#' @noRd
#' @import forecast dplyr lubridate stats
train_ets <- function(train_df, config, aggregation_level, total_periods_needed) { # config currently unused
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



#' Forecast using ETS Model OR Extract from STLF Result
#'
#' Generates forecasts from a trained ETS model or extracts forecast/fitted
#' values from an STLF result object.
#'
#' @param model_or_fcst A fitted 'ets' model object OR a 'forecast' object from `train_ets`.
#' @param total_periods_needed Integer, TOTAL number of periods forecast is needed for.
#' @param train_end_date The last date present in the training data.
#' @param freq_str Character string frequency ('day', 'week').
#'
#' @return A list containing `$forecast` (tibble) and `$fitted` (vector). Returns NULL on error.
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
#' Trains a TBATS model using forecast::tbats.
#'
#' @param train_df Tibble with 'ds' (Date) and 'y' (numeric) columns.
#' @param config List for potential future TBATS configurations (currently unused).
#' @param aggregation_level Character string indicating data frequency ('Daily', 'Weekly').
#'
#' @return A fitted TBATS model object (from forecast package). Returns NULL on error.
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
    seasonal_periods_hint <- c(7, 365.25) # Hint for weekly and yearly seasonality
    message("TBATS: Setting ts frequency=7 for daily data. Hinting seasonal_periods: c(7, 365.25)")
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
    }, error = function(e) {
      warning_message <- paste(
        "Inner tbats() call failed. Error: ", conditionMessage(e),
        "\n  Data rows: ", nrow(train_df),
        ", TS Freq: ", stats::frequency(y_ts),
        ", Seasonal Hint: ", if (is.null(seasonal_periods_hint)) "NULL" else paste(seasonal_periods_hint, collapse=", ")
      )
      warning(warning_message)
      return(NULL) # Return NULL from inner tryCatch
    })

    if (is.null(model)) {
      message("tbats() returned NULL. Model fitting failed.")
    } else if (!inherits(model, "tbats")) {
      warning("TBATS: forecast::tbats() returned an invalid object (not class 'tbats'). Model fitting failed.")
      model <- NULL # Ensure model is NULL if not a valid tbats object
    } else {
      message("tbats() finished.")
      message("TBATS model fitted successfully.") # Added success message
      # You can print details of the fitted model components if needed:
      # print(model)
    }

  }, error = function(e) { # Outer tryCatch, should ideally not be hit if inner one catches
    warning(paste("Outer TBATS model training tryCatch failed:", conditionMessage(e)))
    model <<- NULL
  })

  message("Finished train_tbats")
  return(model)
  }


#' Forecast using TBATS Model
#'
#' Generates forecasts from a trained TBATS model.
#'
#' @param model A fitted TBATS model object from `train_tbats`.
#' @param total_periods_needed Integer, TOTAL number of periods to forecast ahead.
#' @param train_end_date The last date present in the training data.
#' @param freq_str Character string frequency ('day', 'week').
#'
#' @return A list containing `$forecast` (tibble) and `$fitted` (vector). Returns NULL on error.
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
    fitted_vals <- stats::fitted(fcst) # Fitted values from forecast object
    message("forecast.tbats() finished.")
  }, error = function(e) {
    warning(paste("TBATS forecast generation failed:", conditionMessage(e)))
    fcst <<- NULL; fitted_vals <<- NULL
  })

  if(is.null(fcst)) {
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
#' Trains a Prophet model.
#'
#' @param holidays_df Optional dataframe for holidays (cols: holiday, ds, [lower_window, upper_window]).
#' @param regressors_df Optional dataframe for regressors (cols: ds, regressor1, ...).
#' @param regressor_names Character vector of regressor column names to use from regressors_df.
#' @param train_df Tibble with 'ds' (Date) and 'y' (numeric) columns. If growth='logistic',
#'   it must also contain a 'cap' column.
#' @param config List containing Prophet parameters: yearly, weekly, daily seasonality (logical),
#'   growth ('linear'/'logistic'), changepoint_scale.
#'
#' @return A fitted Prophet model object. Returns NULL on error.
#'
#' @noRd
#'
#' @import prophet
#' @import dplyr
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
#' Generates forecasts from a trained Prophet model.
#'
#' @param regressors_df Optional dataframe containing *future* values for regressors.
#' @param regressor_names Character vector of regressor column names (must match training).
#' @param model A fitted Prophet model object from `train_prophet`.
#' @param periods_to_generate Integer, TOTAL number of periods to generate *after*
#'   the end of the training data (should cover test set + future horizon).
#' @param freq Character string for frequency ('day', 'week', 'month', etc.).
#' @param capacity Numeric or NULL. The capacity value for logistic growth.
#'
#' @return A tibble with columns 'ds', 'yhat', 'yhat_lower', 'yhat_upper'. Returns NULL on error.
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
#' Trains an XGBoost model using prepared data from a recipe.
#'
#' @param prep_recipe A *prepared* recipe object from `create_xgb_recipe`.
#' @param config List containing XGBoost hyperparameters: nrounds, eta, max_depth,
#'   subsample, colsample_bytree, gamma.
#'
#' @return A fitted XGBoost model object (xgb.Booster). Returns NULL on error.
#'
#' @noRd
#'
#' @import xgboost
#' @import recipes
#' @import dplyr
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

    # Train the model
    model <- xgboost::xgb.train(
      params = parameters,
      data = dtrain,
      nrounds = config$nrounds,
      verbose = 1 # Set to 1 or 2 for training progress messages
    )

    message("train_xgboost 2")


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
#'
#' Generates forecasts from a trained XGBoost model using a prepared recipe.
#'
#' @param model A fitted XGBoost model object (`xgb.Booster`).
#' @param prep_recipe The *prepared* recipe used for training.
#' @param full_df The original dataframe (post-cleaning/aggregation but pre-split)
#'   containing 'ds' and 'y', used to generate lags/features for future points.
#' @param horizon Integer, number of periods to forecast ahead.
#' @param freq Character string frequency ('day', 'week', etc.) for date sequence generation.
#'
#' @return A tibble with 'ds' and 'yhat' columns. Returns NULL on error. Confidence
#'   intervals are not naturally produced by standard XGBoost.
#'
#' @noRd
#'
#' @import xgboost recipes dplyr tibble
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
    future_matrix <- as.matrix(future_features_baked[, model_features, drop = FALSE]) # Select and order

    message("forecast XGBoost 1")


    # 7. Predict
    predictions <- predict(model, future_matrix)
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

#' Prepare data and features for GAM model
#' @param df Dataframe with ds, y
#' @return Dataframe with added features: time_index, year, month, week, yday, wday
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
#' Trains a GAM model using mgcv::gam.
#'
#' @param train_df Tibble with 'ds' (Date) and 'y' (numeric) columns.
#' @param config List containing GAM parameters: smooth_trend (logical),
#'   use_season_y (logical, for yearly), use_season_w (logical, for weekly).
#'
#' @return A fitted GAM model object (from mgcv package). Returns NULL on error.
#' @noRd
#' @import mgcv dplyr
train_gam <- function(train_df, config) {
  # Basic validation
  if (!is.data.frame(train_df) || !all(c("ds", "y") %in% names(train_df)) || nrow(train_df) < 10) {
    warning("train_df for GAM invalid or too short. Need >= 10 rows.")
    return(NULL)
  }

  model <- NULL
  tryCatch({
    message("Preparing features for GAM...")
    feature_df <- prepare_gam_features(train_df)

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

    # --- KEEP Weekly Seasonality Commented Out ---
    # if (config$use_season_w && length(unique(feature_df$wday)) > 1) {
    #      k_weekly <- min(length(unique(feature_df$wday)) - 1, 4); if (k_weekly < 3) k_weekly <- 3
    #      if (k_weekly >= 3) { formula_str <- paste0(formula_str, " + s(wday, bs='cc', k=", k_weekly, ")") }
    # }
    # --- END Weekly Seasonality ---

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

    message(paste("Fitting GAM with formula:", formula_str))
    gam_formula <- stats::as.formula(formula_str)
    # --- End Build Formula ---

    # Fit the model
    model <- mgcv::gam(gam_formula, data = feature_df, method = "REML") # REML often preferred

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
#' Generates forecasts from a trained GAM model.
#'
#' @param model A fitted GAM model object from `train_gam`.
#' @param train_df The original training dataframe (used for context, e.g., last date, feature generation).
#' @param total_periods_needed Integer, TOTAL number of periods to forecast ahead.
#' @param freq_str Character string frequency ('day', 'week').
#' @param config List containing GAM parameters used for training (needed if features depend on config).
#'
#' @return A list containing `$forecast` (tibble) and `$fitted` (vector). Returns NULL on error.
#' @noRd
#' @import mgcv dplyr tibble lubridate stats
forecast_gam <- function(model, train_df, total_periods_needed, freq_str = "day", config) {
  # Basic validation
  if (is.null(model) || !inherits(model, "gam")) { return(NULL) }
  # ... other validation ...

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
      # Re-create date components needed by the fitted model formula
      # (Need to match features used in train_gam EXACTLY)
      dplyr::mutate(
        # year = lubridate::year(ds), # Only if used in formula
        # month = factor(lubridate::month(ds, label = TRUE)), # Only if used
        # week = lubridate::week(ds), # Only if used
        yday = lubridate::yday(ds), # If s(yday) used
        wday = factor(lubridate::wday(ds, label = TRUE, week_start = 1), # If s(wday) or wday used
                      levels = levels(prepare_gam_features(train_df)$wday)) # Ensure factor levels match training data!
      )
    # Add future values for regressors here if implemented later
    # --- End Create Future Dataframe ---
    message(paste("Created future dataframe for prediction. Dims:", paste(dim(future_df), collapse=" x ")))


    # --- Predict with Confidence Intervals ---
    message("Predicting with GAM (se.fit=TRUE)...")
    # Use se.fit=TRUE to get standard errors for CI calculation
    preds <- predict(model, newdata = future_df, type = "response", se.fit = TRUE)

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
    # Predict on training features (already includes date components)
    # It's often safer to regenerate training features to ensure consistency
    train_feature_df <- prepare_gam_features(train_df)
    fitted_vals <- predict(model, newdata = train_feature_df, type = "response")
    message("GAM fitted values obtained.")
    # --- End Fitted Values ---

  }, error = function(e) {
    warning(paste("GAM forecast generation failed:", conditionMessage(e)))
    fcst_df <<- NULL; fitted_vals <<- NULL
  })

  return(list(forecast = fcst_df, fitted = fitted_vals))
}
#' Train Random Forest Model
#'
#' Trains a Random Forest model using the ranger package.
#'
#' @param prep_recipe A *prepared* recipe object from `create_tree_recipe`.
#' @param config List containing RF hyperparameters: num_trees, mtry (0 for auto), min_node_size.
#'
#' @return A fitted ranger model object. Returns NULL on error.
#' @noRd
#' @import ranger recipes dplyr tibble # Ensure imports
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
#' Generates forecasts from a trained ranger model using a prepared recipe.
#'
#' @param model A fitted ranger model object.
#' @param prep_recipe The *prepared* recipe used for training.
#' @param full_df The original dataframe (post-cleaning/agg) for feature generation history.
#' @param train_end_date The last date present in the training data.
#' @param total_periods_needed Integer, TOTAL number of periods to forecast ahead.
#' @param freq_str Character string frequency ('day', 'week').
#'
#' @return A list containing `$forecast` (tibble) and `$fitted` (vector). Returns NULL on error.
#' @noRd
#' @import ranger recipes dplyr tibble lubridate stats # Ensure imports
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
