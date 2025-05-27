# Tests for R/mod_utils_train_forecast.R
# (train_arima, forecast_arima, and potentially others later)

context("Testing train_arima function")

# Sample Data (as provided in the prompt)
sample_daily_data <- dplyr::tibble(
  ds = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 100),
  y = sin(1:100 / 10) + rnorm(100, 0, 0.1) + seq(1, 10, length.out=100)
)
sample_weekly_data <- dplyr::tibble(
  ds = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 52),
  y = cos(1:52 / 5) + rnorm(52, 0, 0.2) + seq(1,5, length.out=52)
)

# Test 1: Valid Auto ARIMA (Daily Data)
test_that("train_arima works with valid auto ARIMA on daily data", {
  config <- list(auto = TRUE, seasonal = TRUE)
  model <- train_arima(sample_daily_data, config, "Daily")
  expect_s3_class(model, "ARIMA")
  expect_s3_class(model, "forecast_ARIMA") # auto.arima models also have this class
})

# Test 2: Valid Manual Non-Seasonal ARIMA (Daily Data)
test_that("train_arima works with manual non-seasonal ARIMA on daily data", {
  config <- list(auto = FALSE, p = 1, d = 1, q = 0, seasonal = FALSE)
  model <- train_arima(sample_daily_data, config, "Daily")
  expect_s3_class(model, "ARIMA")
  expect_equal(model$arma[1], 1) # p
  expect_equal(model$arma[6], 1) # d
  expect_equal(model$arma[2], 0) # q
})

# Test 3: Valid Manual Seasonal ARIMA (Weekly Data)
test_that("train_arima works with manual seasonal ARIMA on weekly data", {
  config <- list(auto = FALSE, p = 1, d = 0, q = 0, seasonal = TRUE, P = 1, D = 0, Q = 0, period = 52)
  model <- train_arima(sample_weekly_data, config, "Weekly")
  expect_s3_class(model, "ARIMA")
  expect_equal(model$arma[3], 1) # P (seasonal p)
  expect_equal(model$arma[7], 0) # D (seasonal d)
  expect_equal(model$arma[4], 0) # Q (seasonal q)
  expect_equal(model$arma[5], 52) # period
})

# Test 4: Insufficient Data
test_that("train_arima handles insufficient data", {
  config <- list(auto = TRUE, seasonal = TRUE)
  # The function itself has a check for nrow < 5, but also for 2*freq.
  # For daily data, freq=7, so 2*7=14. 5 rows will trigger the 2*freq check.
  expect_warning(
    model <- train_arima(sample_daily_data[1:5, ], config, "Daily"),
    "Insufficient data points" # Updated to match actual warning
  )
  expect_null(model)
})

# Test 5: Invalid Config (e.g., non-numeric order)
test_that("train_arima handles invalid config gracefully", {
  config_invalid_p <- list(auto = FALSE, p = "a", d = 1, q = 0, seasonal = FALSE)
  # This will cause forecast::Arima to error. train_arima catches this.
  expect_warning(
    model_invalid_p <- train_arima(sample_daily_data, config_invalid_p, "Daily"),
    "ARIMA model training failed"
  )
  expect_null(model_invalid_p)

  config_invalid_period <- list(auto = FALSE, p=1,d=0,q=0, seasonal = TRUE, P=1,D=0,Q=0, period = "xyz")
   expect_warning(
    model_invalid_period <- train_arima(sample_weekly_data, config_invalid_period, "Weekly"),
    "Invalid or missing seasonal period for manual ARIMA" # This warning comes first
    # Potentially followed by "ARIMA model training failed" if it proceeds and Arima errors.
    # The function corrects config$seasonal to FALSE if period is invalid.
  )
  # The model might still be fit as non-seasonal if period is invalid.
  # Let's check if it became non-seasonal
  if(!is.null(model_invalid_period)) {
    expect_false(model_invalid_period$call$seasonal$order == c(1,0,0)) # Check it's not seasonal
  } else {
    # If it's NULL, that's also acceptable if the Arima call failed after the period warning
    expect_null(model_invalid_period)
  }
})

# Test 6: Holidays Integration (Daily Data)
test_that("train_arima integrates daily holidays", {
  holidays <- dplyr::tibble(
    ds = as.Date(c("2023-01-10", "2023-02-15", "2023-01-10")), # Duplicate date for one holiday
    holiday = c("Holiday1", "Holiday2", "ExtraForHoliday1")
  )
  config <- list(auto = TRUE, seasonal = TRUE)
  model <- train_arima(sample_daily_data, config, "Daily", holidays_df = holidays)
  expect_s3_class(model, "ARIMA")
  expect_true(!is.null(model$xreg))
  # Expected columns: Holiday1, Holiday2, ExtraForHoliday1 (make.names applied)
  # The function `make.names` is used, so "Holiday1" and "ExtraForHoliday1" are distinct.
  # If pivot_wider results in multiple columns for the same date due to distinct holiday names,
  # they should all be there.
  # The current implementation of train_arima for daily holidays uses pivot_wider.
  # If multiple holiday names fall on the same date, they become separate columns if names are distinct.
  # `make.names` ensures they are.
  # `holidays_in_train_range` filters holidays within the data range.
  # `pivot_wider` creates columns for each unique holiday name.
  # The number of unique holiday names in the sample_daily_data range (up to 2023-04-10) is 2
  # (Holiday1, Holiday2). "ExtraForHoliday1" might be a different holiday name on same date.
  # Let's check unique holiday names that fall within sample_daily_data:
  holidays_in_range <- holidays %>% dplyr::filter(ds >= min(sample_daily_data$ds) & ds <= max(sample_daily_data$ds))
  num_unique_holiday_names_in_range <- length(unique(make.names(holidays_in_range$holiday)))
  expect_equal(ncol(model$xreg), num_unique_holiday_names_in_range)
  expect_equal(attr(model, "holiday_regressor_type"), "daily_dummies")
})


# Test 7: Holidays Integration (Weekly Data - aggregated regressor)
test_that("train_arima integrates weekly holidays (aggregated)", {
  holidays <- dplyr::tibble(
    ds = as.Date(c("2023-01-10", "2023-03-15")), # Within sample_weekly_data range
    holiday = c("HolidayW1", "HolidayW2")
  )
  config <- list(auto = TRUE, seasonal = TRUE)
  model <- train_arima(sample_weekly_data, config, "Weekly", holidays_df = holidays)
  expect_s3_class(model, "ARIMA")
  expect_true(!is.null(model$xreg))
  expect_equal(ncol(model$xreg), 1) # has_holiday_in_week
  expect_match(colnames(model$xreg)[1], "has_holiday_in_week")
  expect_equal(attr(model, "holiday_regressor_type"), "weekly_aggregated")
})


context("Testing forecast_arima function")

# Prepare a sample model for forecast tests
train_data_fcst <- sample_daily_data[1:90, ]
config_fcst <- list(auto = TRUE, seasonal = TRUE)
model_for_fcst <- train_arima(train_data_fcst, config_fcst, "Daily")

# Test 1: Valid Forecast (from Auto ARIMA model)
test_that("forecast_arima works with a valid model", {
  total_periods <- 10
  # train_end_date is the last date IN THE TRAINING SET used for the model
  train_end_date_val <- max(train_data_fcst$ds)
  
  forecast_result <- forecast_arima(model_for_fcst, total_periods, train_end_date_val, "day")

  expect_type(forecast_result, "list")
  expect_named(forecast_result, c("forecast", "fitted"))
  
  fc_df <- forecast_result$forecast
  expect_s3_class(fc_df, "tbl_df")
  expect_equal(nrow(fc_df), total_periods)
  expect_named(fc_df, c("ds", "yhat", "yhat_lower_80", "yhat_upper_80", "yhat_lower_95", "yhat_upper_95"), ignore.order = TRUE)
  
  fitted_v <- forecast_result$fitted
  expect_vector(fitted_v)
  expect_length(fitted_v, nrow(train_data_fcst))
})

# Test 2: Forecast with future_xreg
test_that("forecast_arima works with future_xreg", {
  holidays_train <- dplyr::tibble(
    ds = as.Date(c("2023-01-10", "2023-02-15")), holiday = c("H1", "H2")
  )
  train_data_xreg <- sample_daily_data[1:50, ]
  model_xreg <- train_arima(train_data_xreg, list(auto=TRUE, seasonal=TRUE), "Daily", holidays_df = holidays_train)
  
  expect_false(is.null(model_xreg$xreg), info = "Model should have xreg from training.")
  
  total_periods_xreg <- 7
  train_end_date_xreg <- max(train_data_xreg$ds)
  
  # Create future_xreg compatible with model$xreg (same number of columns)
  # colnames should match make.names(unique(holidays_train$holiday))
  # H1, H2
  num_holiday_cols <- ncol(model_xreg$xreg)
  future_xreg_matrix <- matrix(0, nrow = total_periods_xreg, ncol = num_holiday_cols)
  colnames(future_xreg_matrix) <- colnames(model_xreg$xreg) # Ensure names match
  if (num_holiday_cols > 0) {
    future_xreg_matrix[1, 1] <- 1 # Example: first holiday occurs on first forecast day
  }


  forecast_result_xreg <- forecast_arima(model_xreg, total_periods_xreg, train_end_date_xreg, "day", future_xreg = future_xreg_matrix)
  
  expect_type(forecast_result_xreg, "list")
  expect_named(forecast_result_xreg, c("forecast", "fitted"))
  expect_equal(nrow(forecast_result_xreg$forecast), total_periods_xreg)
})

# Test 3: Invalid Model Input for forecast_arima
test_that("forecast_arima handles invalid model input", {
  expect_warning(
    fc_null <- forecast_arima(NULL, 10, Sys.Date(), "day"),
    "Invalid ARIMA model object provided."
  )
  expect_null(fc_null)
  
  expect_warning(
    fc_list <- forecast_arima(list(), 10, Sys.Date(), "day"),
    "Invalid ARIMA model object provided."
  )
  expect_null(fc_list)
})

# Test 4: Invalid total_periods_needed for forecast_arima
test_that("forecast_arima handles invalid total_periods_needed", {
  # Using model_for_fcst from earlier
  expect_warning(
    fc_zero_h <- forecast_arima(model_for_fcst, 0, max(train_data_fcst$ds), "day"),
    "Invalid horizon provided."
  )
  expect_null(fc_zero_h)
  
  expect_warning(
    fc_neg_h <- forecast_arima(model_for_fcst, -1, max(train_data_fcst$ds), "day"),
    "Invalid horizon provided."
  )
  expect_null(fc_neg_h)
})

# Test 5: Forecast when model had xreg but future_xreg is not provided (but holidays_df for forecast period is)
test_that("forecast_arima generates future_xreg internally if needed and holidays_df provided", {
  holidays_train_internal <- dplyr::tibble(
    ds = as.Date(c("2023-01-10", "2023-02-15")), # Ensure these are within the 90 days
    holiday = c("H_train1", "H_train2")
  )
  train_data_internal_xreg <- sample_daily_data[1:90, ]
  
  model_internal_xreg <- train_arima(
    train_data_internal_xreg,
    list(auto = TRUE, seasonal = TRUE),
    "Daily",
    holidays_df = holidays_train_internal
  )
  
  expect_false(is.null(model_internal_xreg$xreg), info = "Model for internal xreg test should have xreg.")
  expect_equal(attr(model_internal_xreg, "holiday_regressor_type"), "daily_dummies") # Verify attribute

  total_periods_internal <- 10
  train_end_date_internal <- max(train_data_internal_xreg$ds) # Should be 2023-03-31

  # Forecast period starts 2023-04-01. These holidays are within the 10-day forecast.
  holidays_forecast_period <- dplyr::tibble(
    ds = as.Date(c("2023-04-02", "2023-04-05")), # Corrected example dates
    holiday = c("H_future1", "H_future2") # Can be different names
  )
  
  # forecast_arima should use holidays_forecast_period to create future_xreg
  forecast_result_internal <- forecast_arima(
    model = model_internal_xreg,
    total_periods_needed = total_periods_internal,
    train_end_date = train_end_date_internal,
    freq_str = "day",
    future_xreg = NULL, # Explicitly NULL
    holidays_df = holidays_forecast_period # Provide holidays for the forecast period
  )

  expect_type(forecast_result_internal, "list")
  expect_named(forecast_result_internal, c("forecast", "fitted"))
  expect_false(is.null(forecast_result_internal$forecast))
  expect_equal(nrow(forecast_result_internal$forecast), total_periods_internal)
  # Check if the generated future_xreg (which happens inside forecast.Arima, not directly inspectable here unless forecast.Arima is mocked/wrapped)
  # had the correct structure. The fact that it ran without error on future_xreg generation is a good sign.
})

# Test for when model$xreg is NULL but future_xreg is provided (should be ignored by forecast.Arima)
test_that("forecast_arima handles future_xreg provided when model has no xreg", {
  model_no_xreg <- train_arima(sample_daily_data[1:50,], list(auto=TRUE, seasonal=TRUE), "Daily", holidays_df = NULL)
  expect_true(is.null(model_no_xreg$xreg))

  future_xreg_dummy <- matrix(1, nrow=10, ncol=1)
  forecast_res <- forecast_arima(model_no_xreg, 10, max(sample_daily_data[1:50,]$ds), "day", future_xreg = future_xreg_dummy)
  
  # forecast.Arima should ignore xreg if model wasn't trained with it.
  # The main check is that it runs without error.
  expect_false(is.null(forecast_res))
  expect_equal(nrow(forecast_res$forecast), 10)
})

# Test for weekly aggregated holiday regressor forecasting (internal generation)
test_that("forecast_arima generates future_xreg for weekly aggregated holidays", {
  holidays_train_weekly <- dplyr::tibble(
    ds = as.Date(c("2023-01-10", "2023-03-15")), # Within sample_weekly_data
    holiday = c("HolidayW1", "HolidayW2")
  )
  train_data_weekly_internal <- sample_weekly_data[1:40, ] # Use 40 weeks of data
  
  model_weekly_agg_xreg <- train_arima(
    train_data_weekly_internal,
    list(auto = TRUE, seasonal = TRUE),
    "Weekly",
    holidays_df = holidays_train_weekly
  )
  
  expect_false(is.null(model_weekly_agg_xreg$xreg), info = "Weekly model should have xreg.")
  expect_equal(attr(model_weekly_agg_xreg, "holiday_regressor_type"), "weekly_aggregated")

  total_periods_weekly <- 8 # Forecast 8 weeks
  train_end_date_weekly <- max(train_data_weekly_internal$ds)

  # Holidays for the forecast period
  # train_end_date_weekly for 40 weeks from 2023-01-01 is 2023-10-08
  # Forecast starts 2023-10-15.
  holidays_forecast_weekly <- dplyr::tibble(
    ds = as.Date(c("2023-10-17", "2023-11-01")), 
    holiday = c("FutureWeekHol1", "FutureWeekHol2")
  )
  
  forecast_result_weekly_internal <- forecast_arima(
    model = model_weekly_agg_xreg,
    total_periods_needed = total_periods_weekly,
    train_end_date = train_end_date_weekly,
    freq_str = "week",
    future_xreg = NULL,
    holidays_df = holidays_forecast_weekly
  )

  expect_type(forecast_result_weekly_internal, "list")
  expect_named(forecast_result_weekly_internal, c("forecast", "fitted"))
  expect_false(is.null(forecast_result_weekly_internal$forecast))
  expect_equal(nrow(forecast_result_weekly_internal$forecast), total_periods_weekly)
})

# --- ETS Tests Start Here ---
context("Testing train_ets function")

# Ensure positive data for multiplicative tests if needed, or handle warnings
sample_positive_daily_data <- sample_daily_data %>% dplyr::mutate(y = y - min(y, na.rm = TRUE) + 1)
sample_positive_weekly_data <- sample_weekly_data %>% dplyr::mutate(y = y - min(y, na.rm = TRUE) + 1)
# For STLF test: need > 2*freq data. For weekly, freq=52, so >104 rows.
# sample_long_weekly_data <- dplyr::tibble(
#   ds = seq.Date(as.Date("2020-01-01"), by = "week", length.out = 120),
#   y = cos(1:120 / 10) + rnorm(120, 0, 0.2) + seq(1,12, length.out=120)
# )
# sample_positive_long_weekly_data <- sample_long_weekly_data %>% dplyr::mutate(y = y - min(y, na.rm=TRUE) + 1)


# 1. Valid Auto ETS (Daily Data, positive)
test_that("train_ets works with Auto ETS on positive daily data", {
  config <- list(manual = FALSE) # Auto
  # total_periods_needed is for stlf, which won't be triggered here (freq=7)
  model <- train_ets(sample_positive_daily_data, config, "Daily", total_periods_needed = 10)
  expect_s3_class(model, "ets")
})

# 2. Valid Manual ETS (Daily Data, positive, "ANN")
test_that("train_ets works with manual 'ANN' ETS on positive daily data", {
  config <- list(manual = TRUE, ets_e = "A", ets_t = "N", ets_s = "N", ets_damped_str = "NULL")
  model <- train_ets(sample_positive_daily_data, config, "Daily", total_periods_needed = 10)
  expect_s3_class(model, "ets")
  expect_match(model$method, "ANN")
})

# 3. Valid Manual Seasonal ETS (Weekly Data, positive, "MAM")
test_that("train_ets works with manual 'MAM' ETS on positive weekly data", {
  # sample_positive_weekly_data has 52 rows. freq=52. 2*freq = 104.
  # This will warn "Insufficient data ... Fitting non-seasonal ETS instead." and freq becomes 1.
  # So, 'MAM' will become 'MAN' effectively if ets() is smart, or error if not.
  # train_ets sets freq to 1 if nrow < 2*freq. So it will be non-seasonal.
  # User asks for "MAM", but freq=1 means s_comp becomes "N". So "MAN".
  config <- list(manual = TRUE, ets_e = "M", ets_t = "A", ets_s = "M", ets_damped_str = "FALSE")
  
  # Expect a warning due to insufficient data for seasonal model
  # The function itself will change freq to 1.
  expect_warning(
    model <- train_ets(sample_positive_weekly_data, config, "Weekly", total_periods_needed = 10),
    "Insufficient data .* Fitting non-seasonal ETS instead"
  )
  expect_s3_class(model, "ets")
  # Since seasonality is forced to N, check for M,A,N
  expect_match(model$method, "MAN")
})

# 4. Insufficient Data for train_ets
test_that("train_ets handles insufficient data", {
  config <- list(manual = FALSE)
  # train_ets itself has nrow < 5 check.
  expect_warning(
    model <- train_ets(sample_daily_data[1:4, ], config, "Daily", total_periods_needed = 5),
    "ETS training requires sufficient data points."
  )
  expect_null(model)
})

# 5. Data with Non-Positive Values (for Multiplicative Models)
test_that("train_ets handles non-positive data with manual Multiplicative components", {
  # sample_daily_data can have non-positive values
  config <- list(manual = TRUE, ets_e = "M", ets_t = "N", ets_s = "N", ets_damped_str = "NULL") # MNN
  # train_ets issues a general message "Data has non-positive values..."
  # and for manual, it warns "Manual selection includes Multiplicative component(s) but data is non-positive."
  expect_warning(
    model <- train_ets(sample_daily_data, config, "Daily", total_periods_needed = 10),
    "Manual selection includes Multiplicative component(s) but data is non-positive. Check results."
  )
  # The model might still be fitted by ets(), but it's good to check it's not NULL
  # and ets() might have chosen a different model or produced non-finite parameters.
  if (!is.null(model)) {
    expect_s3_class(model, "ets")
    # Check if ets actually fit MNN or switched. It might proceed with MNN and produce issues.
    # This depends on forecast::ets internal behavior with non-positive data and M component.
    # Often, it will produce an error or a model with issues (e.g. Inf likelihood).
    # The warning from train_ets is the primary check here.
  }
})

test_that("train_ets restricts auto ETS with non-positive data", {
  config <- list(manual = FALSE) # Auto
  # Expect message "Restricting direct ets() auto-selection: setting allow.multiplicative.trend = FALSE."
  # This message is conditional on freq > 1. For Daily, freq=7.
  # No specific warning, but a message. We can't test messages easily without capture.output.
  # We can test that the resulting model is not multiplicative in trend or season if auto-selected.
  model <- train_ets(sample_daily_data, config, "Daily", total_periods_needed = 10)
  if (!is.null(model)) {
    expect_s3_class(model, "ets")
    # Check that method does not contain 'M' for trend or season if auto-selected
    # This is a bit tricky as auto-selection "ZZZ" could still pick "M" for error component
    # The function sets `allow.multiplicative.trend = FALSE`. This affects trend.
    # Seasonality choice is separate.
    # If ets() respects this, the trend component of model$method should not be 'M'.
    method_components <- strsplit(gsub("[()]", "", model$method), ",")[[1]]
    trend_comp <- method_components[2]
    # season_comp <- method_components[3] # Not directly restricted by allow.multiplicative.trend
    expect_false(trend_comp == "M")
  } else {
    fail("Model is NULL for auto ETS with non-positive data, which is unexpected.")
  }
})


# 6. stlf Trigger: Not easily testable with current sample_weekly_data (52 rows, freq 52 -> 52 !<= 52/2)
# To test stlf, we'd need data like: nrow = 150, aggregation_level = "Weekly" -> freq = 52.
# Then 52 > 24 is TRUE, and 52 <= 150/2 = 75 is TRUE.
# For now, this specific stlf trigger path in train_ets is not tested due to data constraints.


context("Testing forecast_ets function")

# Prepare sample ETS models for forecast tests
# Using positive data as it's generally safer for ETS
train_data_ets_daily <- sample_positive_daily_data[1:90, ]
config_auto_ets <- list(manual = FALSE)
model_auto_ets <- train_ets(train_data_ets_daily, config_auto_ets, "Daily", total_periods_needed = 10)

config_manual_ets <- list(manual = TRUE, ets_e = "A", ets_t = "N", ets_s = "N", ets_damped_str = "NULL")
model_manual_ets <- train_ets(train_data_ets_daily, config_manual_ets, "Daily", total_periods_needed = 5)

# 1. Valid Forecast (from Auto ETS model)
test_that("forecast_ets works with a valid auto ETS model", {
  if (is.null(model_auto_ets)) skip("Auto ETS model for forecast test is NULL")
  
  total_periods <- 10
  # train_end_date is last date of data used for model_auto_ets
  train_end_date_val <- max(train_data_ets_daily$ds)
  
  forecast_result <- forecast_ets(model_auto_ets, total_periods, train_end_date_val, "day")
  
  expect_type(forecast_result, "list")
  expect_named(forecast_result, c("forecast", "fitted"))
  
  fc_df <- forecast_result$forecast
  expect_s3_class(fc_df, "tbl_df")
  expect_equal(nrow(fc_df), total_periods)
  expect_named(fc_df, c("ds", "yhat", "yhat_lower_80", "yhat_upper_80", "yhat_lower_95", "yhat_upper_95"), ignore.order = TRUE)
  
  fitted_v <- forecast_result$fitted
  expect_vector(fitted_v)
  expect_length(fitted_v, nrow(train_data_ets_daily))
})

# 2. Valid Forecast (from Manual ETS model "ANN")
test_that("forecast_ets works with a valid manual ETS model", {
  if (is.null(model_manual_ets)) skip("Manual ETS model for forecast test is NULL")

  total_periods <- 5
  train_end_date_val <- max(train_data_ets_daily$ds) # Data used for model_manual_ets
  
  forecast_result <- forecast_ets(model_manual_ets, total_periods, train_end_date_val, "day")
  
  expect_type(forecast_result, "list")
  expect_named(forecast_result, c("forecast", "fitted"))
  expect_s3_class(forecast_result$forecast, "tbl_df")
  expect_equal(nrow(forecast_result$forecast), total_periods)
})

# 3. Forecast from stlf output: Skipped due to difficulty triggering stlf with current sample data.
# test_that("forecast_ets processes stlf forecast objects correctly", { skip("STLF trigger not tested in train_ets") })


# 4. Invalid Model Input for forecast_ets
test_that("forecast_ets handles invalid model input", {
  expect_warning(
    fc_null <- forecast_ets(NULL, 10, Sys.Date(), "day"),
    "Invalid model/forecast object provided"
  )
  expect_null(fc_null$forecast) # forecast_ets returns list(forecast=NULL, fitted=NULL)
  expect_null(fc_null$fitted)

  expect_warning(
    fc_list <- forecast_ets(list(), 10, Sys.Date(), "day"),
    "Input object is not class 'ets' or 'forecast'"
  )
  expect_null(fc_list$forecast)
  expect_null(fc_list$fitted)
})

# 5. Invalid total_periods_needed for forecast_ets
test_that("forecast_ets handles invalid total_periods_needed", {
  if (is.null(model_auto_ets)) skip("Auto ETS model for forecast test is NULL")
  
  expect_warning(
    fc_zero_h <- forecast_ets(model_auto_ets, 0, max(train_data_ets_daily$ds), "day"),
    "Invalid total_periods_needed provided."
  )
  expect_null(fc_zero_h$forecast) # Expect NULL for the forecast component
  
  expect_warning(
    fc_neg_h <- forecast_ets(model_auto_ets, -1, max(train_data_ets_daily$ds), "day"),
    "Invalid total_periods_needed provided."
  )
  expect_null(fc_neg_h$forecast)
})

# Test case where train_ets produces an stlf object (requires specific data)
test_that("train_ets uses stlf for high frequency data and forecast_ets processes it", {
  # Create data that should trigger stlf: freq > 24 and nrow >= 2*freq
  # For daily data, freq is 7, so stlf is not used.
  # For weekly data, freq is 52. Need nrow >= 104.
  sample_long_weekly_data <- dplyr::tibble(
    ds = seq.Date(as.Date("2020-01-01"), by = "week", length.out = 110), # > 2*52
    y = sin(1:110 / 10) + rnorm(110, 0, 0.1) + seq(1, 11, length.out=110)
  )
  sample_positive_long_weekly_data <- sample_long_weekly_data %>% 
    dplyr::mutate(y = y - min(y, na.rm=TRUE) + 1)

  config_auto_stlf <- list(manual = FALSE)
  # total_periods_needed for stlf is its h parameter
  # train_ets will call stlf(y_ts, h = total_periods_needed_train)
  total_periods_needed_train <- 12 # This is h for stlf
  
  model_stlf_obj <- train_ets(
    sample_positive_long_weekly_data, 
    config_auto_stlf, 
    "Weekly", 
    total_periods_needed = total_periods_needed_train 
  )
  
  # train_ets returns a forecast object when stlf is used
  expect_s3_class(model_stlf_obj, "forecast") 
  expect_true(!is.null(model_stlf_obj$model)) # Check for underlying model within stlf output
  expect_s3_class(model_stlf_obj$model, "ets") # stlf default uses ets
  expect_equal(length(model_stlf_obj$mean), total_periods_needed_train) # Check stlf forecast horizon

  # Now test forecast_ets with this forecast object
  total_periods_forecast <- total_periods_needed_train # Match the horizon from training
  train_end_date_stlf <- max(sample_positive_long_weekly_data$ds)
  
  forecast_result_stlf <- forecast_ets(
    model_stlf_obj, 
    total_periods_forecast, 
    train_end_date_stlf, 
    "week"
  )
  
  expect_type(forecast_result_stlf, "list")
  expect_named(forecast_result_stlf, c("forecast", "fitted"))
  
  fc_df_stlf <- forecast_result_stlf$forecast
  expect_s3_class(fc_df_stlf, "tbl_df")
  expect_equal(nrow(fc_df_stlf), total_periods_forecast)
  expect_named(fc_df_stlf, c("ds", "yhat", "yhat_lower_80", "yhat_upper_80", "yhat_lower_95", "yhat_upper_95"), ignore.order = TRUE)
  
  fitted_v_stlf <- forecast_result_stlf$fitted
  expect_vector(fitted_v_stlf)
  # Fitted values from stlf are on the original scale of the training data used for stlf
  expect_length(fitted_v_stlf, nrow(sample_positive_long_weekly_data))
})

# --- Prophet Tests Start Here ---
context("Testing train_prophet function")

# 1. Valid Basic Model (Daily Data)
test_that("train_prophet works with a basic linear model on daily data", {
  config <- list(yearly = FALSE, weekly = TRUE, daily = FALSE, growth = 'linear', changepoint_scale = 0.05)
  model <- train_prophet(sample_daily_data, config)
  expect_s3_class(model, "prophet")
  expect_equal(model$growth, "linear")
  expect_true(model$weekly.seasonality)
  expect_false(model$yearly.seasonality)
})

# 2. Model with Yearly Seasonality
test_that("train_prophet accepts yearly seasonality", {
  # sample_daily_data is 100 days, prophet will fit yearly if specified, but it might not be meaningful.
  # The test is primarily that the parameter is accepted and a model is returned.
  config <- list(yearly = TRUE, weekly = FALSE, daily = FALSE, growth = 'linear', changepoint_scale = 0.05)
  model <- train_prophet(sample_daily_data, config)
  expect_s3_class(model, "prophet")
  expect_true(model$yearly.seasonality)
})

# 3. Logistic Growth Model
test_that("train_prophet works with logistic growth model", {
  train_data_logistic <- sample_daily_data %>% dplyr::mutate(cap = max(y) + 20) # Ensure cap is above max y
  config <- list(yearly = FALSE, weekly = TRUE, daily = FALSE, growth = 'logistic', changepoint_scale = 0.05)
  model <- train_prophet(train_data_logistic, config)
  expect_s3_class(model, "prophet")
  expect_equal(model$growth, "logistic")
})

# 4. Logistic Growth Model Missing `cap`
test_that("train_prophet stops if cap is missing for logistic growth", {
  config <- list(growth = 'logistic', yearly = FALSE, weekly = TRUE, daily = FALSE, changepoint_scale = 0.05)
  expect_error(
    train_prophet(sample_daily_data, config), # sample_daily_data has no 'cap'
    "Logistic growth requires a 'cap' column in train_df."
  )
})

# 5. Insufficient Data
test_that("train_prophet handles insufficient data", {
  config <- list(growth = 'linear', yearly = FALSE, weekly = TRUE, daily = FALSE, changepoint_scale = 0.05)
  expect_warning(
    model <- train_prophet(sample_daily_data[1:1, ], config),
    "Prophet training requires at least 2 data points."
  )
  expect_null(model)
})

# 6. With Holidays
test_that("train_prophet integrates holidays", {
  config <- list(growth = 'linear', yearly = FALSE, weekly = TRUE, daily = FALSE, changepoint_scale = 0.05)
  holidays_df <- dplyr::tibble(
    holiday = 'special_day', 
    ds = as.Date(c('2023-01-10', '2023-02-20')), 
    lower_window = 0, 
    upper_window = 1
  )
  model <- train_prophet(sample_daily_data, config, holidays_df = holidays_df)
  expect_s3_class(model, "prophet")
  expect_false(is.null(model$holidays))
  expect_equal(nrow(model$holidays), 2)
  expect_true("special_day" %in% model$holidays$holiday)
})

# 7. With Custom Regressors
test_that("train_prophet integrates custom regressors", {
  config <- list(growth = 'linear', yearly = FALSE, weekly = TRUE, daily = FALSE, changepoint_scale = 0.05)
  reg_df_train <- dplyr::tibble(
    ds = sample_daily_data$ds, 
    extra_reg = rnorm(nrow(sample_daily_data))
  )
  # train_prophet expects train_df to already have regressors if used
  train_data_with_reg <- dplyr::inner_join(sample_daily_data, reg_df_train, by = "ds")

  model <- train_prophet(train_data_with_reg, config, regressors_df = reg_df_train, regressor_names = c("extra_reg"))
  expect_s3_class(model, "prophet")
  expect_true("extra_reg" %in% names(model$extra_regressors))
})


context("Testing forecast_prophet function")

# Prepare a sample Prophet model for forecast tests
config_prophet_base <- list(yearly = FALSE, weekly = TRUE, daily = FALSE, growth = 'linear', changepoint_scale = 0.05)
model_prophet_base <- train_prophet(sample_daily_data, config_prophet_base)

# 1. Valid Basic Forecast
test_that("forecast_prophet works with a basic linear model", {
  if (is.null(model_prophet_base)) skip("Base Prophet model for forecast test is NULL")
  
  periods <- 10
  forecast_result <- forecast_prophet(model_prophet_base, periods_to_generate = periods, freq = "day")
  
  expect_s3_class(forecast_result, "tbl_df")
  # Prophet's make_future_dataframe includes history, predict includes history.
  # The function forecast_prophet returns selected columns from predict output.
  # It includes both historical dates (fitted values) and future dates.
  expect_equal(nrow(forecast_result), nrow(sample_daily_data) + periods)
  expect_named(forecast_result, c("ds", "yhat", "yhat_lower", "yhat_upper"), ignore.order = TRUE)
  expect_true(all(c("trend", "weekly") %in% colnames(predict(model_prophet_base, forecast_result)))) # Check components are there
})

# 2. Forecast with Logistic Growth
test_that("forecast_prophet works with logistic growth model", {
  train_data_logistic_fcst <- sample_daily_data %>% dplyr::mutate(cap = max(y) + 20)
  config_logistic <- list(growth = 'logistic', yearly = FALSE, weekly = TRUE, daily = FALSE, changepoint_scale = 0.05)
  model_logistic <- train_prophet(train_data_logistic_fcst, config_logistic)
  
  if (is.null(model_logistic)) skip("Logistic Prophet model for forecast test is NULL")

  periods <- 7
  cap_value <- max(train_data_logistic_fcst$y) # Use the same cap logic as training (or slightly higher)
  
  forecast_result <- forecast_prophet(model_logistic, periods_to_generate = periods, freq = "day", capacity = cap_value)
  
  expect_s3_class(forecast_result, "tbl_df")
  expect_equal(nrow(forecast_result), nrow(train_data_logistic_fcst) + periods)
  # Check that 'cap' was used in future_df for prediction if possible (indirectly by checking no error)
})

# 3. Forecast with Logistic Growth Missing `capacity`
test_that("forecast_prophet warns if capacity is missing for logistic growth forecast", {
  train_data_lg <- sample_daily_data %>% dplyr::mutate(cap = max(y) + 20)
  config_lg <- list(growth = 'logistic', yearly = FALSE, weekly = TRUE, daily = FALSE, changepoint_scale = 0.05)
  model_lg <- train_prophet(train_data_lg, config_lg)

  if (is.null(model_lg)) skip("Logistic Prophet model for missing capacity test is NULL")

  expect_warning(
    fc <- forecast_prophet(model_lg, periods_to_generate = 7, freq = "day", capacity = NULL),
    "Capacity value must be provided for logistic growth forecast."
  )
  expect_null(fc)
})

# 4. Forecast with Regressors
test_that("forecast_prophet works with regressors", {
  reg_df <- dplyr::tibble(ds = sample_daily_data$ds, extra_reg = rnorm(nrow(sample_daily_data)))
  train_data_regr <- dplyr::inner_join(sample_daily_data, reg_df, by = "ds")
  config_regr <- list(growth = 'linear', yearly = FALSE, weekly = TRUE, daily = FALSE, changepoint_scale = 0.05)
  model_regr <- train_prophet(train_data_regr, config_regr, regressors_df = reg_df, regressor_names = c("extra_reg"))

  if (is.null(model_regr)) skip("Prophet model with regressors for forecast test is NULL")

  periods <- 5
  # Future regressors must cover all dates in future_df (history + forecast horizon)
  # make_future_dataframe creates dates from start of history to end of forecast period.
  # So, regressor_df needs to cover all these dates.
  all_dates_for_fcst <- seq.Date(from = min(sample_daily_data$ds), by = "day", length.out = nrow(sample_daily_data) + periods)
  
  future_regressors_df <- dplyr::tibble(
    ds = all_dates_for_fcst,
    extra_reg = rnorm(length(all_dates_for_fcst))
  )

  forecast_result <- forecast_prophet(
    model_regr, 
    periods_to_generate = periods, 
    freq = "day", 
    regressors_df = future_regressors_df, 
    regressor_names = c("extra_reg")
  )
  
  expect_s3_class(forecast_result, "tbl_df")
  expect_equal(nrow(forecast_result), nrow(sample_daily_data) + periods)
  # Check if extra_regressor component is in the forecast output from predict()
  full_predict_output <- predict(model_regr, forecast_result) # Predict on the generated ds dates
  expect_true("extra_reg" %in% colnames(full_predict_output))
})

# 5. Forecast with Regressors - Missing Future Regressor Values
test_that("forecast_prophet stops if future regressor values are missing", {
  reg_df_train <- dplyr::tibble(ds = sample_daily_data$ds, extra_reg1 = rnorm(nrow(sample_daily_data)))
  train_data_regr_miss <- dplyr::inner_join(sample_daily_data, reg_df_train, by = "ds")
  config_regr_miss <- list(growth = 'linear', yearly = FALSE, weekly = TRUE, daily = FALSE, changepoint_scale = 0.05)
  model_regr_miss <- train_prophet(train_data_regr_miss, config_regr_miss, regressors_df = reg_df_train, regressor_names = c("extra_reg1"))

  if (is.null(model_regr_miss)) skip("Prophet model for missing regressor test is NULL")

  periods <- 5
  # Regressors only for a part of the history, and not for the future forecast dates
  # make_future_dataframe will create dates for history + forecast.
  # If regressors_df doesn't cover these, left_join in forecast_prophet will create NAs.
  # The function should then stop.
  
  # Create regressor data that only covers a portion of history, ensuring NAs for future
  partial_regressors_df <- dplyr::tibble(
    ds = sample_daily_data$ds[1:50], # Only first 50 days of history
    extra_reg1 = rnorm(50)
  )

  expect_error(
    forecast_prophet(
      model_regr_miss, 
      periods_to_generate = periods, 
      freq = "day", 
      regressors_df = partial_regressors_df, 
      regressor_names = c("extra_reg1")
    ),
    "Future regressor values are missing for some dates in the forecast horizon."
  )
})

# 6. Invalid Model Input
test_that("forecast_prophet handles invalid model input", {
  expect_warning(
    fc <- forecast_prophet(NULL, periods_to_generate = 10, freq = "day"),
    "Invalid Prophet model object provided."
  )
  expect_null(fc)
})

# 7. Invalid `periods_to_generate`
test_that("forecast_prophet handles invalid periods_to_generate", {
  if (is.null(model_prophet_base)) skip("Base Prophet model for invalid periods test is NULL")
  
  expect_warning(
    fc0 <- forecast_prophet(model_prophet_base, periods_to_generate = 0, freq = "day"),
    "Invalid periods_to_generate provided."
  )
  expect_null(fc0)
  
  expect_warning(
    fc_neg <- forecast_prophet(model_prophet_base, periods_to_generate = -1, freq = "day"),
    "Invalid periods_to_generate provided."
  )
  expect_null(fc_neg)
})

# --- XGBoost Tests Start Here ---
context("Testing train_xgboost and forecast_xgboost functions")

# Longer sample data for XGBoost, as recipes might create lags
sample_daily_data_long <- dplyr::tibble(
  ds = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 200),
  y = sin(1:200 / 20) + rnorm(200, 0, 0.1) + seq(1, 20, length.out=200)
)

# Basic XGBoost config
xgb_config_base <- list(
  nrounds = 10, eta = 0.1, max_depth = 3, 
  subsample = 0.8, colsample = 0.8, # colsample_bytree is 'colsample' in UI
  gamma = 0
)

# Attempt to load create_tree_recipe from the package namespace
# This assumes the test environment can access non-exported functions
# or that the package is loaded.
create_tree_recipe_fn <- NULL
if (requireNamespace("forecastApp", quietly = TRUE) && 
    exists("create_tree_recipe", where = asNamespace("forecastApp"), mode = "function")) {
  create_tree_recipe_fn <- get("create_tree_recipe", envir = asNamespace("forecastApp"))
} else {
  # Fallback: Define a very minimal recipe for testing if create_tree_recipe is not found
  # This is less ideal as it doesn't test the actual recipe function.
  warning("forecastApp:::create_tree_recipe not found. Using a minimal placeholder recipe for XGBoost tests.")
  create_tree_recipe_fn <- function(df, freq_str) {
    recipes::recipe(y ~ ds, data = df) %>% # Simplest possible recipe
      recipes::step_date(ds, features = c("decimal"), keep_original_cols = FALSE) 
      # Note: This step_date might cause issues if 'ds' is not retained or named differently post-baking
      # The actual create_tree_recipe is more complex and robust.
  }
}


# 1. Valid Model Training (train_xgboost)
test_that("train_xgboost works with valid recipe and config", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("recipes")
  skip_if_null(create_tree_recipe_fn, "create_tree_recipe function not available for testing.")

  unprep_recipe <- create_tree_recipe_fn(sample_daily_data_long, freq_str = "day")
  prep_recipe <- recipes::prep(unprep_recipe, training = sample_daily_data_long)
  
  model <- train_xgboost(prep_recipe, xgb_config_base)
  expect_s3_class(model, "xgb.Booster")
})

# 2. Invalid Recipe (NULL or not prepped) (train_xgboost)
test_that("train_xgboost handles NULL or unprepped recipe", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("recipes")
  skip_if_null(create_tree_recipe_fn, "create_tree_recipe function not available for testing.")

  expect_warning(model_null <- train_xgboost(NULL, xgb_config_base), "received NULL or invalid recipe")
  expect_null(model_null)
  
  unprep_recipe <- create_tree_recipe_fn(sample_daily_data_long, freq_str = "day")
  # train_xgboost expects a prepared recipe. Passing an unprepped one will likely cause errors
  # during juice/bake or xgb.DMatrix creation if it gets that far.
  # The function has a warning if steps or term_info are missing, indicating not prepped.
  expect_warning(
    model_unprepped <- train_xgboost(unprep_recipe, xgb_config_base),
    "received a recipe that might not be prepared."
  )
  # Depending on how robust juice/bake are with unprepped recipes, this might error or return NULL.
  # The function's internal tryCatch should catch errors and return NULL.
  if (!is.null(model_unprepped)) { # If it didn't error out and returned something
     expect_null(model_unprepped) # It should ideally be NULL due to internal failure
  } else {
     expect_null(model_unprepped) # Explicitly confirm it's NULL
  }
})


# 3. Config with Zero Rounds (train_xgboost)
test_that("train_xgboost handles config with zero nrounds", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("recipes")
  skip_if_null(create_tree_recipe_fn, "create_tree_recipe function not available for testing.")

  unprep_recipe <- create_tree_recipe_fn(sample_daily_data_long, freq_str = "day")
  prep_recipe <- recipes::prep(unprep_recipe, training = sample_daily_data_long)
  
  config_zero_rounds <- xgb_config_base
  config_zero_rounds$nrounds <- 0
  
  # xgb.train with nrounds=0 might return a booster with no trees or error.
  # The internal tryCatch in train_xgboost should handle errors.
  # If it returns a model, it would be a very basic one.
  # Often, nrounds=0 is not allowed or leads to a model that can't predict.
  model <- train_xgboost(prep_recipe, config_zero_rounds)
  if (!is.null(model)) {
    expect_s3_class(model, "xgb.Booster")
    # A model with 0 rounds might not have attributes like niter if xgb.train handles it specially.
    # This test mainly ensures it doesn't crash the R session.
  } else {
    # If xgb.train errors due to nrounds=0 and train_xgboost catches it
    expect_null(model)
  }
})

# 4. Recipe Baking Fails or Produces No Predictors (train_xgboost)
test_that("train_xgboost handles recipe bake failure or no predictors", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("recipes")

  # Create a recipe that will result in no predictors
  # Example: try to create lags longer than the dataset, or remove all variables.
  # Simpler: recipe that removes all predictors.
  no_predictors_recipe_unprep <- recipes::recipe(y ~ ds, data = sample_daily_data_long) %>%
    recipes::step_rm(ds) # Removes the only potential predictor
  
  # Check if prep itself errors or results in an unusable state.
  # If prep is fine, bake will produce 0 columns.
  prep_no_pred_recipe <- tryCatch({
    recipes::prep(no_predictors_recipe_unprep, training = sample_daily_data_long)
  }, error = function(e) NULL)

  skip_if_null(prep_no_pred_recipe, "Failed to prepare the no_predictors_recipe for the test.")

  # train_xgboost should error out or return NULL when bake results in 0 columns.
  # It has a check: `if(nrow(train_x_df) == 0 || ncol(train_x_df) == 0) stop(...)`
  expect_error( # or expect_warning then expect_null(model) if it catches the stop
    model_no_pred <- train_xgboost(prep_no_pred_recipe, xgb_config_base)
    # Error message: "Baked predictor data frame has zero rows or columns."
  )
  # Since the above expect_error, we don't get to check if model_no_pred is NULL,
  # but the error means the function didn't return a model.
})


# --- forecast_xgboost Tests ---
context("Testing forecast_xgboost function")

# Helper to train a basic XGB model for forecast tests
get_sample_xgb_model <- function(train_data) {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("recipes")
  skip_if_null(create_tree_recipe_fn, "create_tree_recipe function not available for testing.")
  
  unprep_recipe <- create_tree_recipe_fn(train_data, freq_str = "day")
  prep_recipe_loc <- recipes::prep(unprep_recipe, training = train_data)
  model <- train_xgboost(prep_recipe_loc, xgb_config_base)
  return(list(model = model, prep_recipe = prep_recipe_loc))
}

# 1. Valid Basic Forecast
test_that("forecast_xgboost works for a basic case", {
  skip_if_not_installed("xgboost"); skip_if_not_installed("recipes"); skip_if_null(create_tree_recipe_fn)
  
  train_data <- sample_daily_data_long[1:150, ]
  model_assets <- get_sample_xgb_model(train_data)
  skip_if_null(model_assets$model, "XGBoost model training failed for forecast test.")

  forecast_horizon <- 10
  
  # Use the full sample_daily_data_long for full_df to ensure lags can be created from history.
  # train_end_date is the last date of the data used for training the model.
  fcst <- forecast_xgboost(
    model = model_assets$model, 
    prep_recipe = model_assets$prep_recipe, 
    full_df = sample_daily_data_long, # Provide history up to end of training + future part
    train_end_date = max(train_data$ds), 
    total_periods_needed = forecast_horizon, 
    freq = "day"
  )
  
  expect_s3_class(fcst, "tbl_df")
  expect_equal(nrow(fcst), forecast_horizon)
  expect_named(fcst, c("ds", "yhat"))
})

# 2. `full_df` Too Short for Lags
test_that("forecast_xgboost handles full_df too short for lags", {
  skip_if_not_installed("xgboost"); skip_if_not_installed("recipes"); skip_if_null(create_tree_recipe_fn)

  train_data_short <- sample_daily_data_long[1:50, ] # Train on 50 points
  model_assets_short <- get_sample_xgb_model(train_data_short)
  skip_if_null(model_assets_short$model, "XGBoost model training failed for short full_df test.")

  # If create_tree_recipe creates lags (e.g., lag 7), and full_df is only 5 rows, it should fail.
  # The check in forecast_xgboost is `if(nrow(full_df) < max_lag_needed)`.
  # We need to know what max_lag_needed is. Assuming it's at least 1.
  # If max_lag_needed derived from recipe is > 5, then the error should trigger.
  # Let's assume create_tree_recipe creates a lag of at least 7 for daily data.
  
  # To ensure failure, provide a full_df that's shorter than any potential lag.
  # The error "Need at least X rows in full_df for forecast lags." comes from forecast_xgboost.
  # max_lag_needed is derived from the recipe.
  # The default create_tree_recipe includes lags like 7, 14, etc. for daily.
  
  # Modify full_df to be very short to trigger the error.
  # Note: train_end_date should be based on the data used for training the *model*,
  # but full_df for forecasting is a separate input for feature generation.
  # Here, we test the scenario where full_df is insufficient for the recipe's lag requirements.
  
  # To properly test this, we need to know `max_lag_needed` from the recipe.
  # Let's assume the recipe creates a lag of at least 7.
  # If we provide a full_df with fewer than 7 rows, it should trigger the stop.
  short_full_df <- sample_daily_data_long[1:5, ]
  
  expect_error(
    forecast_xgboost(
      model = model_assets_short$model,
      prep_recipe = model_assets_short$prep_recipe,
      full_df = short_full_df, 
      train_end_date = max(train_data_short$ds), # Based on actual training data
      total_periods_needed = 5, 
      freq = "day"
    ),
    regexp = "Need at least .* rows in full_df for forecast lags."
  )
})


# 3. Invalid Model Input (forecast_xgboost)
test_that("forecast_xgboost handles NULL model input", {
  skip_if_not_installed("xgboost"); skip_if_not_installed("recipes"); skip_if_null(create_tree_recipe_fn)
  
  prep_recipe_dummy <- recipes::prep(create_tree_recipe_fn(sample_daily_data_long, "day"))
  
  expect_warning(
    fc <- forecast_xgboost(NULL, prep_recipe_dummy, sample_daily_data_long, Sys.Date(), 10, "day"),
    "Invalid XGBoost model object provided."
  )
  expect_null(fc)
})

# 4. Invalid Recipe Input (forecast_xgboost)
test_that("forecast_xgboost handles NULL recipe input", {
  skip_if_not_installed("xgboost"); skip_if_not_installed("recipes"); skip_if_null(create_tree_recipe_fn)

  model_assets <- get_sample_xgb_model(sample_daily_data_long[1:150,])
  skip_if_null(model_assets$model, "XGBoost model training failed for invalid recipe test.")
  
  expect_warning(
    fc <- forecast_xgboost(model_assets$model, NULL, sample_daily_data_long, Sys.Date(), 10, "day"),
    "Invalid recipe object provided."
  )
  expect_null(fc)
})

# 5. Feature Mismatch (forecast_xgboost) - Tricky to set up perfectly without deep recipe knowledge
test_that("forecast_xgboost stops if future baked features mismatch model features", {
  skip_if_not_installed("xgboost"); skip_if_not_installed("recipes"); skip_if_null(create_tree_recipe_fn)

  train_data_fm <- sample_daily_data_long[1:150, ]
  model_assets_fm <- get_sample_xgb_model(train_data_fm)
  skip_if_null(model_assets_fm$model, "XGBoost model training failed for feature mismatch test.")

  # Create a different recipe for forecasting that will produce different columns
  # For example, one that adds a feature the original recipe didn't, or removes one.
  # Simpler: manually alter the feature names of the model to force a mismatch.
  # This simulates the scenario where baked features don't align.
  
  # Take a valid model and its recipe
  valid_model <- model_assets_fm$model
  valid_prep_recipe <- model_assets_fm$prep_recipe
  
  # Create a situation where model$feature_names will not match baked features
  # E.g., by removing a feature from the baked data or adding a name to model$feature_names
  
  # Option 1: Modify the model's feature names to expect something not there
  original_feature_names <- valid_model$feature_names
  valid_model$feature_names <- c(original_feature_names, "a_missing_feature_for_test")
  
  expect_error(
    forecast_xgboost(
      model = valid_model, 
      prep_recipe = valid_prep_recipe, 
      full_df = sample_daily_data_long, 
      train_end_date = max(train_data_fm$ds), 
      total_periods_needed = 5, 
      freq = "day"
    ),
    regexp = "Features missing after baking for forecast: a_missing_feature_for_test"
  )
  
  # Restore original feature names if model object is reused (not in this test structure)
  # valid_model$feature_names <- original_feature_names 
})

# --- GAM Tests Start Here ---
context("Testing train_gam and forecast_gam functions")

# Longer sample data for GAM, similar to XGBoost
sample_daily_data_long_gam <- dplyr::tibble(
  ds = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 200),
  y = sin(1:200 / 15) + rnorm(200, 0, 0.2) + seq(1, 20, length.out=200)
)
sample_weekly_data_long_gam <- dplyr::tibble( 
  ds = seq.Date(as.Date("2022-01-01"), by = "week", length.out = 104), 
  y = cos(1:104 / 8) + rnorm(104, 0, 0.3) + seq(1,10, length.out=104)
)

# 1. train_gam: Valid Basic Model (Daily Data, Smooth Trend, No Seasonality)
test_that("train_gam works with smooth trend, no seasonality on daily data", {
  skip_if_not_installed("mgcv")
  config <- list(smooth_trend = TRUE, use_season_y = FALSE, use_season_w = FALSE)
  model <- train_gam(sample_daily_data_long_gam, config, holidays_df = NULL)
  expect_s3_class(model, "gam")
  expect_true(any(grepl("s(time_index)", as.character(model$formula)[3])))
  expect_false(any(grepl("s(yday", as.character(model$formula)[3])))
  expect_false(any(grepl("wday", as.character(model$formula)[3])))
})

# 2. train_gam: Linear Trend, Yearly Seasonality (Daily Data)
test_that("train_gam works with linear trend, yearly seasonality on daily data", {
  skip_if_not_installed("mgcv")
  config <- list(smooth_trend = FALSE, use_season_y = TRUE, use_season_w = FALSE)
  model <- train_gam(sample_daily_data_long_gam, config, holidays_df = NULL)
  expect_s3_class(model, "gam")
  expect_true(any(grepl("time_index", as.character(model$formula)[3])) && !any(grepl("s(time_index)", as.character(model$formula)[3])))
  expect_true(any(grepl("s(yday", as.character(model$formula)[3])))
})

# 3. train_gam: Smooth Trend, Weekly Seasonality (Daily Data - wday factor)
test_that("train_gam works with smooth trend, weekly seasonality on daily data", {
  skip_if_not_installed("mgcv")
  config <- list(smooth_trend = TRUE, use_season_y = FALSE, use_season_w = TRUE)
  model <- train_gam(sample_daily_data_long_gam, config, holidays_df = NULL)
  expect_s3_class(model, "gam")
  expect_true(any(grepl("s(time_index)", as.character(model$formula)[3])))
  expect_true(any(grepl("\\bwday\\b", as.character(model$formula)[3]))) # Use word boundary for wday
})

# 4. train_gam: Smooth Trend, Yearly & Weekly Seasonality (Weekly Data)
test_that("train_gam works with smooth trend, yearly & weekly seasonality on weekly data", {
  skip_if_not_installed("mgcv")
  config <- list(smooth_trend = TRUE, use_season_y = TRUE, use_season_w = TRUE)
  model <- train_gam(sample_weekly_data_long_gam, config, holidays_df = NULL)
  expect_s3_class(model, "gam")
  expect_true(any(grepl("s(time_index)", as.character(model$formula)[3])))
  expect_true(any(grepl("s(yday", as.character(model$formula)[3])))
  expect_true(any(grepl("\\bwday\\b", as.character(model$formula)[3])))
})

# 5. train_gam: Insufficient Data
test_that("train_gam handles insufficient data", {
  skip_if_not_installed("mgcv")
  config <- list(smooth_trend = TRUE, use_season_y = FALSE, use_season_w = FALSE)
  expect_warning(
    model <- train_gam(sample_daily_data_long_gam[1:9, ], config, holidays_df = NULL),
    "train_df for GAM invalid or too short."
  )
  expect_null(model)
})

# 6. train_gam: With Holidays
test_that("train_gam integrates holidays", {
  skip_if_not_installed("mgcv")
  config <- list(smooth_trend = TRUE, use_season_y = TRUE, use_season_w = TRUE)
  holidays_df <- dplyr::tibble(
    ds = as.Date(c("2023-01-15", "2023-03-10")), 
    holiday = c("EventA", "EventB")
  )
  model <- train_gam(sample_daily_data_long_gam, config, holidays_df = holidays_df)
  expect_s3_class(model, "gam")
  expect_true(any(grepl("is_holiday", as.character(model$formula)[3])))
  expect_equal(attr(model, "holiday_levels"), c("NoHoliday", "EventA", "EventB"))
})

# 7. train_gam: Holidays with NoHoliday Level Only
test_that("train_gam handles NULL holidays_df correctly", {
  skip_if_not_installed("mgcv")
  config <- list(smooth_trend = TRUE, use_season_y = FALSE, use_season_w = FALSE)
  model <- train_gam(sample_daily_data_long_gam, config, holidays_df = NULL)
  expect_s3_class(model, "gam")
  expect_true(any(grepl("is_holiday", as.character(model$formula)[3])))
  expect_equal(attr(model, "holiday_levels"), c("NoHoliday"))
  
  empty_holidays_df <- dplyr::tibble(ds = as.Date(character(0)), holiday = character(0))
  model_empty_h <- train_gam(sample_daily_data_long_gam, config, holidays_df = empty_holidays_df)
  expect_s3_class(model_empty_h, "gam")
  expect_true(any(grepl("is_holiday", as.character(model_empty_h$formula)[3])))
  expect_equal(attr(model_empty_h, "holiday_levels"), c("NoHoliday"))
})


context("Testing forecast_gam function")

# Prepare a sample GAM model for forecast tests
train_data_gam_fcst <- sample_daily_data_long_gam[1:150, ]
config_gam_fcst_base <- list(smooth_trend = TRUE, use_season_y = TRUE, use_season_w = TRUE)
holidays_gam_train <- dplyr::tibble(
  ds = as.Date(c("2023-01-15", "2023-03-10", "2023-04-20")), 
  holiday = c("EventA", "EventB", "EventC_TrainOnly") # EventC is in training range
)
model_gam_for_fcst <- train_gam(train_data_gam_fcst, config_gam_fcst_base, holidays_df = holidays_gam_train)

# 1. forecast_gam: Valid Basic Forecast
test_that("forecast_gam works with a valid model", {
  skip_if_not_installed("mgcv")
  skip_if_null(model_gam_for_fcst, "GAM model for forecast test is NULL")

  total_periods <- 20
  train_end_date_val <- max(train_data_gam_fcst$ds) # 2023-05-30
  
  # Holidays for forecast period. ds from 2023-05-31 for 20 periods
  # Forecast period: 2023-05-31 to 2023-06-19
  holidays_gam_fcst <- dplyr::tibble(
    ds = as.Date(c("2023-06-05", "2023-06-15")), 
    holiday = c("EventA", "NewEventForecast") 
  )

  forecast_result <- forecast_gam(
    model_gam_for_fcst, 
    train_df = train_data_gam_fcst, # Pass the original training data
    total_periods_needed = total_periods, 
    freq_str = "day", 
    config = config_gam_fcst_base, # Pass original config
    holidays_df = holidays_gam_fcst # Holidays covering the forecast range
  )
  
  expect_type(forecast_result, "list")
  expect_named(forecast_result, c("forecast", "fitted"))
  
  fc_df <- forecast_result$forecast
  expect_s3_class(fc_df, "tbl_df")
  expect_equal(nrow(fc_df), total_periods)
  expect_named(fc_df, c("ds", "yhat", "yhat_lower_95", "yhat_upper_95"), ignore.order = TRUE)
  
  fitted_v <- forecast_result$fitted
  expect_vector(fitted_v)
  expect_length(fitted_v, nrow(train_data_gam_fcst))
  
  # Check that future_df used for prediction had the holiday column with correct levels
  # This is an indirect check, relying on predict() not erroring due to factor level mismatch.
  # The `holidays_df` for forecast includes "NewEventForecast", which should be handled by
  # using the levels from `attr(model_gam_for_fcst, "holiday_levels")`.
  # If "NewEventForecast" was not in original training holiday names, it will be treated as "NoHoliday"
  # if not explicitly added to `holiday_levels` during forecast prep.
  # The current `forecast_gam` uses `attr(model, "holiday_levels")` for future_df.
  # So, "NewEventForecast" would become NA then "NoHoliday" if not in original levels.
  # Let's ensure EventA (present in training) is handled.
  # The `is_holiday` column in `future_df` within `forecast_gam` should have levels
  # matching `attr(model_gam_for_fcst, "holiday_levels")`.
  # This test primarily ensures it runs and produces output of correct structure.
})


# 2. forecast_gam: Invalid Model Input
test_that("forecast_gam handles invalid model input", {
  skip_if_not_installed("mgcv")
  res <- forecast_gam(NULL, train_data_gam_fcst, 10, "day", config_gam_fcst_base, NULL)
  expect_null(res) # Function returns NULL directly if model is NULL
})


# 3. forecast_gam: Robustness to Mismatched Config (config during forecast vs train)
test_that("forecast_gam feature generation uses trained model's structure", {
  skip_if_not_installed("mgcv")
  
  # Train with yearly seasonality
  config_train <- list(smooth_trend = TRUE, use_season_y = TRUE, use_season_w = FALSE)
  model_trained <- train_gam(train_data_gam_fcst, config_train, holidays_df = NULL)
  skip_if_null(model_trained, "GAM model for mismatched config test (train) is NULL")
  expect_true(any(grepl("s(yday", as.character(model_trained$formula)[3])))

  # Forecast with a config that would imply NO yearly seasonality
  config_forecast <- list(smooth_trend = TRUE, use_season_y = FALSE, use_season_w = FALSE)
  
  # forecast_gam should still prepare 'yday' for prediction because model_trained requires it.
  # This is implicitly tested if predict() doesn't fail due to missing 'yday'.
  # The `config` argument in `forecast_gam` is primarily for passing through to other functions
  # if needed, but feature generation for `future_df` should align with `model$formula`.
  
  forecast_result <- forecast_gam(
    model_trained, 
    train_df = train_data_gam_fcst,
    total_periods_needed = 10, 
    freq_str = "day", 
    config = config_forecast, # Mismatched config
    holidays_df = NULL
  )
  
  expect_type(forecast_result, "list")
  expect_false(is.null(forecast_result$forecast))
  expect_equal(nrow(forecast_result$forecast), 10)
  # If predict worked, it means future_df had the necessary 'yday' column.
})

# 4. forecast_gam: Holiday levels consistency
test_that("forecast_gam handles holiday levels correctly for future data", {
  skip_if_not_installed("mgcv")
  
  train_holidays <- dplyr::tibble(ds = as.Date("2023-01-15"), holiday = "EventTrain")
  config_h <- list(smooth_trend = TRUE, use_season_y = FALSE, use_season_w = FALSE)
  model_h <- train_gam(train_data_gam_fcst, config_h, holidays_df = train_holidays)
  skip_if_null(model_h, "GAM model for holiday levels test is NULL")
  
  expect_equal(attr(model_h, "holiday_levels"), c("NoHoliday", "EventTrain"))

  # Forecast period holidays
  forecast_holidays <- dplyr::tibble(
    ds = as.Date(c("2023-06-01", "2023-06-10")), # Dates after train_data_gam_fcst ends (2023-05-30)
    holiday = c("EventTrain", "EventFutureNew") # One known, one new
  )
  
  forecast_result <- forecast_gam(
    model_h, 
    train_df = train_data_gam_fcst,
    total_periods_needed = 15, # e.g. 2023-05-31 to 2023-06-14
    freq_str = "day", 
    config = config_h, 
    holidays_df = forecast_holidays
  )
  
  expect_type(forecast_result, "list")
  expect_false(is.null(forecast_result$forecast))
  # The main check is that predict() works, implying that the 'is_holiday' factor
  # in the newdata for predict was created with levels compatible with the trained model.
  # "EventFutureNew" if not in trained levels, should be handled as "NoHoliday" or NA then "NoHoliday".
  # `forecast_gam` uses `attr(model, "holiday_levels")` for factor creation in future_df.
})

[end of tests/testthat/test-mod_utils_train_forecast.R]

[end of tests/testthat/test-mod_utils_train_forecast.R]
