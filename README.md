# forecastApp: A Modular Shiny Forecasting Application

`{forecastApp}` is an interactive Shiny application for time series forecasting. It provides a user-friendly interface to upload data, configure models, run forecasts, and evaluate results.

## Features

- **Data Handling**: Upload your own daily time series data in CSV format.
- **Preprocessing**: Aggregate data from daily to weekly, and automatically split into training and testing sets.
- **Interactive Visualization**: Decompose time series, view autocorrelation plots (ACF/PACF), and analyze results with interactive graphs.
- **Modeling**: Configure and run a suite of standard forecasting models:
  - ARIMA
  - Prophet
  - XGBoost
- **Evaluation**: Assess model performance with common accuracy metrics.

## Architecture

This application is built using the [Golem](https://engineering-shiny.org/golem.html) framework, which structures the Shiny app as a robust and scalable R package. The functionality is broken down into discrete Shiny modules for better code organization and maintenance.

## Getting Started

### Installation

You can install the development version of `{forecastApp}` from GitHub. You will need the `remotes` package.

```r
# install.packages("remotes") # If you don't have remotes installed
# Replace <YOUR_GITHUB_USERNAME> with the actual username/organization
remotes::install_github("<YOUR_GITHUB_USERNAME>/forecastApp")
```

**Dependencies:** The app requires several R packages available on CRAN. Some underlying packages (like `prophet` or `xgboost`) may have additional system dependencies (e.g., a C++ compiler). For installation from source, you may need Rtools (Windows) or Xcode Command Line Tools (macOS).

### Usage

Once the package is installed, you can run the application with a single command:

```r
library(forecastApp)
forecastApp::run_app()
```

## Development

This application is structured as a Golem package. To run the app in development mode, you can open the RStudio project and run the following script:

```r
source("dev/run_dev.R")
```