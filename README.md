
# forecastApp

`{forecastApp}` provides an interactive Shiny application for time
series forecasting. It allows users to upload daily data, preprocess it
(including daily/weekly aggregation and train/test splitting), configure
and run standard forecasting models (ARIMA, Prophet, XGBoost), visualize
results, and evaluate model performance.

## Installation

You can install the development version of forecastApp from
[GitHub](https://github.com/) (replace with your actual link if you host
it there) with:

``` r
# install.packages("remotes") # If you don't have remotes installed
# Replace <YOUR_GITHUB_USERNAME> with your actual username/organization
# remotes::install_github("<YOUR_GITHUB_USERNAME>/forecastApp") 
```

*Note:* Installation might require dependent packages available on CRAN.
Some underlying packages (like `xgboost`) might have system dependencies
(e.g., a C++ compiler). Installation from source might also require
Rtools (Windows) or Xcode Command Line Tools (macOS).

Alternatively, if you have built the package tarball (e.g.,
`forecastApp_0.0.0.9000.tar.gz` using `devtools::build()`):

``` r
# install.packages("path/to/forecastApp_0.0.0.9000.tar.gz", repos = NULL, type = "source")
```

## Example Usage

Once installed, you can run the Shiny application with:

``` r
library(forecastApp)
# Run the main application
forecastApp::run_app()
```

**Customize:**

- Replace `<YOUR_GITHUB_USERNAME>` with your actual GitHub username if
  you plan to host the code there. If not, you can remove the GitHub
  installation part or adjust it.
- Add any other relevant badges if you set up continuous integration or
  other services.
- Expand the description or usage example if needed.
