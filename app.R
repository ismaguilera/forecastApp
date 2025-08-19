# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

# This file is the entry point for shinyapps.io
# It ensures the package is loaded before running the app.

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

# Set options for Golem app in production
options("golem.app.prod" = TRUE)

# Detach all loaded packages and clean the environment
golem::detach_all_attached()
rm(list = ls(all.names = TRUE))

# Document and reload the package
golem::document_and_reload()

# Run the application
# run_app()
# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file
forecastApp::run_app() # Reemplaza "forecastApp" con el nombre de tu paquete si es diferente
