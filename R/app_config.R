#' Access files in the current app
#'
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#' For= example, if your package name is "pkg", changed
#' "forecastApp" to "pkg" in the following file calls.
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
#' @importFrom shiny fluidPage column
app_sys <- function(...){
  system.file(..., package = "forecastApp")
}


#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config GOLEM_CONFIG_ACTIVE value.
#' If unset, R_CONFIG_ACTIVE. If unset, "default".
#' @param use_parent Logical,
#' scan the parent directory for config file.
#' @param file Location of the config file
#' @noRd
#' @importFrom config get
#' @importFrom yaml read_yaml
#' @importFrom golem get_golem_options
get_golem_config <- function(
    value,
    config = Sys.getenv("GOLEM_CONFIG_ACTIVE", Sys.getenv("R_CONFIG_ACTIVE", "default")),
    use_parent = TRUE
    # Modify this if your config file is somewhere else
){
  config::get(
    value = value,
    config = config,
    # file = file,
    file = app_sys("golem-config.yml"),
    use_parent = use_parent
  )
}
