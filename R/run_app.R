#' Run the datadashboard
#'
#' @export
run_app <- function(){
  shiny::shinyApp(ui = app_ui(), server = app_server, options = list(launch.browser = TRUE))
}
