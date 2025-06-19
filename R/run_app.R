#' Run the datadashboard
#'
#' @export
run_app <- function(){
  shiny::shinyApp(ui = app_ui(), server = server())
}
