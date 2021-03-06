#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom leaflet leafletOutput
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    leaflet::leafletOutput("map", height = "100vh"),
    absolutePanel(
      id = "controls", class = "panel panel-default", 
      fixed = TRUE,
      top = "5vh", height = "95vh",
      left = "auto", right = 20, width = "45vw", 
      
      h3("Projection"),
      "Test",
      h3("Comparison"),
      "Test"
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'HPA.inpatientProjection'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

