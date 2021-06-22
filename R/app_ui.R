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
    ),
    absolutePanel(
      id = "valuebox", class = "panel panel-default",
      fixed = TRUE,
      top = "70vh", height = "20vh",
      left = 20, width = "50vw",
      
      fluidRow(
        style = "color:white",
        div(
          style = "width:33%;float:left",
          tags$h1("16"),
          tags$p("Average", tags$br(), " length of stay")
        ),
        div(
          style = "width:33%;float:left",
          tags$h1("16"),
          tags$p("% sameday", tags$br(), "episodes")
        ),
        div(
          style = "width:33%;float:left",
          tags$h1("16"),
          tags$p("Relative", tags$br(), "utilisation")
        )
      )
      
      # ,
      # 
      # fluidRow(
      #   column(
      #     4,
      #     "x"
      #   ),
      #   column(
      #     4,
      #     "x"
      #   ),
      #   column(
      #     4,
      #     "x"
      #   )  
      # )
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

