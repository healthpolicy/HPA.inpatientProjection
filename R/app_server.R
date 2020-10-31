#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny leaflet
#' @noRd
app_server <- function( input, output, session ) {
  output$map <- leaflet::renderLeaflet({
    leaflet(usmap_shape) %>%
      addProviderTiles("CartoDB.DarkMatter") %>% 
      addPolygons(fill = ~divx) %>% 
      fitBounds(lng1 = -117.055243, lat1 = 53.662740, lng2 = -20.242797, lat2 = 5.152411)
  })
}
