leaflet_fun_det <- function(data) {
  
  map <- leaflet(
    data =  data,
    options = leafletOptions(
      minZoom = 6, maxZoom = 12
    )) |>
    clearShapes() %>%
    addPolygons (
      fillColor = ~qpal(value), 
      # label = ~paste("<font size='2'><b>Region type:",level_ag()$reg_name, "<br/>Name units:",name,
      #                "</b></font><br/>
      #                  <font size='1' color='#E95420'>Click to 
      #                  get values and graph</font>") %>% lapply(htmltools::HTML),
      #  labelOptions = labelOptions(textsize = "13px"),
      color = "grey",
      weight = 0.5, smoothFactor = 0.1,
      opacity = 0.5, 
      # fillOpacity = opacy ,
      #layerId = ~code,
      #options = pathOptions(pane = "pol"),
      #group = "region",
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        fillOpacity = 0.2,
        bringToFront = TRUE,
        sendToBack = TRUE
      ) 
    ) |>
    leaflet.extras::addBootstrapDependency() %>%
    setView(25, 46, zoom = 6) %>%
    setMaxBounds(20, 43.5, 30, 48.2) |>
    addProviderTiles( "CartoDB.PositronNoLabels")
  
  return(map)
  
}
