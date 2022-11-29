leaflet_fun_det <- function(data) {
  
  map <- leaflet(
    data =  data,
    options = leafletOptions(
      minZoom = 6, maxZoom = 12
    )) |>
    clearShapes() %>%
    addPolygons (
      fillColor = ~qpal(value), 
      label = ~paste("<font size='2'><b>Region type: UAT<br/>Name units:",name_1,
                     "<br/>",round(value,1),"</b></font><br/>
                       <font size='1' color='#E95420'>Click to
                       get values and graph</font>") %>% lapply(htmltools::HTML),
      labelOptions = labelOptions(textsize = "13px"),
      color = "grey",
      weight = 0.5, smoothFactor = 0.1,
      opacity = 0.5, 
      # fillOpacity = opacy ,
      layerId = ~natCode,
      # options = pathOptions(pane = "pol"),
      #group = "region",
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        fillOpacity = 0.2,
        bringToFront = TRUE,
        sendToBack = TRUE)) |>
    setView(25, 46, zoom = 6) |>
    setMaxBounds(20, 43.5, 30, 48.2) |>
    addEasyButton(
      easyButton (
        icon    = "glyphicon glyphicon-home", title = "Reset zoom",
        onClick = JS("function(btn, map){ map.setView([46, 25], 6); }"))) |>
    addMapPane(name = "maplabels", zIndex = 420) |>
    addProviderTiles( "CartoDB.PositronNoLabels") |>
    addProviderTiles(
      "CartoDB.PositronOnlyLabels",
      options = pathOptions(pane = "maplabels"),
      group = "Etichete") |>
    addLayersControl(
      baseGroups = "CartoDB.PositronNoLabels",
      overlayGroups = c("Etichete")) |>
    addScaleBar(
      position = c("bottomleft"),
      options = scaleBarOptions(metric = TRUE)) 
  return(map)
  
}