leaflet_fun_gen <- function(data, raster, domain, cols, cols_rev, tit_leg) {
  
  
  map <- leaflet(
    data =  data,
    options = leafletOptions(
      minZoom = 6, maxZoom = 12
    ) 
  ) %>%
    leaflet.extras::addBootstrapDependency() %>%
    setView(25, 46, zoom = 6) %>%
    setMaxBounds(20, 43.5, 30, 48.2) |>
    addMapPane(name = "granita", zIndex = 410) %>%
    addMapPane(name = "maplabels", zIndex = 420) %>%
    addProviderTiles( "CartoDB.PositronNoLabels")   %>% 
    addEasyButton(
      easyButton (
        icon    = "glyphicon glyphicon-home", title = "Reset zoom",
        onClick = JS("function(btn, map){ map.setView([46, 25], 6); }")
      )
    )   %>%
    addPolylines(
      color = "#444444", weight = 1, smoothFactor = 0.5,
      options = pathOptions(pane = "granita"),
      group = "Granița") |>
    addRasterImage(
      raster,
      colors = cols, opacity = .8
    ) %>%
    addLayersControl(
      baseGroups = "CartoDB.PositronNoLabels",
      overlayGroups = c("Labels", "Granița"))  %>% 
    addProviderTiles(
      "CartoDB.PositronOnlyLabels",
      options = pathOptions(pane = "maplabels"),
      group = "Labels"
    ) %>%
    addScaleBar(
      position = c("bottomleft"),
      options = scaleBarOptions(metric = TRUE)) |>
    clearControls() |>
    addLegend(
      title =  paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), tit_leg,"</html>"),
      position = "bottomright",
      pal = cols_rev, values = domain,
      opacity = 1,
      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
    )
  return(map)
}