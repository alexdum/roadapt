
# harta leaflet -----------------------------------------------------------
agr_rea <- reactive({
  
  indic <- input$agr_ind
  scena <-  input$agr_scen
  perio <- strsplit(input$agr_perio, "-")[[1]][2]
  nc <- raster::brick(paste0("www/data/ncs/",indic,"Adjust_",scena,"_",perio,"-50_19710101_21001231.nc"))
  nc
  
})


output$test <- renderText({
  paste("You chose", input$agr_ind)
})


output$agr_map <- renderLeaflet ({
  leaflet(
    #data = cities_map,
    options = leafletOptions(
      minZoom = 3, maxZoom = 12
    ) 
  ) %>%
    leaflet.extras::addBootstrapDependency() %>%
    setView(25, 46, zoom = 6) %>%
    setMaxBounds(-12, 27.58, 56, 71.5) %>% 
    #addMapPane(name = "pol", zIndex = 410) %>%
    addMapPane(name = "maplabels", zIndex = 420) %>%
    addProviderTiles( "CartoDB.PositronNoLabels")   %>% 
    addEasyButton(
      easyButton (
        icon    = "glyphicon glyphicon-home", title = "Reset zoom",
        onClick = JS("function(btn, map){ map.setView([46, 25], 3); }")
      )
    )   %>%
    addRasterImage(
      agr_rea()[[1]], opacity = .8
      # options = leafletOptions(pane = "raster")
    ) %>%
    addLayersControl(
      baseGroups = "CartoDB.PositronNoLabels",
      overlayGroups = c("Labels", "City borders"))  %>% 
    addProviderTiles(
      "CartoDB.PositronOnlyLabels",
      options = pathOptions(pane = "maplabels"),
      group = "Labels"
    ) %>%
    addScaleBar(
      position = c("bottomleft"),
      options = scaleBarOptions(metric = TRUE)
    )
  
})
