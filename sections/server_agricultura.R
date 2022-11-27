
# harta leaflet -----------------------------------------------------------
agr_rea <- eventReactive(input$go_agrgen,{
  
  indic <- input$agr_ind
  scena <-  input$agr_scen
  perio_tip <- strsplit(input$agr_perio, "-")[[1]][2]
  perio_sub <- strsplit(input$agr_perio, "-")[[1]][1]
  
  # selectare slider in functie de tipul hartii
  if (input$agr_tip == "abate") {
    an1 <- input$slider_agro_abate[1]
    an2 <- input$slider_agro_abate[2]
    
    nc <- rast(paste0("www/data/ncs/",indic,"Adjust_",scena,"_",perio_tip,"-50_19710101_21001231.nc"))
    dats <- names_to_date(nc) 
    
    dats.norm <- dats[dats >= as.Date("1971-01-01") & dats <= as.Date("2000-12-31")]
    dats.norm <- dats.norm[format(dats.norm, "%m") %in% perio_sub]
    nc.norm <- nc[[which(dats %in% dats.norm)]] |> mean()
  
    dats.sub <- dats[dats >= as.Date(paste0(an1, perio_sub, "01"), format = "%Y%m%d") & dats <= (as.Date(paste0(an2 + 1, perio_sub, "01"), format = "%Y%m%d"))]
    dats.sub <- dats.sub[format(dats.sub, "%m") %in% perio_sub]
    nc.abs <- nc[[which(dats %in% dats.sub)]] |> mean()
    
    ncf <- nc.abs -  nc.norm 
    
    
  } else {
    an1 <- input$slider_agro_absol[1]
    an2 <- input$slider_agro_absol[2]
    
    nc <- rast(paste0("www/data/ncs/",indic,"Adjust_",scena,"_",perio_tip,"-50_19710101_21001231.nc"))
    dats <- names_to_date(nc) # extrage data din nume cu fct utils
    
    dats.sub <- dats[dats >= as.Date(paste0(an1, perio_sub, "01"), format = "%Y%m%d") & dats <= (as.Date(paste0(an2 + 1, perio_sub, "01"), format = "%Y%m%d"))]
    dats.sub <- dats.sub[format(dats.sub, "%m") %in% perio_sub]
    nc <- nc[[which(dats %in% dats.sub)]] 
    ncf <- mean(nc)
  }
  print(summary(ncf))
  list(
    nc = ncf, 
    indic = indic,  scena =  scena,  perio_tip =  perio_tip,  perio_sub =  perio_sub, an1 = an1, an2 = an2,
    min_dats_sub = as.character(min(dats.sub)), max_dats_sub = as.character(max(dats.sub))
    
  )
  
})


output$test <- renderText({
  paste("You chose", agr_rea()$indic,  agr_rea()$scena,  agr_rea()$perio_tip, agr_rea()$perio_sub,
        agr_rea()$an1, agr_rea()$an2, agr_rea()$min_dats_sub, agr_rea()$max_dats_sub)
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
      agr_rea()$nc[[1]], opacity = .8
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
