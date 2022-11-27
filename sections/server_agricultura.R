
# harta leaflet -----------------------------------------------------------
agr_rea <- eventReactive(list(input$go_agrgen, isolate(input$tab_agro_gen)),{
  
  indic <- input$agr_ind
  scena <-  input$agr_scen
  agr_tip <- input$agr_tip 
  perio_tip <- strsplit(input$agr_perio, "-")[[1]][2]
  perio_sub <- strsplit(input$agr_perio, "-")[[1]][1]
  
  # citeste fisierul
  nc <- rast(paste0("www/data/ncs/",indic,"Adjust_",scena,"_",perio_tip,"-50_19710101_21001231.nc"))
  dats <- names_to_date(nc) # extrage data din nume cu fct utils
  
  
  # selectare slider in functie de tipul hartii
  if (agr_tip == "abate") {
    an1 <- input$slider_agro_abate[1]
    an2 <- input$slider_agro_abate[2]
    
    
    dats.sub <- dats[dats >= as.Date(paste0(an1, "0101"), format = "%Y%m%d") & dats <= as.Date(paste0(an2 , "1231"), format = "%Y%m%d") ]
    dats.norm <- dats[dats >= as.Date("1971-01-01") & dats <= as.Date("2000-12-31")]
    
    if (perio_sub != "year") { #daca ai an formateaza data diferit
      dats.sub <- dats.sub[format(dats.sub, "%m") %in% perio_sub] # daca ai an nu subseta pe perioade
      dats.norm <- dats.norm[format(dats.norm, "%m") %in% perio_sub] # daca ai an nu subseta pe perioade
    }
    
    nc.norm <- nc[[which(dats %in% dats.norm)]] |> mean()
    nc.abs <- nc[[which(dats %in% dats.sub)]] |> mean()
    
    ncf <- nc.abs -  nc.norm 
    
    
  } else {
    an1 <- input$slider_agro_absol[1]
    an2 <- input$slider_agro_absol[2]
    dats.sub <- dats[dats >= as.Date(paste0(an1, "0101"), format = "%Y%m%d") & dats <= as.Date(paste0(an2 , "1231"), format = "%Y%m%d") ]
    if (perio_sub != "year") { #daca ai an formateaza data diferit
      dats.sub <- dats.sub[format(dats.sub, "%m") %in% perio_sub] # daca ai an nu subseta pe perioade
    }
    
    ncf <- nc[[which(dats %in% dats.sub)]] 
    ncf <- mean(ncf)
  }
  
  # pentru legenda 
  domain <- minmax(ncf)
  
  if (indic == "pr") {
    pal_rev <- colorNumeric("GnBu", domain = domain, reverse = T, na.color = "transparent")
    pal<- colorNumeric("GnBu", domain = domain, reverse = F, na.color = "transparent")
    tit_leg <- "mm"
  } else {
    pal_rev <- colorNumeric("RdYlBu", domain = domain, reverse = F, na.color = "transparent")
    pal<- colorNumeric("RdYlBu", domain = domain, reverse = T, na.color = "transparent")
    tit_leg <- "°C"
  }
  
  
  list(
    nc = ncf, 
    indic = indic,  scena =  scena,  perio_tip =  perio_tip,  perio_sub =  perio_sub, an1 = an1, an2 = an2,
    min_dats_sub = as.character(min(dats.sub)), max_dats_sub = as.character(max(dats.sub)),
    domain = domain, pal = pal, pal_rev = pal_rev, tit_leg  =  tit_leg
    
  )
  
})


output$test <- renderText({
  paste("You chose", agr_rea()$indic,  agr_rea()$scena,  agr_rea()$perio_tip, agr_rea()$perio_sub,
        agr_rea()$an1, agr_rea()$an2, agr_rea()$min_dats_sub, agr_rea()$max_dats_sub)
})


output$agr_map <- renderLeaflet ({
  
  leaflet(
    data = borders,
    options = leafletOptions(
      minZoom = 3, maxZoom = 12
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
      agr_rea()$nc,
      colors = agr_rea()$pal, opacity = .8
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
      options = scaleBarOptions(metric = TRUE)
    )  |> 
    clearControls() %>%
    addLegend(
      title =  agr_rea()$tit_leg,
      position = "bottomright",
      pal = agr_rea()$pal_rev, values = agr_rea()$domain,
      opacity = 1,
      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
    )
  
})
