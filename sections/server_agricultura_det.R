
agr_rdet <- eventReactive(list(input$go_agrdet, isolate(input$tab_agro_det)),{
  
  ind <- input$agr_ind_det
  scena <- input$agr_scen_det
  agr_tip_det <- input$agr_tip_det
  perio_tip <- strsplit(input$agr_perio_det, "-")[[1]][2]
  perio_sub <- strsplit(input$agr_perio_det, "-")[[1]][1]
  tab <-  open_dataset(paste0("www/data/parquet/agro/", ind ,"Adjust_",scena,"_", perio_tip ,"-50_19710101_21001231.parquet"))
  
  #subseteaza dupa tip/an
  if (agr_tip_det == "abate") {
    an1 <- input$slider_agro_abate[1]
    an2 <- input$slider_agro_abate[2]
    
    if (perio_sub != "year") { #daca ai an formateaza data diferit
      tab.sub <- tab |> 
        #filter( month ==  as.integer(perio_sub)) |>
        to_duckdb() |>
        mutate(norm = mean(p50[year >= 1971 & year <= 2000])) |> 
        filter(year >= an1, year <= an2) |>
        group_by(ID) |> summarise(p50 = mean(p50) , norm = mean(norm)) |>
        mutate(value = p50 - norm) |>
        collect()
    } else {
      tab.sub <- tab |> 
        filter(year >= an1, year <= an1 ) |>
        group_by(ID) |> summarise(value = mean(p50)) |>
        collect() 
    }
  } else {
    an1 <- input$slider_agro_absol[1]
    an2 <- input$slider_agro_absol[2]
    if (perio_sub != "year") { #daca ai an formateaza data diferit
      tab.sub <- tab |> 
        filter( month ==  as.integer(perio_sub), year >= an1, year <= an2) |>
        group_by(ID) |> summarise(value = mean(p50)) |>
        collect()
    } else {
      tab.sub <- tab |> 
        filter(year >= an1, year <= an2) |>
        group_by(ID) |> summarise(value = mean(p50)) |>
        collect() 
    }
  }
  
  # unire cu spatial
  uat.sub <- uat |> left_join(tab.sub, by = c( "natCode" = "ID"))
  
  
  
  if (ind == "pr") {
    if (perio_tip == "year") {
      bins <- seq(floor(min(uat.sub$value)), ceiling(max(uat.sub$value)), by = 100)
    } else if (perio_sub == "season") {
      bins <- seq(floor(min(uat.sub$value)), ceiling(max(uat.sub$value)), by = 50)
    } else {
      bins <- seq(floor(min(uat.sub$value)), ceiling(max(uat.sub$value)), by = 10)
    }
    pal <- colorBin("GnBu", domain = floor(min(uat.sub$value)), ceiling(max(uat.sub$value)), bins = bins)
    pal_rev <- colorBin("GnBu", domain = floor(min(uat.sub$value)), ceiling(max(uat.sub$value)), bins = bins, reverse = T)
    tit_leg <- "mm"
    
  } else {
    bins <- seq(floor(min(uat.sub$value)), ceiling(max(uat.sub$value)), by = 2)
    print(bins)
    pal <- colorBin("RdYlBu", domain = floor(min(uat.sub$value)), ceiling(max(uat.sub$value)), bins = bins, reverse = T)
    pal_rev <- colorBin("RdYlBu", domain = floor(min(uat.sub$value)), ceiling(max(uat.sub$value)), bins = bins,reverse = F)
    tit_leg <- "°C"
  }
  
  list(
    uat.sub = uat.sub, pal = pal, pal_rev = pal_rev, tit_leg = tit_leg
  )
  
})

output$agr_map_det <- renderLeaflet ({
  
  leaflet_fun_det(
    data = isolate(agr_rdet()$uat.sub),
    pal =  isolate(agr_rdet()$pal),
    pal_rev =  isolate(agr_rdet()$pal_rev),
    tit_leg = isolate(agr_rdet()$tit_leg)
  )
})


observe({
  pal_rev =  agr_rdet()$pal_rev
  tit_leg = agr_rdet()$tit_leg
  req(input$go_agrdet) # Only display if tab is 'Detaliil'
  data <- agr_rdet()$uat.sub
  pal <- agr_rdet()$pal
  
  leafletProxy("agr_map_det",  data = data)  %>%
    clearShapes() %>%
    addPolygons (
      fillColor = ~pal(value),
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
    clearControls() |>
    addLegend(
      title = paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), tit_leg,"</html>"),
      "bottomright", pal = pal_rev, values = ~values, opacity = 1,
      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
    ) 
  
}) 
