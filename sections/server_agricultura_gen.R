
# harta leaflet -----------------------------------------------------------
agr_rea <- eventReactive(list(input$go_agrgen, isolate(input$tab_agro_gen)),{
  
  indic <- input$agr_ind
  scena <-  input$agr_scen
  agr_tip <- input$agr_tip 
  perio_tip <- strsplit(input$agr_perio, "-")[[1]][2]
  perio_sub <- strsplit(input$agr_perio, "-")[[1]][1]
  
  # citeste fisierul
  nc <- rast(paste0("www/data/ncs/agro/",indic,"Adjust_",scena,"_",perio_tip,"-50_19710101_21001231.nc"))
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
    
    ncf <- nc.abs - nc.norm 
    
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
  
  # mask raster
  ncfm <- project(ncf,  "EPSG:3857")
  ncfm <- terra::mask(ncfm, mask)
  
  
  # text harta
  param_text<- ifelse (
    agr_tip == "abate", 
    paste(names(select_agro_ind)[which(select_agro_ind %in% indic)], " - scenariul", toupper(scena),
          "schimbare", names(select_interv)[which(select_interv %in% input$agr_perio)], 
          an1,"-", an2,  "(perioada de referință 1971-2000)"
    ),
    paste(names(select_agro_ind)[which(select_agro_ind %in% indic)], " - scenariul", toupper(scena),
          "- medii multianuale - ", names(select_interv)[which(select_interv %in% input$agr_perio)], 
          an1,"-", an2
    )
  )
  
  
  list(
    nc = ncfm, nc_geo = ncf, # pentru popup
    domain = domain, pal = pal, pal_rev = pal_rev, tit_leg  =  tit_leg, param_text = param_text,
    opacy = input$transp_agr_gen
  )
  
})

# text harta
output$agr_text_gen <- renderText({
  agr_rea()$param_tex
  
})


output$agr_map_gen <- renderLeaflet ({
  leaflet_fun_gen(
    data = borders, 
    raster = isolate(agr_rea()$nc),
    cols = isolate(agr_rea()$pal),
    cols_rev = isolate(agr_rea()$pal_rev), 
    tit_leg = isolate(agr_rea()$tit_leg),
    domain = isolate(agr_rea()$domain)
  )
})

observe({ 
  leafletProxy("agr_map_gen")  |>
    clearImages() %>%
    addRasterImage(
      agr_rea()$nc,
      colors = agr_rea()$pal, opacity = agr_rea()$opacy) |>
    clearControls() |>
    addLegend(
      title =  agr_rea()$tit_leg,
      position = "bottomright",
      pal = agr_rea()$pal_rev, values = agr_rea()$domain,
      opacity = 1,
      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
    )
})

observe({ 
  proxy <- leafletProxy("agr_map_gen")
  click <- input$agr_map_gen_click
  nc_ex <- agr_rea()$nc_geo
  # afiseaza popup sau grafic time series
  if (input$radio_agr_gen == 1 & !is.null(click)) {
    show_popup(x = click$lng, y = click$lat, rdat = nc_ex, proxy = proxy)
  } else {
    proxy %>% clearPopups()
  }
})
