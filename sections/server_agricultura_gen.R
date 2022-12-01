
# harta leaflet -----------------------------------------------------------
agr_rea <- eventReactive(list(input$go_agrgen, isolate(input$tab_agro_gen)),{
  
  indic <- input$agr_ind
  scena <-  input$agr_scen
  agr_tip <- input$agr_tip 
  perio_tip <- strsplit(input$agr_perio, "-")[[1]][2]
  perio_sub <- strsplit(input$agr_perio, "-")[[1]][1]
  
  # citeste fisierul
  nc_fil <- paste0("www/data/ncs/agro/",indic,"Adjust_",scena,"_",perio_tip,"-50_19710101_21001231.nc")
  nc <- rast(nc_fil)
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
  name_ind <- names(select_agro_ind)[which(select_agro_ind %in% indic)] #nume indicator clar
  agro_perio <- names(select_interv)[which(select_interv %in% input$agr_perio)] # luna.sezon clar
  param_text<- ifelse (
    agr_tip == "abate", 
    paste(name_ind, " - scenariul", toupper(scena),
          "schimbare", agro_perio , 
          an1,"-", an2,  "(perioada de referință 1971-2000)"
    ),
    paste( name_ind, " - scenariul", toupper(scena),
           "- medii multianuale - ", agro_perio , 
           an1,"-", an2
    )
  )
  
  list(
    nc = ncfm, nc_geo = ncf, # pentru popup
    domain = domain, pal = pal, pal_rev = pal_rev, tit_leg  =  tit_leg, param_text = param_text,
    opacy = input$transp_agr_gen, indic = indic, scena = scena, perio_tip = perio_tip,
    nc_fil = nc_fil, perio_sub = perio_sub, # pentru procesare cu python extragere time series plot
    agr_tip = agr_tip,  name_ind =  name_ind, agro_perio = agro_perio 
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


variables_plot_agro_gen <- reactiveValues(input = NULL, title = NULL, cors = NULL)

observe({ 
  proxy <- leafletProxy("agr_map_gen")
  click <- input$agr_map_gen_click
  nc_ex <- agr_rea()$nc_geo
  nc_fil <- agr_rea()$nc_fil
  perio_sub <- agr_rea()$perio_sub
  name_ind <-  agr_rea()$name_ind 
  agro_perio <- agr_rea()$agro_perio
  scena <-  agr_rea()$scena
  # pentru titlu grafic
  agr_tip <- agr_rea()$agr_tip
  agr_tip_name_ind <- ifelse(agr_tip == "abate", paste("Schimbare în",tolower(name_ind)), name_ind) 
  # afiseaza popup sau grafic time series
  if (input$radio_agr_gen == 1 & !is.null(click)) {
    show_popup(x = click$lng, y = click$lat, rdat = nc_ex, proxy = proxy)
  } else {
    proxy %>% clearPopups()
    if (!is.null(click)) {
      cell <- terra::cellFromXY(nc_ex, cbind(click$lng, click$lat))
      xy <- terra::xyFromCell(nc_ex, cell)
      # extrage variabila pentru python
      dd <- extract_timeser_gen(nc_fil, xy, perio_sub) # functie extrage time series netcdf 
      # text conditional panel plot
      print(is.na(cell))
      print(is.na(mean(dd$med, na.rm = T)))
      condpan_agro_gen_txt <- ifelse( 
        is.na(mean(dd$med, na.rm = T)) | is.na(cell), 
        "nas", 
        paste0(
          agr_tip_name_ind," ", agro_perio," ",toupper(scena), " (lon = ",round(click$lng, 5),", lat = "  , round(click$lat, 5),") - perioada de referință 1971 - 2000"
        )
      )
      output$condpan_agro_gen <- renderText({
        condpan_agro_gen_txt
      })
      outputOptions(output, "condpan_agro_gen", suspendWhenHidden = FALSE)
      # # valori pentru plot la reactive values
      #values_plot_lst_mon$title <- condpan_monthly.txt
      variables_plot_agro_gen$input <- dd
      variables_plot_agro_gen$cors <- paste0(round(click$lng, 5), "_", round(click$lat, 5))
    }
  }
  
})

# plot actualizat daca schimb si coordonatee
output$agro_timeseries_gen_plot <- renderPlotly({
  indic <-agr_rea()$indic
  req(!is.na(variables_plot_agro_gen$input))
  print(agr_rea()$agr_tip)
  plt <- plots_agro_gen(variables_plot_agro_gen$input, agr_rea()$agr_tip, indic)
  plt$gp
})


output$agro_timeseries_gen_data <- DT::renderDT({
  
  DT::datatable(
    variables_plot_agro_gen$input, extensions = 'Buttons', rownames = F,
    options = list(
      dom = 'Bfrtip',
      pageLength = 5, autoWidth = TRUE,
      buttons = c('pageLength','copy', 'csv', 'excel'),
      pagelength = 10, lengthMenu = list(c(10, 25, 100, -1), c('10', '25', '100','All')
      )
      
    )
  )
  
})

