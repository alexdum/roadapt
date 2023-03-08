# update select input year/season/month -----------------------------------

observe({
  indic <- input$transport_ind
  if (indic %in% c("wsgsmax", "tasminAdjust", "tasmaxAdjust")) {
    # luni/sezona/an
    updateSelectInput(
      session, "transport_perio",
      choices = select_interv,
      selected = select_interv[1]
    )
  } else { # doara anuala cand nu le ai pe celelalte
    updateSelectInput(
      session, "transport_perio",
      choices = select_interv[17],
      selected = select_interv[17]
    )
  }
})


# harta leaflet -----------------------------------------------------------
transport_rea <- eventReactive(list(input$go_transportgen, isolate(input$tab_transport_gen)),{
  
  indic <- input$transport_ind
  scena <-  input$transport_scen
  transport_tip <- input$transport_tip 
  perio_tip <- strsplit(input$transport_perio, "-")[[1]][2]
  perio_sub <- strsplit(input$transport_perio, "-")[[1]][1]
  indic_path <- indicator_def$path[indicator_def$cod == indic] # calea catre fisier (director nc)
  
  an1_abat <- input$slider_transport_abate_gen[1]
  an2_abat <- input$slider_transport_abate_gen[2]
  an1_abs <- input$slider_transport_absol_gen[1]
  an2_abs <- input$slider_transport_absol_gen[2]
  
  # citeste fisierul
  nc_fil <- paste0("www/data/ncs/",indic_path,"/",indic,"_",scena,"_",perio_tip,"-50_19710101_21001231.nc")

  # calcal abateri sau media multianuala cu functie calcul_transport_gen din utils
  ncf <- calcul_gen(nc_fil, transport_tip, perio_sub, indic, an1_abat, an2_abat, an1_abs, an2_abs)
  
  # mask raster
  ncfm <- project(ncf,  "EPSG:3857", res = 5000, method = "near")
  ncfm <- terra::mask(ncfm, mask, touches = F)
  
  # pentru legenda titlu §i intervale §i culori
  min_max <- map_func_min_max(indic, transport_tip, perio_tip)
  if (!is.na(min_max[1])) { # verifica daca ai valori disponibile, cel putin una
    ncfm[ncfm > min_max[2]] <- min_max[2]
    ncfm[ncfm < min_max[1]] <- min_max[1]
  }
  # pentru legenda titlu §i intervale §i culori
  domain <- terra::minmax(ncfm)
  map_leg <- map_func_cols(indic, transport_tip, domain = domain, perio_tip)
  
  

  # text harta
  name_ind <- names(select_transport_ind)[which(select_transport_ind %in% indic)] #nume indicator clar
  transport_perio <- names(select_interv)[which(select_interv %in% input$transport_perio)] # luna.sezon clar
  param_text<- ifelse (
    transport_tip == "abate", 
    paste(name_ind, " - scenariul", toupper(scena),
          "schimbare", transport_perio , 
          an1_abat,"-", an2_abat,  "(perioada de referință 1971-2000)"
    ),
    paste( name_ind, " - scenariul", toupper(scena),
           "- medii multianuale - ", transport_perio, 
           an1_abs,"-", an2_abs
    )
  )
  
  list(
    nc = ncfm, nc_geo = ncf, # pentru popup
    domain = domain, pal =  map_leg$pal, pal_rev =  map_leg$pal_rev, tit_leg  =   map_leg$tit_leg, param_text = param_text,
    opacy = input$transp_transport_gen, indic = indic, scena = scena, perio_tip = perio_tip,
    nc_fil = nc_fil, perio_sub = perio_sub, # pentru procesare cu python extragere time series plot
    transport_tip = transport_tip,  name_ind =  name_ind, transport_perio = transport_perio 
  )
  
})

# text harta
output$transport_text_gen <- renderText({
  transport_rea()$param_tex
  
})


output$transport_map_gen <- renderLeaflet ({
  leaflet_fun_gen(
    data = borders, 
    raster = isolate(transport_rea()$nc),
    cols = isolate(transport_rea()$pal),
    cols_rev = isolate(transport_rea()$pal_rev), 
    tit_leg = isolate(transport_rea()$tit_leg),
    domain = isolate(transport_rea()$domain)
  )
})

observe({ 
  leafletProxy("transport_map_gen")  |>
    clearImages() %>%
    addRasterImage(
      transport_rea()$nc,
      colors = transport_rea()$pal, opacity = transport_rea()$opacy) |>
    clearControls() |>
    addLegend(
      title = transport_rea()$tit_leg,
      position = "bottomright",
      pal = transport_rea()$pal_rev, values = transport_rea()$domain,
      opacity = 1,
      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
    )
})


variables_plot_transport_gen <- reactiveValues(
  input = NULL, title = NULL, cors = NULL, indic = NULL, tip = NULL, 
  # variabile control pentru actualizare grafic/date
  update_input = NULL, update_input_tip_plt = NULL 
)

observe({ 
  proxy <- leafletProxy("transport_map_gen") 
  click <- input$transport_map_gen_click
  nc_ex <- transport_rea()$nc_geo
  nc_fil <- transport_rea()$nc_fil
  perio_sub <- transport_rea()$perio_sub
  indic <- transport_rea()$indic
  name_ind <-  transport_rea()$name_ind 
  transport_perio <- transport_rea()$transport_perio
  scena <-  transport_rea()$scena
  # pentru titlu grafic
  transport_tip <- transport_rea()$transport_tip
  transport_tip_name_ind <- ifelse(transport_tip == "abate", paste("Schimbare în",tolower(name_ind)), name_ind) 
  # afiseaza popup sau grafic time series
  if (input$radio_transport_gen == 1 & !is.null(click)) {
    show_popup(x = click$lng, y = click$lat, rdat = nc_ex, proxy = proxy)
  } else {
    proxy %>% clearPopups()
    if (!is.null(click)) {
      cell <- terra::cellFromXY(nc_ex, cbind(lon = click$lng, lat = click$lat))
      xy <- terra::xyFromCell(nc_ex, cell)
      ex_ver <- terra::extract(nc_ex, xy) # pentru verificare disponilibitate valori
      
      # extrage variabila pentru python
      if (!is.na(ex_ver)) {
        dd <- extract_timeser_gen(nc_fil, xy, perio_sub) # functie extrage time series netcdf 
      } else {
        dd <- NA
      }
      # text conditional panel plot
      condpan_transport_gen_txt <- ifelse( 
        is.na(ex_ver), 
        "nas", 
        paste0(
          transport_tip_name_ind," ", transport_perio," ",toupper(scena), " (lon = ",round(click$lng, 5),", lat = "  , round(click$lat, 5),") - perioada de referință 1971 - 2000"
        )
      )
      output$condpan_transport_gen <- renderText({
        condpan_transport_gen_txt
      })
      outputOptions(output, "condpan_transport_gen", suspendWhenHidden = FALSE)
   
      variables_plot_transport_gen$input <- dd
      variables_plot_transport_gen$cors <- paste0(round(click$lng, 5), "_", round(click$lat, 5))
      variables_plot_transport_gen$indic <- indic
      variables_plot_transport_gen$tip <- transport_tip
      
    }
  }
  
  
})

# conditie pentru update plot si data table doar daca se modifica tipul plotului (abatere/absolute)
# sau datele de intrare in plot
observe({
  
  if (!isTRUE(all.equal(variables_plot_transport_gen$input, variables_plot_transport_gen$update_input ))  |
     !isTRUE(all.equal(variables_plot_transport_gen$tip, variables_plot_transport_gen$update_input_tip_plt))) {
    
    indic_plt <- variables_plot_transport_gen$indic
    tip_plt <- variables_plot_transport_gen$tip
    input_plt <- variables_plot_transport_gen$input
    # pentru comparare in caz de update
    variables_plot_transport_gen$update_input <- input_plt 
    variables_plot_transport_gen$update_input_tip_plt <- tip_plt 
    
    # pentru subtab plot
    output$transport_timeseries_gen_plot <- renderPlotly({
      plt <- plots_gen(input_plt, tip_plt, indic_plt)
      plt$gp
    })
    
    # pentru afisare subtab date
    output$transport_timeseries_gen_data <- DT::renderDT({
      
      DT::datatable(
        input_plt, extensions = 'Buttons', rownames = F,
        options = list(
          dom = 'Bfrtip',
          pageLength = 5, autoWidth = TRUE,
          buttons = c('pageLength','copy', 'csv', 'excel'),
          pagelength = 10, lengthMenu = list(c(10, 25, 100, -1), c('10', '25', '100','All')
          )
          
        )
      )
      
    })
  }
})



