# update select input year/season/month -----------------------------------

observe({
  indic <- input$industrie_ind
  if (indic %in% c("tasmaxAdjust", "hurs")) {
    # luni/sezona/an
    updateSelectInput(
      session, "industrie_perio",
      choices = select_interv,
      selected = select_interv[1]
    )
  } else { # doara anuala cand nu le ai pe celelalte
    updateSelectInput(
      session, "industrie_perio",
      choices = select_interv[17],
      selected = select_interv[17]
    )
  }
})

# harta leaflet -----------------------------------------------------------
industrie_rea <- eventReactive(list(input$go_industriegen, isolate(input$tab_industrie_gen)),{
  
  indic <- input$industrie_ind
  scena <-  input$industrie_scen
  industrie_tip <- input$industrie_tip 
  perio_tip <- strsplit(input$industrie_perio, "-")[[1]][2]
  perio_sub <- strsplit(input$industrie_perio, "-")[[1]][1]
  indic_path <- indicator_def$path[indicator_def$cod == indic] # calea catre fisier (director nc)
  
  an1_abat <- input$slider_industrie_abate_gen[1]
  an2_abat <- input$slider_industrie_abate_gen[2]
  an1_abs <- input$slider_industrie_absol_gen[1]
  an2_abs <- input$slider_industrie_absol_gen[2]
  
  # citeste fisierul
  nc_fil <- paste0("www/data/ncs/",indic_path,"/",indic,"_",scena,"_",perio_tip,"-50_19710101_21001231.nc")

  # calcal abateri sau media multianuala cu functie calcul_industrie_gen din utils
  ncf <- calcul_gen(nc_fil, industrie_tip, perio_sub, indic, an1_abat, an2_abat, an1_abs, an2_abs)
  
  
  # mask raster
  ncfm <- project(ncf,  "EPSG:3857", res = 5000, method = "near")
  ncfm <- terra::mask(ncfm, mask, touches = F)
  
  
  # pentru legenda titlu §i intervale §i culori
  min_max <- map_func_min_max(indic, industrie_tip, perio_tip)
  if (!is.na(min_max[1])) { # verifica daca ai valori disponibile, cel putin una
    ncfm[ncfm > min_max[2]] <- min_max[2]
    ncfm[ncfm < min_max[1]] <- min_max[1]
  }
  domain <- terra::minmax(ncfm)
  map_leg <- map_func_cols(indic, industrie_tip, domain = domain, perio_tip)
  
  
  
  # text harta
  name_ind <- names(select_industrie_ind)[which(select_industrie_ind %in% indic)] #nume indicator clar
  industrie_perio <- names(select_interv)[which(select_interv %in% input$industrie_perio)] # luna.sezon clar
  param_text <- ifelse (
    industrie_tip == "abate", 
    paste(name_ind, " - scenariul", toupper(scena),
          "schimbare", industrie_perio , 
          an1_abat,"-", an2_abat,  "(perioada de referință 1971-2000)"
    ),
    paste( name_ind, " - scenariul", toupper(scena),
           "- medii multianuale - ", industrie_perio, 
           an1_abs,"-", an2_abs
    )
  )
  
  # definitie indicator
  param_def <- indicator_def$definitie[indicator_def$cod == indic] 
  
  list(
    nc = ncfm, nc_geo = ncf, # pentru popup
    domain = domain, pal =  map_leg$pal, pal_rev =  map_leg$pal_rev, tit_leg  =   map_leg$tit_leg, 
    param_text = param_text, param_def = param_def,
    opacy = input$transp_industrie_gen, indic = indic, scena = scena, perio_tip = perio_tip,
    nc_fil = nc_fil, perio_sub = perio_sub, # pentru procesare cu python extragere time series plot
    industrie_tip = industrie_tip,  name_ind =  name_ind, industrie_perio = industrie_perio 
  )
  
})

# text harta
output$industrie_text_gen <- renderText({
  paste(industrie_rea()$param_text, "<br>", industrie_rea()$param_def)

  
})


output$industrie_map_gen <- renderLeaflet ({
  leaflet_fun_gen(
    data = borders, 
    raster = isolate(industrie_rea()$nc),
    cols = isolate(industrie_rea()$pal),
    cols_rev = isolate(industrie_rea()$pal_rev), 
    tit_leg = isolate(industrie_rea()$tit_leg),
    domain = isolate(industrie_rea()$domain)
  )
})

observe({ 
  leafletProxy("industrie_map_gen")  |>
    clearImages() %>%
    addRasterImage(
      industrie_rea()$nc,
      colors = industrie_rea()$pal, opacity = industrie_rea()$opacy) |>
    clearControls() |>
    addLegend(
      title = industrie_rea()$tit_leg,
      position = "bottomright",
      pal = industrie_rea()$pal_rev, values = industrie_rea()$domain,
      opacity = 1,
      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
    )
})


variables_plot_industrie_gen <- reactiveValues(
  input = NULL, title = NULL, cors = NULL, indic = NULL, tip = NULL, 
  # variabile control pentru actualizare grafic/date
  update_input = NULL, update_input_tip_plt = NULL 
)

observe({ 
  proxy <- leafletProxy("industrie_map_gen") 
  click <- input$industrie_map_gen_click
  nc_ex <- industrie_rea()$nc_geo
  nc_fil <- industrie_rea()$nc_fil
  perio_sub <- industrie_rea()$perio_sub
  indic <- industrie_rea()$indic
  name_ind <-  industrie_rea()$name_ind 
  industrie_perio <- industrie_rea()$industrie_perio
  scena <-  industrie_rea()$scena
  # pentru titlu grafic
  industrie_tip <- industrie_rea()$industrie_tip
  industrie_tip_name_ind <- ifelse(industrie_tip == "abate", paste("Schimbare în",tolower(name_ind)), name_ind) 
  # afiseaza popup sau grafic time series
  if (input$radio_industrie_gen == 1 & !is.null(click)) {
    show_popup(x = click$lng, y = click$lat, rdat = nc_ex, proxy = proxy)
  } else {
    proxy %>% clearPopups()
    if (!is.null(click)) {
      cell <- terra::cellFromXY(nc_ex, cbind(lon = click$lng, lat = click$lat))
      xy <- terra::xyFromCell(nc_ex, cell)
      ex_ver <- terra::extract(nc_ex, xy) # pentru verificare disponilibitate valori
      
      # extrage variabila pentru python
      if(!is.na(ex_ver)) {
        dd <- extract_timeser_gen(nc_fil, xy, perio_sub) # functie extrage time series netcdf 
      } else {
        dd <- NA
      }
      # text conditional panel plot
      condpan_industrie_gen_txt <- ifelse( 
        is.na(ex_ver), 
        "nas", 
        paste0(
          industrie_tip_name_ind," ", industrie_perio," ",toupper(scena), " (lon = ",round(click$lng, 5),", lat = "  , round(click$lat, 5),") - perioada de referință 1971 - 2000"
        )
      )
      output$condpan_industrie_gen <- renderText({
        condpan_industrie_gen_txt
      })
      outputOptions(output, "condpan_industrie_gen", suspendWhenHidden = FALSE)
   
      variables_plot_industrie_gen$input <- dd
      variables_plot_industrie_gen$cors <- paste0(round(click$lng, 5), "_", round(click$lat, 5))
      variables_plot_industrie_gen$indic <- indic
      variables_plot_industrie_gen$tip <- industrie_tip
      
    }
  }
  
  
})

# conditie pentru update plot si data table doar daca se modifica tipul plotului (abatere/absolute)
# sau datele de intrare in plot
observe({
  
  if(!isTRUE(all.equal(variables_plot_industrie_gen$input, variables_plot_industrie_gen$update_input ))  |
     !isTRUE(all.equal(variables_plot_industrie_gen$tip, variables_plot_industrie_gen$update_input_tip_plt))) {
    
    indic_plt <- variables_plot_industrie_gen$indic
    tip_plt <- variables_plot_industrie_gen$tip
    input_plt <- variables_plot_industrie_gen$input
    # pentru comparare in caz de update
    variables_plot_industrie_gen$update_input <- input_plt 
    variables_plot_industrie_gen$update_input_tip_plt <- tip_plt 
    
    # pentru subtab plot
    output$industrie_timeseries_gen_plot <- renderPlotly({
      plt <- plots_gen(input_plt, tip_plt, indic_plt)
      plt$gp
    })
    
    # pentru afisare subtab date
    output$industrie_timeseries_gen_data <- DT::renderDT({
      
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



