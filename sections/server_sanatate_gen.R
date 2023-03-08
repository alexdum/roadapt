# update select input year/season/month -----------------------------------

observe({
  indic <- input$sanatate_ind
  if (indic %in% c("hurs", "tasmaxAdjust", "tasminAdjust")) {
    # luni/sezona/an
    updateSelectInput(
      session, "sanatate_perio",
      choices = select_interv,
      selected = select_interv[1]
    )
  } else { # doara anuala cand nu le ai pe celelalte
    updateSelectInput(
      session, "sanatate_perio",
      choices = select_interv[17],
      selected = select_interv[17]
    )
  }
})

# harta leaflet -----------------------------------------------------------
sanatate_rea <- eventReactive(list(input$go_sanatategen, isolate(input$tab_sanatate_gen)),{
  
  indic <- input$sanatate_ind
  scena <-  input$sanatate_scen
  sanatate_tip <- input$sanatate_tip 
  perio_tip <- strsplit(input$sanatate_perio, "-")[[1]][2]
  perio_sub <- strsplit(input$sanatate_perio, "-")[[1]][1]
  indic_path <- indicator_def$path[indicator_def$cod == indic] # calea catre fisier (director nc)
  
  an1_abat <- input$slider_sanatate_abate_gen[1]
  an2_abat <- input$slider_sanatate_abate_gen[2]
  an1_abs <- input$slider_sanatate_absol_gen[1]
  an2_abs <- input$slider_sanatate_absol_gen[2]
  
  # citeste fisierul
  nc_fil <- paste0("www/data/ncs/",indic_path,"/",indic,"_",scena,"_",perio_tip,"-50_19710101_21001231.nc")
  
  
  # calcal abateri sau media multianuala cu functie calcul_sanatate_gen din utils
  ncf <- calcul_gen(nc_fil, sanatate_tip, perio_sub, indic, an1_abat, an2_abat, an1_abs, an2_abs)
  
  # mask raster
  ncfm <- project(ncf,  "EPSG:3857", res = 5000, method = "near")
  ncfm <- terra::mask(ncfm, mask, touches = F)
  
  
  # pentru legenda titlu si intervale si culori
  min_max <- map_func_min_max(indic, sanatate_tip, perio_tip)
  if (!is.na(min_max[1])) { # verifica daca ai valori disponibile, cel putin una
    ncfm[ncfm > min_max[2]] <- min_max[2]
    ncfm[ncfm < min_max[1]] <- min_max[1]
  }
  # pentru legenda titlu §i intervale §i culori
  domain <- terra::minmax(ncfm)
  map_leg <- map_func_cols(indic, sanatate_tip, domain = domain, perio_tip)
  
  
  
  # text harta
  name_ind <- names(select_sanatate_ind)[which(select_sanatate_ind %in% indic)] #nume indicator clar
  sanatate_perio <- names(select_interv)[which(select_interv %in% input$sanatate_perio)] # luna.sezon clar
  param_text <- ifelse(
    sanatate_tip == "abate", 
    paste(name_ind, " - scenariul", toupper(scena),
          "schimbare", sanatate_perio , 
          an1_abat,"-", an2_abat,  "(perioada de referință 1971-2000)"
    ),
    paste( name_ind, " - scenariul", toupper(scena),
           "- medii multianuale - ", sanatate_perio, 
           an1_abs,"-", an2_abs
    )
  )
  
  list(
    nc = ncfm, nc_geo = ncf, # pentru popup
    domain = domain, pal =  map_leg$pal, pal_rev =  map_leg$pal_rev, tit_leg  =   map_leg$tit_leg, param_text = param_text,
    opacy = input$transp_sanatate_gen, indic = indic, scena = scena, perio_tip = perio_tip,
    nc_fil = nc_fil, perio_sub = perio_sub, # pentru procesare cu python extragere time series plot
    sanatate_tip = sanatate_tip,  name_ind =  name_ind, sanatate_perio = sanatate_perio 
  )
  
})

# text harta
output$sanatate_text_gen <- renderText({
  sanatate_rea()$param_tex
  
})


output$sanatate_map_gen <- renderLeaflet ({
  leaflet_fun_gen(
    data = borders, 
    raster = isolate(sanatate_rea()$nc),
    cols = isolate(sanatate_rea()$pal),
    cols_rev = isolate(sanatate_rea()$pal_rev), 
    tit_leg = isolate(sanatate_rea()$tit_leg),
    domain = isolate(sanatate_rea()$domain)
  )
})

observe({ 
  leafletProxy("sanatate_map_gen")  |>
    clearImages() %>%
    addRasterImage(
      sanatate_rea()$nc,
      colors = sanatate_rea()$pal, opacity = sanatate_rea()$opacy) |>
    clearControls() |>
    addLegend(
      title = sanatate_rea()$tit_leg,
      position = "bottomright",
      pal = sanatate_rea()$pal_rev, values = sanatate_rea()$domain,
      opacity = 1,
      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
    )
})


variables_plot_sanatate_gen <- reactiveValues(
  input = NULL, title = NULL, cors = NULL, indic = NULL, tip = NULL, 
  # variabile control pentru actualizare grafic/date
  update_input = NULL, update_input_tip_plt = NULL 
)

observe({ 
  proxy <- leafletProxy("sanatate_map_gen") 
  click <- input$sanatate_map_gen_click
  nc_ex <- sanatate_rea()$nc_geo
  nc_fil <- sanatate_rea()$nc_fil
  perio_sub <- sanatate_rea()$perio_sub
  indic <- sanatate_rea()$indic
  name_ind <-  sanatate_rea()$name_ind 
  sanatate_perio <- sanatate_rea()$sanatate_perio
  scena <-  sanatate_rea()$scena
  # pentru titlu grafic
  sanatate_tip <- sanatate_rea()$sanatate_tip
  sanatate_tip_name_ind <- ifelse(sanatate_tip == "abate", paste("Schimbare în",tolower(name_ind)), name_ind) 
  # afiseaza popup sau grafic time series
  if (input$radio_sanatate_gen == 1 & !is.null(click)) {
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
      condpan_sanatate_gen_txt <- ifelse( 
        is.na(ex_ver), 
        "nas", 
        paste0(
          sanatate_tip_name_ind," ", sanatate_perio," ",toupper(scena), " (lon = ",round(click$lng, 5),", lat = "  , round(click$lat, 5),") - perioada de referință 1971 - 2000"
        )
      )
      output$condpan_sanatate_gen <- renderText({
        condpan_sanatate_gen_txt
      })
      outputOptions(output, "condpan_sanatate_gen", suspendWhenHidden = FALSE)
   
      variables_plot_sanatate_gen$input <- dd
      variables_plot_sanatate_gen$cors <- paste0(round(click$lng, 5), "_", round(click$lat, 5))
      variables_plot_sanatate_gen$indic <- indic
      variables_plot_sanatate_gen$tip <- sanatate_tip
      
    }
  }
  
  
})

# conditie pentru update plot si data table doar daca se modifica tipul plotului (abatere/absolute)
# sau datele de intrare in plot
observe({
  
  if(!isTRUE(all.equal(variables_plot_sanatate_gen$input, variables_plot_sanatate_gen$update_input ))  |
     !isTRUE(all.equal(variables_plot_sanatate_gen$tip, variables_plot_sanatate_gen$update_input_tip_plt))) {
    
    indic_plt <- variables_plot_sanatate_gen$indic
    tip_plt <- variables_plot_sanatate_gen$tip
    input_plt <- variables_plot_sanatate_gen$input
    # pentru comparare in caz de update
    variables_plot_sanatate_gen$update_input <- input_plt 
    variables_plot_sanatate_gen$update_input_tip_plt <- tip_plt 
    
    # pentru subtab plot
    output$sanatate_timeseries_gen_plot <- renderPlotly({
      plt <- plots_gen(input_plt, tip_plt, indic_plt)
      plt$gp
    })
    
    # pentru afisare subtab date
    output$sanatate_timeseries_gen_data <- DT::renderDT({
      
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



