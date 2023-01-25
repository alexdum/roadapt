# update select input year/season/month -----------------------------------

observe({
  indic <- input$silvicultura_ind
  if (indic %in% c("wsgsmax")) {
    # luni/sezona/an
    updateSelectInput(
      session, "silvicultura_perio",
      choices = select_interv,
      selected = select_interv[1]
    )
  } else { # doara anuala cand nu le ai pe celelalte
    updateSelectInput(
      session, "silvicultura_perio",
      choices = select_interv[17],
      selected = select_interv[17]
    )
  }
})


# harta leaflet -----------------------------------------------------------
silvicultura_rea <- eventReactive(list(input$go_silviculturagen, isolate(input$tab_silvicultura_gen)),{
  
  indic <- input$silvicultura_ind
  scena <-  input$silvicultura_scen
  silvicultura_tip <- input$silvicultura_tip 
  perio_tip <- strsplit(input$silvicultura_perio, "-")[[1]][2]
  perio_sub <- strsplit(input$silvicultura_perio, "-")[[1]][1]
  indic_path <- indicator_def$path[indicator_def$cod == indic] # calea catre fisier (director nc)
  
  an1_abat <- input$slider_silvicultura_abate_gen[1]
  an2_abat <- input$slider_silvicultura_abate_gen[2]
  an1_abs <- input$slider_silvicultura_absol_gen[1]
  an2_abs <- input$slider_silvicultura_absol_gen[2]
  
  # citeste fisierul
  nc_fil <- paste0("www/data/ncs/",indic_path,"/",indic,"_",scena,"_",perio_tip,"-50_19710101_21001231.nc")

  # calcal abateri sau media multianuala cu functie calcul_silvicultura_gen din utils
  ncf <- calcul_gen(nc_fil, silvicultura_tip, perio_sub, indic, an1_abat, an2_abat, an1_abs, an2_abs)
  
  # pentru legenda titlu §i intervale §i culori
  domain <- terra::minmax(ncf)
  map_leg <- map_func_cols(indic, silvicultura_tip, domain = domain, perio_tip)
  
  
  # mask raster
  ncfm <- project(ncf,  "EPSG:3857", res = 5000, method = "near")
  ncfm <- terra::mask(ncfm, mask, touches=F)
  
  
  # text harta
  name_ind <- names(select_silvicultura_ind)[which(select_silvicultura_ind %in% indic)] #nume indicator clar
  silvicultura_perio <- names(select_interv)[which(select_interv %in% input$silvicultura_perio)] # luna.sezon clar
  param_text<- ifelse (
    silvicultura_tip == "abate", 
    paste(name_ind, " - scenariul", toupper(scena),
          "schimbare", silvicultura_perio , 
          an1_abat,"-", an2_abat,  "(perioada de referință 1971-2000)"
    ),
    paste( name_ind, " - scenariul", toupper(scena),
           "- medii multianuale - ", silvicultura_perio, 
           an1_abs,"-", an2_abs
    )
  )
  
  list(
    nc = ncfm, nc_geo = ncf, # pentru popup
    domain = domain, pal =  map_leg$pal, pal_rev =  map_leg$pal_rev, tit_leg  =   map_leg$tit_leg, param_text = param_text,
    opacy = input$transp_silvicultura_gen, indic = indic, scena = scena, perio_tip = perio_tip,
    nc_fil = nc_fil, perio_sub = perio_sub, # pentru procesare cu python extragere time series plot
    silvicultura_tip = silvicultura_tip,  name_ind =  name_ind, silvicultura_perio = silvicultura_perio 
  )
  
})

# text harta
output$silvicultura_text_gen <- renderText({
  silvicultura_rea()$param_tex
  
})


output$silvicultura_map_gen <- renderLeaflet ({
  leaflet_fun_gen(
    data = borders, 
    raster = isolate(silvicultura_rea()$nc),
    cols = isolate(silvicultura_rea()$pal),
    cols_rev = isolate(silvicultura_rea()$pal_rev), 
    tit_leg = isolate(silvicultura_rea()$tit_leg),
    domain = isolate(silvicultura_rea()$domain)
  )
})

observe({ 
  leafletProxy("silvicultura_map_gen")  |>
    clearImages() %>%
    addRasterImage(
      silvicultura_rea()$nc,
      colors = silvicultura_rea()$pal, opacity = silvicultura_rea()$opacy) |>
    clearControls() |>
    addLegend(
      title = silvicultura_rea()$tit_leg,
      position = "bottomright",
      pal = silvicultura_rea()$pal_rev, values = silvicultura_rea()$domain,
      opacity = 1,
      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
    )
})


variables_plot_silvicultura_gen <- reactiveValues(
  input = NULL, title = NULL, cors = NULL, indic = NULL, tip = NULL, 
  # variabile control pentru actualizare grafic/date
  update_input = NULL, update_input_tip_plt = NULL 
)

observe({ 
  proxy <- leafletProxy("silvicultura_map_gen") 
  click <- input$silvicultura_map_gen_click
  nc_ex <- silvicultura_rea()$nc_geo
  nc_fil <- silvicultura_rea()$nc_fil
  perio_sub <- silvicultura_rea()$perio_sub
  indic <- silvicultura_rea()$indic
  name_ind <-  silvicultura_rea()$name_ind 
  silvicultura_perio <- silvicultura_rea()$silvicultura_perio
  scena <-  silvicultura_rea()$scena
  # pentru titlu grafic
  silvicultura_tip <- silvicultura_rea()$silvicultura_tip
  silvicultura_tip_name_ind <- ifelse(silvicultura_tip == "abate", paste("Schimbare în",tolower(name_ind)), name_ind) 
  # afiseaza popup sau grafic time series
  if (input$radio_silvicultura_gen == 1 & !is.null(click)) {
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
      condpan_silvicultura_gen_txt <- ifelse( 
        is.na(ex_ver), 
        "nas", 
        paste0(
          silvicultura_tip_name_ind," ", silvicultura_perio," ",toupper(scena), " (lon = ",round(click$lng, 5),", lat = "  , round(click$lat, 5),") - perioada de referință 1971 - 2000"
        )
      )
      output$condpan_silvicultura_gen <- renderText({
        condpan_silvicultura_gen_txt
      })
      outputOptions(output, "condpan_silvicultura_gen", suspendWhenHidden = FALSE)
   
      variables_plot_silvicultura_gen$input <- dd
      variables_plot_silvicultura_gen$cors <- paste0(round(click$lng, 5), "_", round(click$lat, 5))
      variables_plot_silvicultura_gen$indic <- indic
      variables_plot_silvicultura_gen$tip <- silvicultura_tip
      
    }
  }
  
  
})

# conditie pentru update plot si data table doar daca se modifica tipul plotului (abatere/absolute)
# sau datele de intrare in plot
observe({
  
  if (!isTRUE(all.equal(variables_plot_silvicultura_gen$input, variables_plot_silvicultura_gen$update_input ))  |
     !isTRUE(all.equal(variables_plot_silvicultura_gen$tip, variables_plot_silvicultura_gen$update_input_tip_plt))) {
    
    indic_plt <- variables_plot_silvicultura_gen$indic
    tip_plt <- variables_plot_silvicultura_gen$tip
    input_plt <- variables_plot_silvicultura_gen$input
    # pentru comparare in caz de update
    variables_plot_silvicultura_gen$update_input <- input_plt 
    variables_plot_silvicultura_gen$update_input_tip_plt <- tip_plt 
    
    # pentru subtab plot
    output$silvicultura_timeseries_gen_plot <- renderPlotly({
      plt <- plots_gen(input_plt, tip_plt, indic_plt)
      plt$gp
    })
    
    # pentru afisare subtab date
    output$silvicultura_timeseries_gen_data <- DT::renderDT({
      
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



