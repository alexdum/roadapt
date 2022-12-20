# update select input year/season/month -----------------------------------

observe({
  indic <- input$energie_ind
  if (indic %in% c("rsds")) {
    # luni/sezona/an
    updateSelectInput(
      session, "energie_perio",
      choices = select_interv,
      selected = select_interv[1]
    )
  } else { # doara anuala cand nu le ai pe celelalte
    updateSelectInput(
      session, "energie_perio",
      choices = select_interv[17],
      selected = select_interv[17]
    )
  }
})

# harta leaflet -----------------------------------------------------------
energie_rea <- eventReactive(list(input$go_energiegen, isolate(input$tab_energie_gen)),{
  
  indic <- input$energie_ind
  scena <-  input$energie_scen
  energie_tip <- input$energie_tip 
  perio_tip <- strsplit(input$energie_perio, "-")[[1]][2]
  perio_sub <- strsplit(input$energie_perio, "-")[[1]][1]
  indic_path <- indicator_def$path[indicator_def$cod == indic] # calea catre fisier (director nc)
  
  an1_abat <- input$slider_energie_abate_gen[1]
  an2_abat <- input$slider_energie_abate_gen[2]
  an1_abs <- input$slider_energie_absol_gen[1]
  an2_abs <- input$slider_energie_absol_gen[2]
  
  # citeste fisierul
  nc_fil <- paste0("www/data/ncs/",indic_path,"/",indic,"_",scena,"_",perio_tip,"-50_19710101_21001231.nc")

  # calcal abateri sau media multianuala cu functie calcul_energie_gen din utils
  ncf <- calcul_gen(nc_fil, energie_tip, perio_sub, indic, an1_abat, an2_abat, an1_abs, an2_abs)
  
  # pentru legenda titlu §i intervale §i culori
  domain <- terra::minmax(ncf)
  map_leg <- map_func_cols(indic, energie_tip, domain = domain, perio_tip)
  
  
  # mask raster
  ncfm <- project(ncf,  "EPSG:3857", res = 5000, method = "near")
  ncfm <- terra::mask(ncfm, mask, touches=F)
  
  
  # text harta
  name_ind <- names(select_energie_ind)[which(select_energie_ind %in% indic)] #nume indicator clar
  energie_perio <- names(select_interv)[which(select_interv %in% input$energie_perio)] # luna.sezon clar
  param_text<- ifelse (
    energie_tip == "abate", 
    paste(name_ind, " - scenariul", toupper(scena),
          "schimbare", energie_perio , 
          an1_abat,"-", an2_abat,  "(perioada de referință 1971-2000)"
    ),
    paste( name_ind, " - scenariul", toupper(scena),
           "- medii multianuale - ", energie_perio, 
           an1_abs,"-", an2_abs
    )
  )
  
  list(
    nc = ncfm, nc_geo = ncf, # pentru popup
    domain = domain, pal =  map_leg$pal, pal_rev =  map_leg$pal_rev, tit_leg  =   map_leg$tit_leg, param_text = param_text,
    opacy = input$transp_energie_gen, indic = indic, scena = scena, perio_tip = perio_tip,
    nc_fil = nc_fil, perio_sub = perio_sub, # pentru procesare cu python extragere time series plot
    energie_tip = energie_tip,  name_ind =  name_ind, energie_perio = energie_perio 
  )
  
})

# text harta
output$energie_text_gen <- renderText({
  energie_rea()$param_tex
  
})


output$energie_map_gen <- renderLeaflet ({
  leaflet_fun_gen(
    data = borders, 
    raster = isolate(energie_rea()$nc),
    cols = isolate(energie_rea()$pal),
    cols_rev = isolate(energie_rea()$pal_rev), 
    tit_leg = isolate(energie_rea()$tit_leg),
    domain = isolate(energie_rea()$domain)
  )
})

observe({ 
  leafletProxy("energie_map_gen")  |>
    clearImages() %>%
    addRasterImage(
      energie_rea()$nc,
      colors = energie_rea()$pal, opacity = energie_rea()$opacy) |>
    clearControls() |>
    addLegend(
      title = energie_rea()$tit_leg,
      position = "bottomright",
      pal = energie_rea()$pal_rev, values = energie_rea()$domain,
      opacity = 1,
      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
    )
})


variables_plot_energie_gen <- reactiveValues(
  input = NULL, title = NULL, cors = NULL, indic = NULL, tip = NULL, 
  # variabile control pentru actualizare grafic/date
  update_input = NULL, update_input_tip_plt = NULL 
)

observe({ 
  proxy <- leafletProxy("energie_map_gen") 
  click <- input$energie_map_gen_click
  nc_ex <- energie_rea()$nc_geo
  nc_fil <- energie_rea()$nc_fil
  perio_sub <- energie_rea()$perio_sub
  indic <- energie_rea()$indic
  name_ind <-  energie_rea()$name_ind 
  energie_perio <- energie_rea()$energie_perio
  scena <-  energie_rea()$scena
  # pentru titlu grafic
  energie_tip <- energie_rea()$energie_tip
  energie_tip_name_ind <- ifelse(energie_tip == "abate", paste("Schimbare în",tolower(name_ind)), name_ind) 
  # afiseaza popup sau grafic time series
  if (input$radio_energie_gen == 1 & !is.null(click)) {
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
      condpan_energie_gen_txt <- ifelse( 
        is.na(ex_ver), 
        "nas", 
        paste0(
          energie_tip_name_ind," ", energie_perio," ",toupper(scena), " (lon = ",round(click$lng, 5),", lat = "  , round(click$lat, 5),") - perioada de referință 1971 - 2000"
        )
      )
      output$condpan_energie_gen <- renderText({
        condpan_energie_gen_txt
      })
      outputOptions(output, "condpan_energie_gen", suspendWhenHidden = FALSE)
   
      variables_plot_energie_gen$input <- dd
      variables_plot_energie_gen$cors <- paste0(round(click$lng, 5), "_", round(click$lat, 5))
      variables_plot_energie_gen$indic <- indic
      variables_plot_energie_gen$tip <- energie_tip
      
    }
  }
  
  
})

# conditie pentru update plot si data table doar daca se modifica tipul plotului (abatere/absolute)
# sau datele de intrare in plot
observe({
  
  if(!isTRUE(all.equal(variables_plot_energie_gen$input, variables_plot_energie_gen$update_input ))  |
     !isTRUE(all.equal(variables_plot_energie_gen$tip, variables_plot_energie_gen$update_input_tip_plt))) {
    
    indic_plt <- variables_plot_energie_gen$indic
    tip_plt <- variables_plot_energie_gen$tip
    input_plt <- variables_plot_energie_gen$input
    # pentru comparare in caz de update
    variables_plot_energie_gen$update_input <- input_plt 
    variables_plot_energie_gen$update_input_tip_plt <- tip_plt 
    
    # pentru subtab plot
    output$energie_timeseries_gen_plot <- renderPlotly({
      req(!is.na(input_plt))
      plt <- plots_gen(input_plt, tip_plt, indic_plt)
      plt$gp
    })
    
    # pentru afisare subtab date
    output$energie_timeseries_gen_data <- DT::renderDT({
      
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



