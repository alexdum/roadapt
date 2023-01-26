# update select input year/season/month -----------------------------------

observe({
  indic <- input$hidro_ind
  if (indic %in% c("prAdjust")) {
    # luni/sezona/an
    updateSelectInput(
      session, "hidro_perio",
      choices = select_interv,
      selected = select_interv[1]
    )
  } else { # doara anuala cand nu le ai pe celelalte
    updateSelectInput(
      session, "hidro_perio",
      choices = select_interv,
      selected = select_interv[1]
    )
  }
})

# harta leaflet -----------------------------------------------------------
hidro_rea <- eventReactive(list(input$go_hidrogen, isolate(input$tab_hidro_gen)),{
  
  indic <- input$hidro_ind
  scena <-  input$hidro_scen
  hidro_tip <- input$hidro_tip 
  perio_tip <- strsplit(input$hidro_perio, "-")[[1]][2]
  perio_sub <- strsplit(input$hidro_perio, "-")[[1]][1]
  indic_path <- indicator_def$path[indicator_def$cod == indic] # calea catre fisier (director nc)
  
  an1_abat <- input$slider_hidro_abate_gen[1]
  an2_abat <- input$slider_hidro_abate_gen[2]
  an1_abs <- input$slider_hidro_absol_gen[1]
  an2_abs <- input$slider_hidro_absol_gen[2]
  
  # citeste fisierul
  nc_fil <- paste0("www/data/ncs/",indic_path,"/",indic,"_",scena,"_",perio_tip,"-50_19710101_21001231.nc")

  # calcal abateri sau media multianuala cu functie calcul_hidro_gen din utils
  ncf <- calcul_gen(nc_fil, hidro_tip, perio_sub, indic, an1_abat, an2_abat, an1_abs, an2_abs)
  
  # pentru legenda titlu §i intervale §i culori
  domain <- terra::minmax(ncf)
  map_leg <- map_func_cols(indic, hidro_tip, domain = domain, perio_tip)
  
  
  # mask raster
  ncfm <- project(ncf,  "EPSG:3857", res = 5000, method = "near")
  ncfm <- terra::mask(ncfm, mask, touches=F)
  
  
  # text harta
  name_ind <- names(select_hidro_ind)[which(select_hidro_ind %in% indic)] #nume indicator clar
  hidro_perio <- names(select_interv)[which(select_interv %in% input$hidro_perio)] # luna.sezon clar
  param_text<- ifelse (
    hidro_tip == "abate", 
    paste(name_ind, " - scenariul", toupper(scena),
          "schimbare", hidro_perio , 
          an1_abat,"-", an2_abat,  "(perioada de referință 1971-2000)"
    ),
    paste( name_ind, " - scenariul", toupper(scena),
           "- medii multianuale - ", hidro_perio, 
           an1_abs,"-", an2_abs
    )
  )
  
  list(
    nc = ncfm, nc_geo = ncf, # pentru popup
    domain = domain, pal =  map_leg$pal, pal_rev =  map_leg$pal_rev, tit_leg  =   map_leg$tit_leg, param_text = param_text,
    opacy = input$transp_hidro_gen, indic = indic, scena = scena, perio_tip = perio_tip,
    nc_fil = nc_fil, perio_sub = perio_sub, # pentru procesare cu python extragere time series plot
    hidro_tip = hidro_tip,  name_ind =  name_ind, hidro_perio = hidro_perio 
  )
  
})

# text harta
output$hidro_text_gen <- renderText({
  hidro_rea()$param_tex
  
})


output$hidro_map_gen <- renderLeaflet ({
  leaflet_fun_gen(
    data = borders, 
    raster = isolate(hidro_rea()$nc),
    cols = isolate(hidro_rea()$pal),
    cols_rev = isolate(hidro_rea()$pal_rev), 
    tit_leg = isolate(hidro_rea()$tit_leg),
    domain = isolate(hidro_rea()$domain)
  )
})

observe({ 
  leafletProxy("hidro_map_gen")  |>
    clearImages() %>%
    addRasterImage(
      hidro_rea()$nc,
      colors = hidro_rea()$pal, opacity = hidro_rea()$opacy) |>
    clearControls() |>
    addLegend(
      title = hidro_rea()$tit_leg,
      position = "bottomright",
      pal = hidro_rea()$pal_rev, values = hidro_rea()$domain,
      opacity = 1,
      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
    )
})


variables_plot_hidro_gen <- reactiveValues(
  input = NULL, title = NULL, cors = NULL, indic = NULL, tip = NULL, 
  # variabile control pentru actualizare grafic/date
  update_input = NULL, update_input_tip_plt = NULL 
)

observe({ 
  proxy <- leafletProxy("hidro_map_gen") 
  click <- input$hidro_map_gen_click
  nc_ex <- hidro_rea()$nc_geo
  nc_fil <- hidro_rea()$nc_fil
  perio_sub <- hidro_rea()$perio_sub
  indic <- hidro_rea()$indic
  name_ind <-  hidro_rea()$name_ind 
  hidro_perio <- hidro_rea()$hidro_perio
  scena <-  hidro_rea()$scena
  # pentru titlu grafic
  hidro_tip <- hidro_rea()$hidro_tip
  hidro_tip_name_ind <- ifelse(hidro_tip == "abate", paste("Schimbare în",tolower(name_ind)), name_ind) 
  # afiseaza popup sau grafic time series
  if (input$radio_hidro_gen == 1 & !is.null(click)) {
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
      condpan_hidro_gen_txt <- ifelse( 
        is.na(ex_ver), 
        "nas", 
        paste0(
          hidro_tip_name_ind," ", hidro_perio," ",toupper(scena), " (lon = ",round(click$lng, 5),", lat = "  , round(click$lat, 5),") - perioada de referință 1971 - 2000"
        )
      )
      output$condpan_hidro_gen <- renderText({
        condpan_hidro_gen_txt
      })
      outputOptions(output, "condpan_hidro_gen", suspendWhenHidden = FALSE)
   
      variables_plot_hidro_gen$input <- dd
      variables_plot_hidro_gen$cors <- paste0(round(click$lng, 5), "_", round(click$lat, 5))
      variables_plot_hidro_gen$indic <- indic
      variables_plot_hidro_gen$tip <- hidro_tip
      
    }
  }
  
  
})

# conditie pentru update plot si data table doar daca se modifica tipul plotului (abatere/absolute)
# sau datele de intrare in plot
observe({
  
  if(!isTRUE(all.equal(variables_plot_hidro_gen$input, variables_plot_hidro_gen$update_input ))  |
     !isTRUE(all.equal(variables_plot_hidro_gen$tip, variables_plot_hidro_gen$update_input_tip_plt))) {
    
    indic_plt <- variables_plot_hidro_gen$indic
    tip_plt <- variables_plot_hidro_gen$tip
    input_plt <- variables_plot_hidro_gen$input
    # pentru comparare in caz de update
    variables_plot_hidro_gen$update_input <- input_plt 
    variables_plot_hidro_gen$update_input_tip_plt <- tip_plt 
    
    # pentru subtab plot
    output$hidro_timeseries_gen_plot <- renderPlotly({
      plt <- plots_gen(input_plt, tip_plt, indic_plt)
      plt$gp
    })
    
    # pentru afisare subtab date
    output$hidro_timeseries_gen_data <- DT::renderDT({
      
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



