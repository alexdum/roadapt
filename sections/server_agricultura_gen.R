
# harta leaflet -----------------------------------------------------------
agr_rea <- eventReactive(list(input$go_agrgen, isolate(input$tab_agro_gen)),{
  
  indic <- input$agr_ind
  scena <-  input$agr_scen
  agr_tip <- input$agr_tip 
  perio_tip <- strsplit(input$agr_perio, "-")[[1]][2]
  perio_sub <- strsplit(input$agr_perio, "-")[[1]][1]
  
  an1_abat <- input$slider_agro_abate_gen[1]
  an2_abat <- input$slider_agro_abate_gen[2]
  an1_abs <- input$slider_agro_absol_gen[1]
  an2_abs <- input$slider_agro_absol_gen[2]
  
  # citeste fisierul
  nc_fil <- paste0("www/data/ncs/agro/",indic,"Adjust_",scena,"_",perio_tip,"-50_19710101_21001231.nc")
  
  
  # calcal abateri sau media multianuala cu functie calcul_agro_gen din utils
  ncf <- calcul_agro_gen(nc_fil, agr_tip, perio_sub, indic, an1_abat, an2_abat, an1_abs, an2_abs)
  
  # pentru legenda titlu §i intervale §i culori
  domain <- terra::minmax(ncf)
  map_leg <- map_func_cols(indic, agr_tip, domain = domain, perio_tip)
  
  
  # mask raster
  ncfm <- project(ncf,  "EPSG:3857", res = 5000, method = "near")
  ncfm <- terra::mask(ncfm, mask, touches=F)
  
  
  # text harta
  name_ind <- names(select_agro_ind)[which(select_agro_ind %in% indic)] #nume indicator clar
  agro_perio <- names(select_interv)[which(select_interv %in% input$agr_perio)] # luna.sezon clar
  param_text<- ifelse (
    agr_tip == "abate", 
    paste(name_ind, " - scenariul", toupper(scena),
          "schimbare", agro_perio , 
          an1_abat,"-", an2_abat,  "(perioada de referință 1971-2000)"
    ),
    paste( name_ind, " - scenariul", toupper(scena),
           "- medii multianuale - ", agro_perio, 
           an1_abs,"-", an2_abs
    )
  )
  
  list(
    nc = ncfm, nc_geo = ncf, # pentru popup
    domain = domain, pal =  map_leg$pal, pal_rev =  map_leg$pal_rev, tit_leg  =   map_leg$tit_leg, param_text = param_text,
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
      title =  paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))),agr_rea()$tit_leg,"</html>"),
      position = "bottomright",
      pal = agr_rea()$pal_rev, values = agr_rea()$domain,
      opacity = 1,
      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
    )
})


variables_plot_agro_gen <- reactiveValues(
  input = NULL, title = NULL, cors = NULL, indic = NULL, tip = NULL, 
  # variabile control pentru actualizare grafic/date
  update_input = NULL, update_input_tip_plt = NULL 
)

observe({ 
  proxy <- leafletProxy("agr_map_gen") 
  click <- input$agr_map_gen_click
  nc_ex <- agr_rea()$nc_geo
  nc_fil <- agr_rea()$nc_fil
  perio_sub <- agr_rea()$perio_sub
  indic <- agr_rea()$indic
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
      condpan_agro_gen_txt <- ifelse( 
        is.na(ex_ver), 
        "nas", 
        paste0(
          agr_tip_name_ind," ", agro_perio," ",toupper(scena), " (lon = ",round(click$lng, 5),", lat = "  , round(click$lat, 5),") - perioada de referință 1971 - 2000"
        )
      )
      output$condpan_agro_gen <- renderText({
        condpan_agro_gen_txt
      })
      outputOptions(output, "condpan_agro_gen", suspendWhenHidden = FALSE)
   
      variables_plot_agro_gen$input <- dd
      variables_plot_agro_gen$cors <- paste0(round(click$lng, 5), "_", round(click$lat, 5))
      variables_plot_agro_gen$indic <- indic
      variables_plot_agro_gen$tip <- agr_tip
      
    }
  }
  
  
})

# conditie pentru update plot si data table doar daca se modifica tipul plotului (abatere/absolute)
# sau datele de intrare in plot
observe({
  
  if(!isTRUE(all.equal(variables_plot_agro_gen$input, variables_plot_agro_gen$update_input ))  |
     !isTRUE(all.equal(variables_plot_agro_gen$tip, variables_plot_agro_gen$update_input_tip_plt))) {
    
    indic_plt <- variables_plot_agro_gen$indic
    tip_plt <- variables_plot_agro_gen$tip
    input_plt <- variables_plot_agro_gen$input
    # pentru comparare in caz de update
    variables_plot_agro_gen$update_input <- input_plt 
    variables_plot_agro_gen$update_input_tip_plt <- tip_plt 
    
    # pentru subtab plot
    output$agro_timeseries_gen_plot <- renderPlotly({
      req(!is.na(input_plt))
      plt <- plots_agro_gen(input_plt, tip_plt, indic_plt)
      plt$gp
    })
    
    # pentru afisare subtab date
    output$agro_timeseries_gen_data <- DT::renderDT({
      
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



