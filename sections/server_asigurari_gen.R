# update select input year/season/month -----------------------------------

observe({
  indic <- input$asigurari_ind
  if (indic %in% c("rsds")) {
    # luni/sezona/an
    updateSelectInput(
      session, "asigurari_perio",
      choices = select_interv,
      selected = select_interv[1]
    )
  } else { # doara anuala cand nu le ai pe celelalte
    updateSelectInput(
      session, "asigurari_perio",
      choices = select_interv[17],
      selected = select_interv[17]
    )
  }
})

# harta leaflet -----------------------------------------------------------
asigurari_rea <- eventReactive(list(input$go_asigurarigen, isolate(input$tab_asigurari_gen)),{
  
  indic <- input$asigurari_ind
  scena <-  input$asigurari_scen
  asigurari_tip <- input$asigurari_tip 
  perio_tip <- strsplit(input$asigurari_perio, "-")[[1]][2]
  perio_sub <- strsplit(input$asigurari_perio, "-")[[1]][1]
  indic_path <- indicator_def$path[indicator_def$cod == indic] # calea catre fisier (director nc)
  
  an1_abat <- input$slider_asigurari_abate_gen[1]
  an2_abat <- input$slider_asigurari_abate_gen[2]
  an1_abs <- input$slider_asigurari_absol_gen[1]
  an2_abs <- input$slider_asigurari_absol_gen[2]
  
  # citeste fisierul
  nc_fil <- paste0("www/data/ncs/",indic_path,"/",indic,"_",scena,"_",perio_tip,"-50_19710101_21001231.nc")

  # calcal abateri sau media multianuala cu functie calcul_asigurari_gen din utils
  ncf <- calcul_gen(nc_fil, asigurari_tip, perio_sub, indic, an1_abat, an2_abat, an1_abs, an2_abs)
  
  # pentru legenda titlu §i intervale §i culori
  domain <- terra::minmax(ncf)
  map_leg <- map_func_cols(indic, asigurari_tip, domain = domain, perio_tip)
  
  
  # mask raster
  ncfm <- project(ncf,  "EPSG:3857", res = 5000, method = "near")
  ncfm <- terra::mask(ncfm, mask, touches = F)
  
  
  # text harta
  name_ind <- names(select_asigurari_ind)[which(select_asigurari_ind %in% indic)] #nume indicator clar
  asigurari_perio <- names(select_interv)[which(select_interv %in% input$asigurari_perio)] # luna.sezon clar
  param_text <- ifelse(
    asigurari_tip == "abate", 
    paste(name_ind, " - scenariul", toupper(scena),
          "schimbare", asigurari_perio , 
          an1_abat,"-", an2_abat,  "(perioada de referință 1971-2000)"
    ),
    paste( name_ind, " - scenariul", toupper(scena),
           "- medii multianuale - ", asigurari_perio, 
           an1_abs,"-", an2_abs
    )
  )
  
  list(
    nc = ncfm, nc_geo = ncf, # pentru popup
    domain = domain, pal =  map_leg$pal, pal_rev =  map_leg$pal_rev, tit_leg  =   map_leg$tit_leg, param_text = param_text,
    opacy = input$transp_asigurari_gen, indic = indic, scena = scena, perio_tip = perio_tip,
    nc_fil = nc_fil, perio_sub = perio_sub, # pentru procesare cu python extragere time series plot
    asigurari_tip = asigurari_tip,  name_ind =  name_ind, asigurari_perio = asigurari_perio 
  )
  
})

# text harta
output$asigurari_text_gen <- renderText({
  asigurari_rea()$param_tex
  
})


output$asigurari_map_gen <- renderLeaflet ({
  leaflet_fun_gen(
    data = borders, 
    raster = isolate(asigurari_rea()$nc),
    cols = isolate(asigurari_rea()$pal),
    cols_rev = isolate(asigurari_rea()$pal_rev), 
    tit_leg = isolate(asigurari_rea()$tit_leg),
    domain = isolate(asigurari_rea()$domain)
  )
})

observe({ 
  leafletProxy("asigurari_map_gen")  |>
    clearImages() %>%
    addRasterImage(
      asigurari_rea()$nc,
      colors = asigurari_rea()$pal, opacity = asigurari_rea()$opacy) |>
    clearControls() |>
    addLegend(
      title = asigurari_rea()$tit_leg,
      position = "bottomright",
      pal = asigurari_rea()$pal_rev, values = asigurari_rea()$domain,
      opacity = 1,
      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
    )
})


variables_plot_asigurari_gen <- reactiveValues(
  input = NULL, title = NULL, cors = NULL, indic = NULL, tip = NULL, 
  # variabile control pentru actualizare grafic/date
  update_input = NULL, update_input_tip_plt = NULL 
)

observe({ 
  proxy <- leafletProxy("asigurari_map_gen") 
  click <- input$asigurari_map_gen_click
  nc_ex <- asigurari_rea()$nc_geo
  nc_fil <- asigurari_rea()$nc_fil
  perio_sub <- asigurari_rea()$perio_sub
  indic <- asigurari_rea()$indic
  name_ind <-  asigurari_rea()$name_ind 
  asigurari_perio <- asigurari_rea()$asigurari_perio
  scena <-  asigurari_rea()$scena
  # pentru titlu grafic
  asigurari_tip <- asigurari_rea()$asigurari_tip
  asigurari_tip_name_ind <- ifelse(asigurari_tip == "abate", paste("Schimbare în",tolower(name_ind)), name_ind) 
  # afiseaza popup sau grafic time series
  if (input$radio_asigurari_gen == 1 & !is.null(click)) {
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
      condpan_asigurari_gen_txt <- ifelse( 
        is.na(ex_ver), 
        "nas", 
        paste0(
          asigurari_tip_name_ind," ", asigurari_perio," ",toupper(scena), " (lon = ",round(click$lng, 5),", lat = "  , round(click$lat, 5),") - perioada de referință 1971 - 2000"
        )
      )
      output$condpan_asigurari_gen <- renderText({
        condpan_asigurari_gen_txt
      })
      outputOptions(output, "condpan_asigurari_gen", suspendWhenHidden = FALSE)
   
      variables_plot_asigurari_gen$input <- dd
      variables_plot_asigurari_gen$cors <- paste0(round(click$lng, 5), "_", round(click$lat, 5))
      variables_plot_asigurari_gen$indic <- indic
      variables_plot_asigurari_gen$tip <- asigurari_tip
      
    }
  }
  
  
})

# conditie pentru update plot si data table doar daca se modifica tipul plotului (abatere/absolute)
# sau datele de intrare in plot
observe({
  
  if(!isTRUE(all.equal(variables_plot_asigurari_gen$input, variables_plot_asigurari_gen$update_input ))  |
     !isTRUE(all.equal(variables_plot_asigurari_gen$tip, variables_plot_asigurari_gen$update_input_tip_plt))) {
    
    indic_plt <- variables_plot_asigurari_gen$indic
    tip_plt <- variables_plot_asigurari_gen$tip
    input_plt <- variables_plot_asigurari_gen$input
    # pentru comparare in caz de update
    variables_plot_asigurari_gen$update_input <- input_plt 
    variables_plot_asigurari_gen$update_input_tip_plt <- tip_plt 
    
    # pentru subtab plot
    output$asigurari_timeseries_gen_plot <- renderPlotly({
      plt <- plots_gen(input_plt, tip_plt, indic_plt)
      plt$gp
    })
    
    # pentru afisare subtab date
    output$asigurari_timeseries_gen_data <- DT::renderDT({
      
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



