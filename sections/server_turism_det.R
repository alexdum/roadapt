# update select input year/season/month -----------------------------------
observe({
  indic <- input$turism_ind_det
  if (indic %in% c("prAdjust")) {
    # luni/sezona/an
    updateSelectInput(
      session, "turism_perio_det",
      choices = select_interv,
      selected = select_interv[1]
    )
  } else { # doara anuala cand nu le ai pe celelalte
    updateSelectInput(
      session, "turism_perio_det",
      choices = select_interv[17],
      selected = select_interv[17]
    )
  }
})

turism_rdet <- eventReactive(list(input$go_turismdet, isolate(input$tab_turism_det)),{
  
  
  admin <- input$turism_admin_det
  indic <- input$turism_ind_det
  scena <- input$turism_scen_det
  turism_tip <- input$turism_tip_det # absolut/abate
  perio_sub <- strsplit(input$turism_perio_det, "-")[[1]][1] # number of month
  perio_tip <- strsplit(input$turism_perio_det, "-")[[1]][2] # month/season/year
  indic_path <- indicator_def$path[indicator_def$cod == indic] # calea catre fisier (director parquet)
  
  # selectie unitate
  switch( # alege nume indicator care să fie afișat
    which(c("uat", "jud", "reg", "ltser", "sci_spa") %in%  admin),
    admin_spat <- uat,
    admin_spat <- jud,
    admin_spat <- reg,
    admin_spat <- ltser,
    admin_spat <- sci_spa
  )
  
  
  tab <-  read_parquet(paste0("www/data/parquet/",indic_path,"/",admin,"/", indic ,"_",scena,"_", perio_tip ,"-50_19710101_21001231.parquet"))
  
  an1_abat <- input$slider_turism_abate_det[1]
  an2_abat <- input$slider_turism_abate_det[2]
  an1_abs <- input$slider_turism_absol_det[1]
  an2_abs <- input$slider_turism_absol_det[2]
  
  # calcul abateri absolute cu funct utils/calcul_turism_det.R
  tab_sub <- calcul_det(tab, turism_tip, perio_sub, indic, an1_abat, an2_abat, an1_abs, an2_abs)
  # valori minime maxime pentru plotare reclasificare valori/plotare sugestiva
  min_max <- map_func_min_max(indic = indic, ind_tip =  turism_tip, perio_tip = perio_tip)
  tab_sub$value[tab_sub$value > min_max[2]] <- min_max[2]
  tab_sub$value[tab_sub$value < min_max[1]] <- min_max[1]
  # unire cu spatial
  admin_spat_sub <- admin_spat |> left_join(tab_sub, by = c( "natcode" = "ID"))
  
  # legenda si intervale functie utils/cols_leg_turism_det.R
  map_leg <- map_func_cols(indic, turism_tip, domain = range(admin_spat_sub$value), perio_tip)
  
  
  # text harta
  # text harta
  name_ind <- names(select_turism_ind)[which(select_turism_ind %in% indic)] #nume indicator clar
  turism_perio <- names(select_interv)[which(select_interv %in% input$turism_perio_det)] # luna.sezon clar
  param_text <- ifelse(
    turism_tip == "abate", 
    paste(name_ind , " - scenariul", toupper(scena), "schimbare", turism_perio, an1_abat,"-", an2_abat,  "(perioada de referință 1971-2000)"),
    paste(name_ind , " - scenariul", toupper(scena), "- medii multianuale - ", turism_perio, an1_abs,"-", an2_abs)
  )
  
  list(
    admin_spat_sub = admin_spat_sub, pal = map_leg$pal, pal_rev = map_leg$pal_rev, tit_leg = map_leg$tit_leg,
    param_text = param_text, opacy = input$transp_turism_det, tab = tab, perio_sub = perio_sub, indic = indic,
    turism_tip = turism_tip, scena = scena, name_ind = name_ind, turism_perio = turism_perio, 
    an1_abat = an1_abat, an2_abat = an2_abat, an1_abs = an1_abs, an2_abs = an2_abs,
    admin = admin
  )
  
})

output$turism_text_det <- renderText({
  turism_rdet()$param_tex
  
})

output$turism_map_det <- renderLeaflet({
  leaflet_fun_det(
    data = isolate(turism_rdet()$admin_spat_sub),
    pal =  isolate(turism_rdet()$pal),
    pal_rev =  isolate(turism_rdet()$pal_rev),
    tit_leg = isolate(turism_rdet()$tit_leg)
  )
})


observe({
  #req(input$go_turismdet == "Detalii") # Only display if tab is 'Detalii'
  pal_rev =  turism_rdet()$pal_rev
  tit_leg = turism_rdet()$tit_leg
  data <- turism_rdet()$admin_spat_sub
  pal <- turism_rdet()$pal
  opacy <- turism_rdet()$opacy 
  
  leafletProxy("turism_map_det",  data = data)  %>%
    clearShapes() %>%
    addPolygons(
      fillColor = ~pal(value),
      label = ~paste("<font size='2'><b>Name unit:",name,
                     "<br/>",round(value,1),"</b></font><br/>
                       <font size='1' color='#E95420'>Click to
                       get values and graph</font>") %>% lapply(htmltools::HTML),
      labelOptions = labelOptions(textsize = "13px"),
      color = "grey",
      weight = 0.5, smoothFactor = 0.1,
      opacity = 0.5,
      fillOpacity = opacy ,
      layerId = ~natcode,
      # options = pathOptions(pane = "pol"),
      #group = "region",
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        fillOpacity = 0.2,
        bringToFront = TRUE,
        sendToBack = TRUE)) |>
    clearControls() |>
    addLegend(
      title = tit_leg,
      "bottomright", pal = pal_rev, values = ~value, opacity = 1,
      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
    ) 
  
}) 




# reactive values plot ----------------------------------------------------
variables_plot_turism_det <- reactiveValues(
  input = NULL, title = NULL, cors = NULL, indic = NULL, tip = NULL, 
  # variabile control pentru actualizare grafic/date
  norm = NULL, mean = NULL, change = NULL, id = NULL, name = NULL, county = NULL,
  admin = NULL, update_admin = NULL
  
) 

observeEvent(list(isolate(input$go_turismdet)),{ #, isolate(input$tab_turism_det)),{
  variables_plot_turism_det$admin  <- turism_rdet()$admin
  admin_spat_sub <- turism_rdet()$admin_spat_sub
  first_sel <- sample(1:nrow(admin_spat_sub), 1)
  variables_plot_turism_det$id <- admin_spat_sub$natcode[first_sel]
  variables_plot_turism_det$name <- admin_spat_sub$name[admin_spat_sub$natcode == variables_plot_turism_det$id]
  variables_plot_turism_det$county <- admin_spat_sub$county[admin_spat_sub$natcode == variables_plot_turism_det$id]
  
})

# pentru actualizare grafic doar cand se schimba regiunea
observe({
  
  variables_plot_turism_det$admin <- turism_rdet()$admin
  
  if (!isTRUE(all.equal(variables_plot_turism_det$admin, variables_plot_turism_det$update_admin)))  {
    admin <- variables_plot_turism_det$admin
    admin_spat_sub <- turism_rdet()$admin_spat_sub
    first_sel <- sample(1:nrow(admin_spat_sub), 1)
    variables_plot_turism_det$id <- admin_spat_sub$natcode[first_sel]
    variables_plot_turism_det$name <- admin_spat_sub$name[admin_spat_sub$natcode == variables_plot_turism_det$id]
    variables_plot_turism_det$county <- admin_spat_sub$county[admin_spat_sub$natcode == variables_plot_turism_det$id]
    variables_plot_turism_det$update_admin <- admin
  }
})



observeEvent(input$turism_map_det_shape_click$id,{ 
  admin_spat_sub <- turism_rdet()$admin_spat_sub
  variables_plot_turism_det$id  <- input$turism_map_det_shape_click$id
  variables_plot_turism_det$name <- admin_spat_sub$name[admin_spat_sub$natcode == input$turism_map_det_shape_click$id]
  variables_plot_turism_det$county <- admin_spat_sub$county[admin_spat_sub$natcode == input$turism_map_det_shape_click$id]
})


observeEvent(list(input$go_turismdet, variables_plot_turism_det$id), {
  
  turism_tip <- turism_rdet()$turism_tip
  tab <- turism_rdet()$tab
  perio_sub <- turism_rdet()$perio_sub
  indic <- turism_rdet()$indic
  name_ind <- turism_rdet()$name_ind
  
  dd <- extract_timeser_det(tab, variables_plot_turism_det$id, perio_sub, indic)
  
  variables_plot_turism_det$input <- dd
  variables_plot_turism_det$indic <-  indic 
  variables_plot_turism_det$tip <- turism_tip 
  
  
}) 

# nume grafic
output$condpan_turism_det <- renderText({
  admin <- turism_rdet()$admin
  turism_perio <-  turism_rdet()$turism_perio 
  name_ind <- turism_rdet()$name_ind
  turism_tip <- turism_rdet()$turism_tip
  scena <- turism_rdet()$scena
  # ajustare nume in functie de unitat administrativa
  turism_tip_name_ind <- ifelse(turism_tip == "abate", paste("Schimbare în",tolower(name_ind)), name_ind) 
  if (admin == "reg") name_aadmin <- paste("regiunea", variables_plot_turism_det$name)
  if (admin == "jud") name_aadmin <- paste("județul", variables_plot_turism_det$name)
  if (admin == "ltser") name_aadmin <- paste("LTSER", variables_plot_turism_det$name)
  if (admin == "sci_spa") name_aadmin <- paste("SCI-SPA", variables_plot_turism_det$name)
  if (admin == "uat") name_aadmin <- paste(variables_plot_turism_det$name," - județul ",variables_plot_turism_det$county)
  paste0(
    turism_tip_name_ind," ", turism_perio," ",toupper(scena), 
    " (",name_aadmin ,") 
    - perioada de referință 1971 - 2000"
  )
})



output$turism_det_stat <- renderUI({
  
  an1_abat <- turism_rdet()$an1_abat
  an2_abat <- turism_rdet()$an2_abat
  an1_abs <- turism_rdet()$an1_abs
  an2_abs <- turism_rdet()$an2_abs
  admin_spat_sub <- turism_rdet()$admin_spat_sub
  turism_tip <- turism_rdet()$turism_tip
  
  if (turism_tip == 'abate') {
    norm <- admin_spat_sub$norm[admin_spat_sub$natcode == variables_plot_turism_det$id] |> round(1)
    multimean <- admin_spat_sub$p50[admin_spat_sub$natcode == variables_plot_turism_det$id] |> round(1)
    change <- admin_spat_sub$value[admin_spat_sub$natcode == variables_plot_turism_det$id] |> round(1)
    
    HTML(
      paste0(
        "<table>
      <tr>
      <td style='padding:5px 10px 5px 5px'>Media 1971- 2000</d>
      <td style='padding:5px 10px 5px 5px'>Media ",an1_abat,"-",an2_abat,"</td>
      <td style='padding:5px 10px 5px 5px'>Schimbare ",an1_abat,"-",an2_abat," 
      vs. 1971- 2010</td>
      </tr>
      <tr>
      <td style='padding:5px 10px 5px 5px'>",norm,"</td>
      <td style='padding:5px 10px 5px 5px'>",multimean,"</td>
      <td style='padding:5px 10px 5px 5px'>",change,"</td>
     </tr>
      </table>",
     "<font size='2' color='#E95420'>Click pe hartă (regiunea de interes) pentru actualizare grafic și valori </font>"
      )
    )
  } else {
    value <-admin_spat_sub$value[admin_spat_sub$natcode == variables_plot_turism_det$id] |> round(1)
    
    HTML(
      paste0(
        "<table>
      <tr>
      <td style='padding:5px 10px 5px 5px'>Media ",an1_abs,"-",an2_abs,"</td>
      </tr>
      <tr>
      <td style='padding:5px 10px 5px 5px'>",value,"</td>
     </tr>
      </table>",
     "<font size='2' color='#E95420'>Click pe hartă (regiunea de interes) pentru actualizare grafic și valori </font>"
      )
    )
  }
  
})



# pentru subtab plot
output$turism_timeseries_det_plot <- renderPlotly({
  #req(!is.na(variables_plot_turism_det$input))
  plt <- plots_det(
    variables_plot_turism_det$input,
    variables_plot_turism_det$tip,  
    variables_plot_turism_det$indic
  )
  plt$gp
})

# pentru afisare subtab date
output$turism_timeseries_det_data <- DT::renderDT({
  
  DT::datatable(
    variables_plot_turism_det$input |> 
      dplyr::mutate(across(is.numeric, round, digits = 1)) |>
      dplyr::select(-ID, -p10, -p90),
    extensions = 'Buttons', rownames = F,
    options = list(
      dom = 'Bfrtip',digits = 1,
      pageLength = 5, autoWidth = TRUE,
      buttons = c('pageLength','copy', 'csv', 'excel'),
      pagelength = 10, lengthMenu = list(c(10, 25, 100, -1), c('10', '25', '100','All')
      )
      
    )
  )
  
})
