# update select input year/season/month -----------------------------------

observe({
  indic <- input$hidro_ind_det
  if (indic %in% c("prAdjust")) {
    # luni/sezona/an
    updateSelectInput(
      session, "hidro_perio_det",
      choices = select_interv,
      selected = select_interv[1]
    )
  } else { # doara anuala cand nu le ai pe celelalte
    updateSelectInput(
      session, "hidro_perio_det",
      choices = select_interv[17],
      selected = select_interv[17]
    )
  }
})

hidro_rdet <- eventReactive(list(input$go_hidrodet, isolate(input$tab_hidro_det)),{
  
  
  admin <- input$hidro_admin_det
  indic <- input$hidro_ind_det
  scena <- input$hidro_scen_det
  hidro_tip <- input$hidro_tip_det # absolut/abate
  perio_sub <- strsplit(input$hidro_perio_det, "-")[[1]][1] # number of month
  perio_tip <- strsplit(input$hidro_perio_det, "-")[[1]][2] # month/season/year
  indic_path <- indicator_def$path[indicator_def$cod == indic] # calea catre fisier (director parquet)
  
  # selectie unitate
  switch( # alege nume indicator care să fie afișat
    which(c("uat", "jud", "reg") %in%  admin),
    admin_spat <- uat,
    admin_spat <- jud,
    admin_spat <- reg
  )
  
  
  tab <-  read_parquet(paste0("www/data/parquet/",indic_path,"/",admin,"/", indic ,"_",scena,"_", perio_tip ,"-50_19710101_21001231.parquet"))

  an1_abat <- input$slider_hidro_abate_det[1]
  an2_abat <- input$slider_hidro_abate_det[2]
  an1_abs <- input$slider_hidro_absol_det[1]
  an2_abs <- input$slider_hidro_absol_det[2]
  
  # calcul abateri absolute cu funct utils/calcul_hidro_det.R
  tab_sub <- calcul_det(tab, hidro_tip, perio_sub, indic, an1_abat, an2_abat, an1_abs, an2_abs)
  # valori minime maxime pentru plotare reclasificare valori/plotare sugestiva
  min_max <- map_func_min_max(indic = indic, ind_tip =  hidro_tip, perio_tip = perio_tip)
  tab_sub$value[tab_sub$value > min_max[2]] <- min_max[2]
  tab_sub$value[tab_sub$value < min_max[1]] <- min_max[1]
  # unire cu spatial
  admin_spat_sub <- admin_spat |> left_join(tab_sub, by = c( "natcode" = "ID"))
  
  # legenda si intervale functie utils/cols_leg_hidro_det.R
  map_leg <- map_func_cols(indic, hidro_tip, domain = range(admin_spat_sub$value), perio_tip)
  
  
  # text harta
  # text harta
  name_ind <- names(select_hidro_ind)[which(select_hidro_ind %in% indic)] #nume indicator clar
  hidro_perio <- names(select_interv)[which(select_interv %in% input$hidro_perio_det)] # luna.sezon clar
  param_text<- ifelse (
    hidro_tip == "abate", 
    paste(name_ind , " - scenariul", toupper(scena), "schimbare", hidro_perio, an1_abat,"-", an2_abat,  "(perioada de referință 1971-2000)"),
    paste(name_ind , " - scenariul", toupper(scena), "- medii multianuale - ", hidro_perio, an1_abs,"-", an2_abs)
  )
  
  list(
    admin_spat_sub =admin_spat_sub, pal = map_leg$pal, pal_rev = map_leg$pal_rev, tit_leg = map_leg$tit_leg,
    param_text = param_text, opacy = input$transp_hidro_det, tab = tab, perio_sub = perio_sub, indic = indic,
    hidro_tip = hidro_tip, scena = scena, name_ind = name_ind, hidro_perio = hidro_perio, 
    an1_abat = an1_abat, an2_abat = an2_abat, an1_abs = an1_abs, an2_abs = an2_abs,
    admin = admin
  )
  
})

output$hidro_text_det <- renderText({
  hidro_rdet()$param_tex
  
})

output$hidro_map_det <- renderLeaflet ({
  leaflet_fun_det(
    data = isolate(hidro_rdet()$admin_spat_sub),
    pal =  isolate(hidro_rdet()$pal),
    pal_rev =  isolate(hidro_rdet()$pal_rev),
    tit_leg = isolate(hidro_rdet()$tit_leg)
  )
})


observe({
  #req(input$go_hidrodet == "Detalii") # Only display if tab is 'Detalii'
  pal_rev =  hidro_rdet()$pal_rev
  tit_leg = hidro_rdet()$tit_leg
  data <- hidro_rdet()$admin_spat_sub
  pal <- hidro_rdet()$pal
  opacy <- hidro_rdet()$opacy 
  
  leafletProxy("hidro_map_det",  data = data)  %>%
    clearShapes() %>%
    addPolygons (
      fillColor = ~pal(value),
      label = ~paste("<font size='2'><b>Region type: UAT<br/>Name units:",name,
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
variables_plot_hidro_det <- reactiveValues(
  input = NULL, title = NULL, cors = NULL, indic = NULL, tip = NULL, 
  # variabile control pentru actualizare grafic/date
  norm = NULL, mean = NULL, change = NULL, id = NULL, name = NULL, county = NULL,
  admin = NULL, update_admin = NULL
  
) 

observeEvent(list(isolate(input$go_hidrodet), isolate(input$tab_hidro_det)),{
  variables_plot_hidro_det$admin  <- hidro_rdet()$admin
  admin_spat_sub <- hidro_rdet()$admin_spat_sub
  first_sel <- sample(1:nrow(admin_spat_sub), 1)
  variables_plot_hidro_det$id <-admin_spat_sub$natcode[first_sel]
  variables_plot_hidro_det$name <-admin_spat_sub$name[admin_spat_sub$natcode == variables_plot_hidro_det$id]
  variables_plot_hidro_det$county <-admin_spat_sub$county[admin_spat_sub$natcode == variables_plot_hidro_det$id]
  
})

# pentru actualizare grafic doar cand se schimba regiunea
observe({
  
  variables_plot_hidro_det$admin <- hidro_rdet()$admin
  
  if(!isTRUE(all.equal(variables_plot_hidro_det$admin, variables_plot_hidro_det$update_admin)))  {
    admin <- variables_plot_hidro_det$admin
    admin_spat_sub <- hidro_rdet()$admin_spat_sub
    first_sel <- sample(1:nrow(admin_spat_sub), 1)
    variables_plot_hidro_det$id <- admin_spat_sub$natcode[first_sel]
    variables_plot_hidro_det$name <-admin_spat_sub$name[admin_spat_sub$natcode == variables_plot_hidro_det$id]
    variables_plot_hidro_det$county <-admin_spat_sub$county[admin_spat_sub$natcode == variables_plot_hidro_det$id]
    variables_plot_hidro_det$update_admin <- admin
    
  }
})



observeEvent(input$hidro_map_det_shape_click$id,{ 
  admin_spat_sub <- hidro_rdet()$admin_spat_sub
  variables_plot_hidro_det$id  <- input$hidro_map_det_shape_click$id
  variables_plot_hidro_det$name <-admin_spat_sub$name[admin_spat_sub$natcode == input$hidro_map_det_shape_click$id]
  variables_plot_hidro_det$county <-admin_spat_sub$county[admin_spat_sub$natcode == input$hidro_map_det_shape_click$id]
})


observeEvent(list(input$go_hidrodet, variables_plot_hidro_det$id), {
  
  hidro_tip <- hidro_rdet()$hidro_tip
  tab <- hidro_rdet()$tab
  perio_sub <- hidro_rdet()$perio_sub
  indic <- hidro_rdet()$indic
  name_ind <- hidro_rdet()$name_ind
  
  dd <- extract_timeser_det(tab, variables_plot_hidro_det$id, perio_sub, indic)
  
  variables_plot_hidro_det$input <- dd
  variables_plot_hidro_det$indic <-  indic 
  variables_plot_hidro_det$tip <- hidro_tip 
  
  
}) 

# nume grafic
output$condpan_hidro_det <- renderText({
  admin <- hidro_rdet()$admin
  hidro_perio <-  hidro_rdet()$hidro_perio 
  name_ind <- hidro_rdet()$name_ind
  hidro_tip <- hidro_rdet()$hidro_tip
  scena <- hidro_rdet()$scena
  # ajustare nume in functie de unitat administrativa
  hidro_tip_name_ind <- ifelse(hidro_tip == "abate", paste("Schimbare în",tolower(name_ind)), name_ind) 
  if (admin == "reg") name_aadmin <- paste("regiunea", variables_plot_hidro_det$name)
  if (admin == "jud") name_aadmin <- paste("județul", variables_plot_hidro_det$name)
  if (admin == "uat") name_aadmin <- paste(variables_plot_hidro_det$name," - județul ",variables_plot_hidro_det$county)
  paste0(
    hidro_tip_name_ind," ", hidro_perio," ",toupper(scena), 
    " (",name_aadmin ,") 
    - perioada de referință 1971 - 2000"
  )
})



output$hidro_det_stat <- renderUI({
  
  an1_abat <- hidro_rdet()$an1_abat
  an2_abat <- hidro_rdet()$an2_abat
  an1_abs <- hidro_rdet()$an1_abs
  an2_abs <- hidro_rdet()$an2_abs
  admin_spat_sub <- hidro_rdet()$admin_spat_sub
  hidro_tip <- hidro_rdet()$hidro_tip
  
  if( hidro_tip == 'abate') {
    norm <-admin_spat_sub$norm[admin_spat_sub$natcode == variables_plot_hidro_det$id] |> round(1)
    multimean <-admin_spat_sub$p50[admin_spat_sub$natcode == variables_plot_hidro_det$id] |> round(1)
    change <-admin_spat_sub$value[admin_spat_sub$natcode == variables_plot_hidro_det$id] |> round(1)
    print(paste(an1_abat, an2_abat, norm, multimean, change))
    
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
    value <-admin_spat_sub$value[admin_spat_sub$natcode == variables_plot_hidro_det$id] |> round(1)
    
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
output$hidro_timeseries_det_plot <- renderPlotly({
  #req(!is.na(variables_plot_hidro_det$input))
  plt <- plots_det(
    variables_plot_hidro_det$input,
    variables_plot_hidro_det$tip,  
    variables_plot_hidro_det$indic
  )
  plt$gp
})

# pentru afisare subtab date
output$hidro_timeseries_det_data <- DT::renderDT({
  
  DT::datatable(
    variables_plot_hidro_det$input |> 
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
