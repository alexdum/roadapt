# update select input year/season/month -----------------------------------
observe({
  indic <- input$transport_ind_det
  if (indic %in% c("wsgsmax", "tasminAdjust", "tasmaxAdjust")) {
    # luni/sezona/an
    updateSelectInput(
      session, "transport_perio_det",
      choices = select_interv,
      selected = select_interv[1]
    )
  } else { # doara anuala cand nu le ai pe celelalte
    updateSelectInput(
      session, "transport_perio_det",
      choices = select_interv[17],
      selected = select_interv[17]
    )
  }
})

transport_rdet <- eventReactive(list(input$go_transportdet, isolate(input$tab_transport_det)),{
  
  
  admin <- input$transport_admin_det
  indic <- input$transport_ind_det
  scena <- input$transport_scen_det
  transport_tip <- input$transport_tip_det # absolut/abate
  perio_sub <- strsplit(input$transport_perio_det, "-")[[1]][1] # number of month
  perio_tip <- strsplit(input$transport_perio_det, "-")[[1]][2] # month/season/year
  indic_path <- indicator_def$path[indicator_def$cod == indic] # calea catre fisier (director parquet)
  
  # selectie unitate
  switch( # alege nume indicator care să fie afișat
    which(c("uat", "jud", "reg") %in%  admin),
    admin_spat <- uat,
    admin_spat <- jud,
    admin_spat <- reg
  )
  
  
  tab <-  read_parquet(paste0("www/data/parquet/",indic_path,"/",admin,"/", indic ,"_",scena,"_", perio_tip ,"-50_19710101_21001231.parquet"))
  
  an1_abat <- input$slider_transport_abate_det[1]
  an2_abat <- input$slider_transport_abate_det[2]
  an1_abs <- input$slider_transport_absol_det[1]
  an2_abs <- input$slider_transport_absol_det[2]
  
  # calcul abateri absolute cu funct utils/calcul_transport_det.R
  tab_sub <- calcul_det(tab, transport_tip, perio_sub, indic, an1_abat, an2_abat, an1_abs, an2_abs)
  # unire cu spatial
  admin_spat_sub <- admin_spat |> left_join(tab_sub, by = c( "natcode" = "ID"))
  
  # legenda si intervale functie utils/cols_leg_transport_det.R
  map_leg <- map_func_cols(indic, transport_tip, domain = range(admin_spat_sub$value), perio_tip)
  
  
  # text harta
  # text harta
  name_ind <- names(select_transport_ind)[which(select_transport_ind %in% indic)] #nume indicator clar
  transport_perio <- names(select_interv)[which(select_interv %in% input$transport_perio_det)] # luna.sezon clar
  param_text<- ifelse (
    transport_tip == "abate", 
    paste(name_ind , " - scenariul", toupper(scena), "schimbare", transport_perio, an1_abat,"-", an2_abat,  "(perioada de referință 1971-2000)"),
    paste(name_ind , " - scenariul", toupper(scena), "- medii multianuale - ", transport_perio, an1_abs,"-", an2_abs)
  )
  
  list(
    admin_spat_sub = admin_spat_sub, pal = map_leg$pal, pal_rev = map_leg$pal_rev, tit_leg = map_leg$tit_leg,
    param_text = param_text, opacy = input$transp_transport_det, tab = tab, perio_sub = perio_sub, indic = indic,
    transport_tip = transport_tip, scena = scena, name_ind = name_ind, transport_perio = transport_perio, 
    an1_abat = an1_abat, an2_abat = an2_abat, an1_abs = an1_abs, an2_abs = an2_abs,
    admin = admin
  )
  
})

output$transport_text_det <- renderText({
  transport_rdet()$param_tex
  
})

output$transport_map_det <- renderLeaflet ({
  leaflet_fun_det(
    data = isolate(transport_rdet()$admin_spat_sub),
    pal =  isolate(transport_rdet()$pal),
    pal_rev =  isolate(transport_rdet()$pal_rev),
    tit_leg = isolate(transport_rdet()$tit_leg)
  )
})


observe({
  #req(input$go_transportdet == "Detalii") # Only display if tab is 'Detalii'
  pal_rev =  transport_rdet()$pal_rev
  tit_leg = transport_rdet()$tit_leg
  data <- transport_rdet()$admin_spat_sub
  pal <- transport_rdet()$pal
  opacy <- transport_rdet()$opacy 
  
  leafletProxy("transport_map_det",  data = data)  %>%
    clearShapes() %>%
    addPolygons(
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
variables_plot_transport_det <- reactiveValues(
  input = NULL, title = NULL, cors = NULL, indic = NULL, tip = NULL, 
  # variabile control pentru actualizare grafic/date
  norm = NULL, mean = NULL, change = NULL, id = NULL, name = NULL, county = NULL,
  admin = NULL, update_admin = NULL
  
) 

observeEvent(list(isolate(input$go_transportdet), isolate(input$tab_transport_det)),{
  variables_plot_transport_det$admin  <- transport_rdet()$admin
  admin_spat_sub <- transport_rdet()$admin_spat_sub
  first_sel <- sample(1:nrow(admin_spat_sub), 1)
  variables_plot_transport_det$id <- admin_spat_sub$natcode[first_sel]
  variables_plot_transport_det$name <- admin_spat_sub$name[admin_spat_sub$natcode == variables_plot_transport_det$id]
  variables_plot_transport_det$county <- admin_spat_sub$county[admin_spat_sub$natcode == variables_plot_transport_det$id]
  
})

# pentru actualizare grafic doar cand se schimba regiunea
observe({
  
  variables_plot_transport_det$admin <- transport_rdet()$admin
  
  if (!isTRUE(all.equal(variables_plot_transport_det$admin, variables_plot_transport_det$update_admin)))  {
    admin <- variables_plot_transport_det$admin
    admin_spat_sub <- transport_rdet()$admin_spat_sub
    first_sel <- sample(1:nrow(admin_spat_sub), 1)
    variables_plot_transport_det$id <- admin_spat_sub$natcode[first_sel]
    variables_plot_transport_det$name <- admin_spat_sub$name[admin_spat_sub$natcode == variables_plot_transport_det$id]
    variables_plot_transport_det$county <- admin_spat_sub$county[admin_spat_sub$natcode == variables_plot_transport_det$id]
    variables_plot_transport_det$update_admin <- admin
  }
})



observeEvent(input$transport_map_det_shape_click$id,{ 
  admin_spat_sub <- transport_rdet()$admin_spat_sub
  variables_plot_transport_det$id  <- input$transport_map_det_shape_click$id
  variables_plot_transport_det$name <- admin_spat_sub$name[admin_spat_sub$natcode == input$transport_map_det_shape_click$id]
  variables_plot_transport_det$county <- admin_spat_sub$county[admin_spat_sub$natcode == input$transport_map_det_shape_click$id]
})


observeEvent(list(input$go_transportdet, variables_plot_transport_det$id), {
  
  transport_tip <- transport_rdet()$transport_tip
  tab <- transport_rdet()$tab
  perio_sub <- transport_rdet()$perio_sub
  indic <- transport_rdet()$indic
  name_ind <- transport_rdet()$name_ind
  
  dd <- extract_timeser_det(tab, variables_plot_transport_det$id, perio_sub, indic)
  
  variables_plot_transport_det$input <- dd
  variables_plot_transport_det$indic <-  indic 
  variables_plot_transport_det$tip <- transport_tip 
  
  
}) 

# nume grafic
output$condpan_transport_det <- renderText({
  admin <- transport_rdet()$admin
  transport_perio <-  transport_rdet()$transport_perio 
  name_ind <- transport_rdet()$name_ind
  transport_tip <- transport_rdet()$transport_tip
  scena <- transport_rdet()$scena
  # ajustare nume in functie de unitat administrativa
  transport_tip_name_ind <- ifelse(transport_tip == "abate", paste("Schimbare în",tolower(name_ind)), name_ind) 
  if (admin == "reg") name_aadmin <- paste("regiunea", variables_plot_transport_det$name)
  if (admin == "jud") name_aadmin <- paste("județul", variables_plot_transport_det$name)
  if (admin == "uat") name_aadmin <- paste(variables_plot_transport_det$name," - județul ",variables_plot_transport_det$county)
  paste0(
    transport_tip_name_ind," ", transport_perio," ",toupper(scena), 
    " (",name_aadmin ,") 
    - perioada de referință 1971 - 2000"
  )
})



output$transport_det_stat <- renderUI({
  
  an1_abat <- transport_rdet()$an1_abat
  an2_abat <- transport_rdet()$an2_abat
  an1_abs <- transport_rdet()$an1_abs
  an2_abs <- transport_rdet()$an2_abs
  admin_spat_sub <- transport_rdet()$admin_spat_sub
  transport_tip <- transport_rdet()$transport_tip
  
  if (transport_tip == 'abate') {
    norm <- admin_spat_sub$norm[admin_spat_sub$natcode == variables_plot_transport_det$id] |> round(1)
    multimean <- admin_spat_sub$p50[admin_spat_sub$natcode == variables_plot_transport_det$id] |> round(1)
    change <- admin_spat_sub$value[admin_spat_sub$natcode == variables_plot_transport_det$id] |> round(1)
    
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
    value <-admin_spat_sub$value[admin_spat_sub$natcode == variables_plot_transport_det$id] |> round(1)
    
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
output$transport_timeseries_det_plot <- renderPlotly({
  #req(!is.na(variables_plot_transport_det$input))
  plt <- plots_det(
    variables_plot_transport_det$input,
    variables_plot_transport_det$tip,  
    variables_plot_transport_det$indic
  )
  plt$gp
})

# pentru afisare subtab date
output$transport_timeseries_det_data <- DT::renderDT({
  
  DT::datatable(
    variables_plot_transport_det$input |> 
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
