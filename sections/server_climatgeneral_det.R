

climgen_rdet <- eventReactive(list(input$go_climgendet, isolate(input$tab_climgen_det)),{
  
  
  admin <- input$climgen_admin_det
  indic <- input$climgen_ind_det
  scena <- input$climgen_scen_det
  climgen_tip <- input$climgen_tip_det # absolut/abate
  perio_sub <- strsplit(input$climgen_perio_det, "-")[[1]][1] # number of month
  perio_tip <- strsplit(input$climgen_perio_det, "-")[[1]][2] # month/season/year
  
  # selectie unitate
  switch( # alege nume indicator care să fie afișat
    which(c("uat", "jud", "reg") %in%  admin),
    admin_spat <- uat,
    admin_spat <- jud,
    admin_spat <- reg
  )
  
  
  tab <-  read_parquet(paste0("www/data/parquet/climgen/",admin,"/", indic ,"_",scena,"_", perio_tip ,"-50_19710101_21001231.parquet"))
  
  
  an1_abat <- input$slider_climgen_abate_det[1]
  an2_abat <- input$slider_climgen_abate_det[2]
  an1_abs <- input$slider_climgen_absol_det[1]
  an2_abs <- input$slider_climgen_absol_det[2]
  
  # calcul abateri absolute cu funct utils/calcul_climgen_det.R
  tab_sub <- calcul_climgen_det(tab, climgen_tip, perio_sub, indic, an1_abat, an2_abat, an1_abs, an2_abs)
  # unire cu spatial
  admin_spat_sub <- admin_spat |> left_join(tab_sub, by = c( "natcode" = "ID"))
  
  # legenda si intervale functie utils/cols_leg_climgen_det.R
  map_leg <- map_func_cols(indic, climgen_tip, domain = range(admin_spat_sub$value), perio_tip)
  
  
  # text harta
  # text harta
  name_ind <- names(select_climgen_ind)[which(select_climgen_ind %in% indic)] #nume indicator clar
  climgen_perio <- names(select_interv)[which(select_interv %in% input$climgen_perio_det)] # luna.sezon clar
  param_text<- ifelse (
    climgen_tip == "abate", 
    paste(name_ind , " - scenariul", toupper(scena), "schimbare", climgen_perio, an1_abat,"-", an2_abat,  "(perioada de referință 1971-2000)"),
    paste(name_ind , " - scenariul", toupper(scena), "- medii multianuale - ", climgen_perio, an1_abs,"-", an2_abs)
  )
  
  list(
    admin_spat_sub =admin_spat_sub, pal = map_leg$pal, pal_rev = map_leg$pal_rev, tit_leg = map_leg$tit_leg,
    param_text = param_text, opacy = input$transp_climgen_det, tab = tab, perio_sub = perio_sub, indic = indic,
    climgen_tip = climgen_tip, scena = scena, name_ind = name_ind, climgen_perio = climgen_perio, 
    an1_abat = an1_abat, an2_abat = an2_abat, an1_abs = an1_abs, an2_abs = an2_abs,
    admin = admin
  )
  
})

output$climgen_text_det <- renderText({
  climgen_rdet()$param_tex
  
})

output$climgen_map_det <- renderLeaflet ({
  leaflet_fun_det(
    data = isolate(climgen_rdet()$admin_spat_sub),
    pal =  isolate(climgen_rdet()$pal),
    pal_rev =  isolate(climgen_rdet()$pal_rev),
    tit_leg = isolate(climgen_rdet()$tit_leg)
  )
})


observe({
  #req(input$go_climgendet == "Detalii") # Only display if tab is 'Detalii'
  pal_rev =  climgen_rdet()$pal_rev
  tit_leg = climgen_rdet()$tit_leg
  data <- climgen_rdet()$admin_spat_sub
  pal <- climgen_rdet()$pal
  opacy <- climgen_rdet()$opacy 
  
  leafletProxy("climgen_map_det",  data = data)  %>%
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
      title = paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), tit_leg,"</html>"),
      "bottomright", pal = pal_rev, values = ~values, opacity = 1,
      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
    ) 
  
}) 




# reactive values plot ----------------------------------------------------
variables_plot_climgen_det <- reactiveValues(
  input = NULL, title = NULL, cors = NULL, indic = NULL, tip = NULL, 
  # variabile control pentru actualizare grafic/date
  norm = NULL, mean = NULL, change = NULL, id = NULL, name = NULL, county = NULL,
  admin = NULL, update_admin = NULL
  
) 

observeEvent(list(isolate(input$go_climgendet), isolate(input$tab_climgen_det)),{
  variables_plot_climgen_det$admin  <- climgen_rdet()$admin
  admin_spat_sub <- climgen_rdet()$admin_spat_sub
  first_sel <- sample(1:nrow(admin_spat_sub), 1)
  variables_plot_climgen_det$id <-admin_spat_sub$natcode[first_sel]
  variables_plot_climgen_det$name <-admin_spat_sub$name[admin_spat_sub$natcode == variables_plot_climgen_det$id]
  variables_plot_climgen_det$county <-admin_spat_sub$county[admin_spat_sub$natcode == variables_plot_climgen_det$id]
  
})

# pentru actualizare grafic doar cand se schimba regiunea
observe({
  
  variables_plot_climgen_det$admin <- climgen_rdet()$admin
  
  if(!isTRUE(all.equal(variables_plot_climgen_det$admin, variables_plot_climgen_det$update_admin)))  {
    admin <- variables_plot_climgen_det$admin
    admin_spat_sub <- climgen_rdet()$admin_spat_sub
    first_sel <- sample(1:nrow(admin_spat_sub), 1)
    variables_plot_climgen_det$id <- admin_spat_sub$natcode[first_sel]
    variables_plot_climgen_det$name <-admin_spat_sub$name[admin_spat_sub$natcode == variables_plot_climgen_det$id]
    variables_plot_climgen_det$county <-admin_spat_sub$county[admin_spat_sub$natcode == variables_plot_climgen_det$id]
    variables_plot_climgen_det$update_admin <- admin
    
  }
})



observeEvent(input$climgen_map_det_shape_click$id,{ 
  admin_spat_sub <- climgen_rdet()$admin_spat_sub
  variables_plot_climgen_det$id  <- input$climgen_map_det_shape_click$id
  variables_plot_climgen_det$name <-admin_spat_sub$name[admin_spat_sub$natcode == input$climgen_map_det_shape_click$id]
  variables_plot_climgen_det$county <-admin_spat_sub$county[admin_spat_sub$natcode == input$climgen_map_det_shape_click$id]
})


observeEvent(list(input$go_climgendet, variables_plot_climgen_det$id), {
  
  climgen_tip <- climgen_rdet()$climgen_tip
  tab <- climgen_rdet()$tab
  perio_sub <- climgen_rdet()$perio_sub
  indic <- climgen_rdet()$indic
  name_ind <- climgen_rdet()$name_ind
  
  dd <- extract_timeser_det(tab, variables_plot_climgen_det$id, perio_sub, indic)
  
  variables_plot_climgen_det$input <- dd
  variables_plot_climgen_det$indic <-  indic 
  variables_plot_climgen_det$tip <- climgen_tip 
  
  
}) 

# nume grafic
output$condpan_climgen_det <- renderText({
  admin <- climgen_rdet()$admin
  climgen_perio <-  climgen_rdet()$climgen_perio 
  name_ind <- climgen_rdet()$name_ind
  climgen_tip <- climgen_rdet()$climgen_tip
  scena <- climgen_rdet()$scena
  # ajustare nume in functie de unitat administrativa
  climgen_tip_name_ind <- ifelse(climgen_tip == "abate", paste("Schimbare în",tolower(name_ind)), name_ind) 
  if (admin == "reg") name_aadmin <- paste("regiunea", variables_plot_climgen_det$name)
  if (admin == "jud") name_aadmin <- paste("județul", variables_plot_climgen_det$name)
  if (admin == "uat") name_aadmin <- paste(variables_plot_climgen_det$name," - județul ",variables_plot_climgen_det$county)
  paste0(
    climgen_tip_name_ind," ", climgen_perio," ",toupper(scena), 
    " (",name_aadmin ,") 
    - perioada de referință 1971 - 2000"
  )
})



output$climgen_det_stat <- renderUI({
  
  an1_abat <- climgen_rdet()$an1_abat
  an2_abat <- climgen_rdet()$an2_abat
  an1_abs <- climgen_rdet()$an1_abs
  an2_abs <- climgen_rdet()$an2_abs
  admin_spat_sub <- climgen_rdet()$admin_spat_sub
  climgen_tip <- climgen_rdet()$climgen_tip
  
  if( climgen_tip == 'abate') {
    norm <-admin_spat_sub$norm[admin_spat_sub$natcode == variables_plot_climgen_det$id] |> round(1)
    multimean <-admin_spat_sub$p50[admin_spat_sub$natcode == variables_plot_climgen_det$id] |> round(1)
    change <-admin_spat_sub$value[admin_spat_sub$natcode == variables_plot_climgen_det$id] |> round(1)
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
    value <-admin_spat_sub$value[admin_spat_sub$natcode == variables_plot_climgen_det$id] |> round(1)
    
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
output$climgen_timeseries_det_plot <- renderPlotly({
  #req(!is.na(variables_plot_climgen_det$input))
  plt <- plots_det(
    variables_plot_climgen_det$input,
    variables_plot_climgen_det$tip,  
    variables_plot_climgen_det$indic
  )
  plt$gp
})

# pentru afisare subtab date
output$climgen_timeseries_det_data <- DT::renderDT({
  
  DT::datatable(
    variables_plot_climgen_det$input 
    |> dplyr:: mutate(across(is.numeric, round, digits = 1)) |>
      dplyr::select(-ID),
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
