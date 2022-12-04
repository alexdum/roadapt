
agr_rdet <- eventReactive(list(input$go_agrdet, isolate(input$tab_agro_det)),{
  
  indic <- input$agr_ind_det
  scena <- input$agr_scen_det
  agr_tip <- input$agr_tip_det
  perio_sub <- strsplit(input$agr_perio_det, "-")[[1]][1] # number of month
  perio_tip <- strsplit(input$agr_perio_det, "-")[[1]][2] # month/season/year
  tab <-  read_parquet(paste0("www/data/parquet/agro/", indic ,"Adjust_",scena,"_", perio_tip ,"-50_19710101_21001231.parquet"))

  
 an1_abat <- input$slider_agro_abate_det[1]
 an2_abat <- input$slider_agro_abate_det[2]
 an1_abs <- input$slider_agro_absol_det[1]
 an2_abs <- input$slider_agro_absol_det[2]
  
  # calcul abateri absolute cu funct utils/calcul_agr_det.R
  tab.sub <- calcul_agro_det(tab, agr_tip, perio_sub, indic, an1_abat, an2_abat, an1_abs, an2_abs)
  # unire cu spatial
  uat.sub <- uat |> left_join(tab.sub, by = c( "natCode" = "ID"))
  #print(tab.sub)
 
  # legenda si intervale functie utils/cols_leg_agr_det.R
  map_leg <- map_func_cols(indic, agr_tip, domain = range(uat.sub$value), perio_tip)
  
  
  # text harta
  param_text<- ifelse (
    agr_tip == "abate", 
    paste(names(select_agro_ind)[which(select_agro_ind %in% indic)], " - scenariul", toupper(scena),
          "schimbare", names(select_interv)[which(select_interv %in% input$agr_perio)], 
          an1_abat,"-", an2_abat,  "(perioada de referință 1971-2000)"
    ),
    paste(names(select_agro_ind)[which(select_agro_ind %in% indic)], " - scenariul", toupper(scena),
          "- medii multianuale - ", names(select_interv)[which(select_interv %in% input$agr_perio)], 
          an1_abs,"-", an2_abs
    )
  )
  
  list(
    uat.sub = uat.sub, pal = map_leg$pal, pal_rev = map_leg$pal_rev, tit_leg = map_leg$tit_leg,
    param_text = param_text, opacy = input$transp_agr_det
  )
  
})

output$agr_text_det <- renderText({
  agr_rdet()$param_tex
  
})

output$agr_map_det <- renderLeaflet ({
  leaflet_fun_det(
    data = isolate(agr_rdet()$uat.sub),
    pal =  isolate(agr_rdet()$pal),
    pal_rev =  isolate(agr_rdet()$pal_rev),
    tit_leg = isolate(agr_rdet()$tit_leg)
  )
})


observe({
  #req(input$go_agrdet == "Detalii") # Only display if tab is 'Detalii'
  pal_rev =  agr_rdet()$pal_rev
  tit_leg = agr_rdet()$tit_leg
  data <- agr_rdet()$uat.sub
  pal <- agr_rdet()$pal
  opacy <- agr_rdet()$opacy 
  
  leafletProxy("agr_map_det",  data = data)  %>%
    clearShapes() %>%
    addPolygons (
      fillColor = ~pal(value),
      label = ~paste("<font size='2'><b>Region type: UAT<br/>Name units:",name_1,
                     "<br/>",round(value,1),"</b></font><br/>
                       <font size='1' color='#E95420'>Click to
                       get values and graph</font>") %>% lapply(htmltools::HTML),
      labelOptions = labelOptions(textsize = "13px"),
      color = "grey",
      weight = 0.5, smoothFactor = 0.1,
      opacity = 0.5,
      fillOpacity = opacy ,
      layerId = ~natCode,
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
