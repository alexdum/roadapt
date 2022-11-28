
agr_rdet <- eventReactive(list(input$go_agrdet, isolate(input$tab_agro_det)),{
  
  param <- input$agr_ind_det
  scen <- input$agr_scen_det
  tab <-  open_dataset(paste0("www/data/parquet/agro/", param ,"Adjust_",scen,"_month-50_19710101_21001231.parquet"))
  
  tab.sub <- tab |> 
    filter(year == 2022, month == 5) |>
    collect()
  uat.sub <- uat |> left_join(tab.sub, by = c( "natCode" = "ID"))
  
  
  
  
  list(
    uat.sub = uat.sub
  )
  
})



output$agr_map_det <- renderLeaflet ({
  
  
  qpal <- colorBin("Blues", domain = range(agr_rdet()$uat.sub$value), bins = 4)
  
  #pal <- colorBin(cols, domain = agr_rdet()$uat.sub$value, bins = 4)
  #pal2 <- colorBin(cols, domain = shape$values, bins = bins, reverse = T)
  
  leaflet_fun_det(
    data = agr_rdet()$uat.sub
  )
})