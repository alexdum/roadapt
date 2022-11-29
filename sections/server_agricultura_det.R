
agr_rdet <- eventReactive(list(input$go_agrdet, isolate(input$tab_agro_det)),{
  
  ind <- input$agr_ind_det
  scena <- input$agr_scen_det
  agr_tip_det <- input$agr_tip_det
  perio_tip <- strsplit(input$agr_perio_det, "-")[[1]][2]
  perio_sub <- strsplit(input$agr_perio_det, "-")[[1]][1]
  tab <-  open_dataset(paste0("www/data/parquet/agro/", ind ,"Adjust_",scena,"_", perio_tip ,"-50_19710101_21001231.parquet"))
  
  #subseteaza dupa tip/an
  if (agr_tip_det == "abate") {
    an1 <- input$slider_agro_abate[1]
    an2 <- input$slider_agro_abate[2]
    
    if (perio_sub != "year") { #daca ai an formateaza data diferit
      tab.sub <- tab |> 
        filter( month ==  as.integer(perio_sub)) |>
        to_duckdb() |>
        mutate(norm = mean(p50[year >= 1971 & year <= 2000])) |> 
        filter(year >= an1, year <= an2) |>
        group_by(ID) |> summarise(p50 = mean(p50) , norm = mean(norm)) |>
        mutate(value = p50 - norm) |>
        collect()
    } else {
      tab.sub <- tab |> 
        filter(year >= 2000, year <= 2000) |>
        group_by(ID) |> summarise(value = mean(p50)) |>
        collect() 
    }
  } else {
    an1 <- input$slider_agro_absol[1]
    an2 <- input$slider_agro_absol[2]
    if (perio_sub != "year") { #daca ai an formateaza data diferit
      tab.sub <- tab |> 
        filter( month ==  as.integer(perio_sub), year >= an1, year <= an2) |>
        group_by(ID) |> summarise(value = mean(p50)) |>
        collect()
    } else {
      tab.sub <- tab |> 
        filter(year >= an1, year <= an2) |>
        group_by(ID) |> summarise(value = mean(p50)) |>
        collect() 
    }
  }
  
  
  uat.sub <- uat |> left_join(tab.sub, by = c( "natCode" = "ID"))
  
  print(uat.sub)
  
  
  
  
  list(
    uat.sub = uat.sub
  )
  
})



output$agr_map_det <- renderLeaflet ({
  
  
  qpal <- colorBin("Blues", domain = range(agr_rdet()$uat.sub$p50), bins = 4)
  
  #pal <- colorBin(cols, domain = agr_rdet()$uat.sub$value, bins = 4)
  #pal2 <- colorBin(cols, domain = shape$values, bins = bins, reverse = T)
  
  leaflet_fun_det(
    data = agr_rdet()$uat.sub
  )
})