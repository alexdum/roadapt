
agr_rdet <- eventReactive(list(input$go_agrdet, isolate(input$tab_agro_det)),{
  
  indic <- input$agr_ind_det
  scena <- input$agr_scen_det
  agr_tip <- input$agr_tip_det # absolut/abate
  perio_sub <- strsplit(input$agr_perio_det, "-")[[1]][1] # number of month
  perio_tip <- strsplit(input$agr_perio_det, "-")[[1]][2] # month/season/year
  tab <-  read_parquet(paste0("www/data/parquet/agro/", indic ,"Adjust_",scena,"_", perio_tip ,"-50_19710101_21001231.parquet"))
  
  
  an1_abat <- input$slider_agro_abate_det[1]
  an2_abat <- input$slider_agro_abate_det[2]
  an1_abs <- input$slider_agro_absol_det[1]
  an2_abs <- input$slider_agro_absol_det[2]
  
  # calcul abateri absolute cu funct utils/calcul_agr_det.R
  tab_sub <- calcul_agro_det(tab, agr_tip, perio_sub, indic, an1_abat, an2_abat, an1_abs, an2_abs)
  # unire cu spatial
  uat_sub <- uat |> left_join(tab_sub, by = c( "natcode" = "ID"))
  
  # legenda si intervale functie utils/cols_leg_agr_det.R
  map_leg <- map_func_cols(indic, agr_tip, domain = range(uat_sub$value), perio_tip)
  
  
  # text harta
  # text harta
  name_ind <- names(select_agro_ind)[which(select_agro_ind %in% indic)] #nume indicator clar
  agro_perio <- names(select_interv)[which(select_interv %in% input$agr_perio_det)] # luna.sezon clar
  param_text<- ifelse (
    agr_tip == "abate", 
    paste(name_ind , " - scenariul", toupper(scena), "schimbare", agro_perio, an1_abat,"-", an2_abat,  "(perioada de referință 1971-2000)"),
    paste(name_ind , " - scenariul", toupper(scena), "- medii multianuale - ", agro_perio, an1_abs,"-", an2_abs)
  )
  
  list(
    uat_sub = uat_sub, pal = map_leg$pal, pal_rev = map_leg$pal_rev, tit_leg = map_leg$tit_leg,
    param_text = param_text, opacy = input$transp_agr_det, tab = tab, perio_sub = perio_sub, indic = indic,
    agr_tip = agr_tip, scena = scena, name_ind = name_ind, agro_perio = agro_perio, 
    an1_abat = an1_abat, an2_abat = an2_abat, an1_abs = an1_abs, an2_abs = an2_abs
  )
  
})

output$agr_text_det <- renderText({
  agr_rdet()$param_tex
  
})

output$agr_map_det <- renderLeaflet ({
  leaflet_fun_det(
    data = isolate(agr_rdet()$uat_sub),
    pal =  isolate(agr_rdet()$pal),
    pal_rev =  isolate(agr_rdet()$pal_rev),
    tit_leg = isolate(agr_rdet()$tit_leg)
  )
})


observe({
  #req(input$go_agrdet == "Detalii") # Only display if tab is 'Detalii'
  pal_rev =  agr_rdet()$pal_rev
  tit_leg = agr_rdet()$tit_leg
  data <- agr_rdet()$uat_sub
  pal <- agr_rdet()$pal
  opacy <- agr_rdet()$opacy 
  
  leafletProxy("agr_map_det",  data = data)  %>%
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

variables_plot_agro_det <- reactiveValues(
  input = NULL, title = NULL, cors = NULL, indic = NULL, tip = NULL, 
  # variabile control pentru actualizare grafic/date
  norm = NULL, mean = NULL, change = NULL, id = NULL, name = NULL, county = NULL
)


# reactive values plot ----------------------------------------------------

observeEvent(list(isolate(input$go_agrdet), isolate(input$tab_agro_det)),{
  uat_sub <- agr_rdet()$uat_sub
  first_sel <- sample(1:nrow(uat_sub), 1)
  variables_plot_agro_det$id <- uat_sub$natcode[first_sel]
  variables_plot_agro_det$name <- uat_sub$name[uat_sub$natcode == variables_plot_agro_det$id]
  variables_plot_agro_det$county <- uat_sub$county[uat_sub$natcode == variables_plot_agro_det$id]
  
})


observeEvent(input$agr_map_det_shape_click$id,{ 
  uat_sub <- agr_rdet()$uat_sub
  variables_plot_agro_det$id  <- input$agr_map_det_shape_click$id
  variables_plot_agro_det$name <- uat_sub$name[uat_sub$natcode == input$agr_map_det_shape_click$id]
  variables_plot_agro_det$county <- uat_sub$county[uat_sub$natcode == input$agr_map_det_shape_click$id]
})


observeEvent(list(input$go_agrdet, variables_plot_agro_det$id), {
  agr_tip <- agr_rdet()$agr_tip
  tab <- agr_rdet()$tab
  perio_sub <- agr_rdet()$perio_sub
  indic <- agr_rdet()$indic
  name_ind <- agr_rdet()$name_ind
  
  dd <- extract_timeser_det(tab, variables_plot_agro_det$id, perio_sub, indic)

  variables_plot_agro_det$input <- dd
  variables_plot_agro_det$indic <-  indic 
  variables_plot_agro_det$tip <- agr_tip 
  
  
}) 


output$condpan_agro_det <- renderText({
  agro_perio <-  agr_rdet()$agro_perio 
  name_ind <- agr_rdet()$name_ind
  agr_tip <- agr_rdet()$agr_tip
  scena <- agr_rdet()$scena
  agr_tip_name_ind <- ifelse(agr_tip == "abate", paste("Schimbare în",tolower(name_ind)), name_ind) 
  paste0(
    agr_tip_name_ind," ", agro_perio," ",toupper(scena), " (",variables_plot_agro_det$name," - județul ",variables_plot_agro_det$county,") - perioada de referință 1971 - 2000"
  )
})



output$agro_det_stat <- renderUI({
  
  an1_abat <- agr_rdet()$an1_abat
  an2_abat <- agr_rdet()$an2_abat
  an1_abs <- agr_rdet()$an1_abs
  an2_abs <- agr_rdet()$an2_abs
  uat_sub <- agr_rdet()$uat_sub
  agr_tip <- agr_rdet()$agr_tip
  
  if( agr_tip == 'abate') {
    norm <- uat_sub$norm[uat_sub$natcode == variables_plot_agro_det$id] |> round(1)
    multimean <- uat_sub$p50[uat_sub$natcode == variables_plot_agro_det$id] |> round(1)
    change <- uat_sub$value[uat_sub$natcode == variables_plot_agro_det$id] |> round(1)
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
    value <- uat_sub$value[uat_sub$natcode == variables_plot_agro_det$id] |> round(1)
    
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
output$agro_timeseries_det_plot <- renderPlotly({
  #req(!is.na(variables_plot_agro_det$input))
  plt <- plots_agro_det(
    variables_plot_agro_det$input,
    variables_plot_agro_det$tip,  
    variables_plot_agro_det$indic
  )
  plt$gp
})

# pentru afisare subtab date
output$agro_timeseries_det_data <- DT::renderDT({
  
  DT::datatable(
    variables_plot_agro_det$input 
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
