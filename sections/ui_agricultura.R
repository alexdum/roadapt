ui_agricultura <- tabPanel(
  title = "Agricultura", icon = icon("tractor"),
  value = "#agricultura", id = "#agricultura",
  tabsetPanel(
    tabPanel(
      id = "tab_agro_gen",
      title = "General",
      fluidRow(
        column(
          width = 2,
          selectInput(
            inputId = "agr_ind",
            label = "Indicator",
            choices = select_agro_ind,
            selected =  select_agro_ind[1]
          ),
          selectInput(
            inputId = "agr_tip",
            label = "Tip",
            choices = list(
              `Valori absolute` = "absol",
              `Schimbare` = "abate"
            ),
            selected = "abate"
          ),
          selectInput(
            inputId = "agr_scen",
            label = "Scenariu",
            choices = list(
              `RCP4.5` = "rcp45",
              `RCP8.5` = "rcp85"
            ), 
            selected = " rcp45"
          ),
          selectInput(
            inputId = "agr_perio",
            label = "Luna/Sezon",
            choices = select_interv,
            selected =  select_interv[1]
          ),
          conditionalPanel(
            condition = "input.agr_tip == 'abate'",
            sliderInput(
              "slider_agro_abate_gen", label = "Interval calcul", min = 2006, 
              max = 2100, value = c(2071, 2100), dragRange = T, ticks = F,
              sep = "", step = 1)
          ),
          conditionalPanel(
            condition = "input.agr_tip == 'absol'",
            sliderInput(
              "slider_agro_absol_gen", label = "Interval calcul", min = 1971, 
              max = 2100, value = c(1971, 2000), dragRange = T, ticks = F,
              sep = "", step = 1)
          ),
          sliderInput(
            "transp_agr_gen", "Transparență raster",
            min = 0, max = 1, ticks = F,
            value = 0.8, step = 0.1,
          ),
          
          actionButton("go_agrgen", "Actualizare harta", icon("sync")),
          radioButtons( # radio button show values
            "radio_agr_gen", label = "Harta click funcționalitate",
            choices = 
              list(
                "Afișează valoare raster" = 1, 
                "Grafic serie temporală (afișare sub hartă)" = 2
              ), 
            selected = 1
          ),
        ),
        column(
          width = 7,
          
          h6(textOutput("agr_text_gen"), style = "text-align:center;"),
          wellPanel(
            leafletOutput("agr_map_gen")
          ),
          br(),
          conditionalPanel( # show graphs only when data available
            condition = "input.radio_agr_gen == 2 && output.condpan_agro_gen != 'nas'",
            
            tabsetPanel(
              fluidRow (
                h6(textOutput("condpan_agro_gen"), style = "text-align:center;")
              ),
              tabPanel(
                value = "Grafic",
                title = h6("Grafic"),
                wellPanel(
                  plotlyOutput("agro_timeseries_gen_plot") |> withSpinner(size = 0.5),
                  #downloadLink('down_plot_regio_ind', label = 'Download  PNG')
                )
              ), 
              tabPanel(
                value = "Data",
                title = h6("Data"),
                wellPanel(
                  DT::dataTableOutput("agro_timeseries_gen_data")
                )
              )
            ),
            
          ),
          conditionalPanel(
            condition = "input.radio_agr_gen == 2 && output.condpan_agro_gen == 'nas'",
            wellPanel(
              p("Trebuie selectată o regiune cu valori disponibile")
            )
          )
        )
      )
    ),
    tabPanel(
      id = "tab_agro_det",
      title = "Detalii",
      fluidRow(
        column(
          width = 2,
          selectInput(
            inputId = "agr_ind_det",
            label = "Indicator",
            choices = select_agro_ind,
            selected =  select_agro_ind[1]
          ),
          selectInput(
            inputId = "agr_tip_det",
            label = "Tip",
            choices = list(
              `Valori absolute` = "absol",
              `Schimbare` = "abate"
            ),
            selected = "abate"
          ),
          selectInput(
            inputId = "agr_scen_det",
            label = "Scenariu",
            choices = list(
              `RCP4.5` = "rcp45",
              `RCP8.5` = "rcp85"
            ), 
            selected = " rcp45"
          ),
          selectInput(
            inputId = "agr_perio_det",
            label = "Luna/Sezon",
            choices = select_interv,
            selected =  select_interv[1]
          ),
          conditionalPanel(
            condition = "input.agr_tip_det == 'abate'",
            sliderInput(
              "slider_agro_abate_det", label = "Interval calcul", min = 2006, 
              max = 2100, value = c(2071, 2100), dragRange = T, ticks = F,
              sep = "", step = 1)
          ),
          conditionalPanel(
            condition = "input.agr_tip_det== 'absol'",
            sliderInput(
              "slider_agro_absol_det", label = "Interval calcul", min = 1971, 
              max = 2100, value = c(1971, 2000), dragRange = T, ticks = F,
              sep = "", step = 1)
          ),
          sliderInput(
            "transp_agr_det", "Transparență poligon",
            min = 0, max = 1, ticks = F,
            value = 0.7, step = 0.1,
          ),
          actionButton("go_agrdet", "Actualizare harta", icon("sync"))
        ),
        column(
          width = 7,
          h6(textOutput("agr_text_det"), style = "text-align:center;"),
          wellPanel(
            leafletOutput("agr_map_det") |> withSpinner(size = 0.5)
          ),
          br(),
          tabsetPanel(
            fluidRow (
              h6(textOutput("condpan_agro_det"), style = "text-align:center;")
            ),
            tabPanel(
              value = "Grafic",
              title = h6("Grafic"),
              wellPanel(
                plotlyOutput("agro_timeseries_det_plot") |> withSpinner(size = 0.5),
                #downloadLink('down_plot_regio_ind', label = 'Download  PNG')
              )
            ), 
            tabPanel(
              value = "Data",
              title = h6("Data"),
              wellPanel(
                DT::dataTableOutput("agro_timeseries_det_data")
              )
            )
          )
          
        )
      )
    )
  )
)