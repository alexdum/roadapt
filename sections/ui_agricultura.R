ui_agricultura <- tabPanel(
  title = "Agricultura",icon = icon("tractor"),
  value = "#agricultura", id = "#agricultura",
  h5("Agricultură și dezvoltare rurală"),
  tabsetPanel(
    tabPanel(
      id = "tab_agro_gen",
      title = h6("Național"),
      fluidRow(
        column(
          width = 2,
          selectInput(
            inputId = "agro_ind",
            label = "Indicator",
            choices = select_agro_ind,
            selected =  select_agro_ind[1]
          ),
          selectInput(
            inputId = "agro_tip",
            label = "Tip",
            choices = list(
              `Valori absolute` = "absol",
              `Schimbare` = "abate"
            ),
            selected = "abate"
          ),
          selectInput(
            inputId = "agro_scen",
            label = "Scenariu",
            choices = list(
              `RCP4.5` = "rcp45",
              `RCP8.5` = "rcp85"
            ), 
            selected = " rcp45"
          ),
          selectInput(
            inputId = "agro_perio",
            label = "Luna/Sezon",
            choices = select_interv[17],
            selected =  select_interv[17]
          ),
          conditionalPanel(
            condition = "input.agro_tip == 'abate'",
            sliderInput(
              "slider_agro_abate_gen", label = "Interval calcul", min = 2006, 
              max = 2100, value = c(2071, 2100), dragRange = T, ticks = F,
              sep = "", step = 1)
          ),
          conditionalPanel(
            condition = "input.agro_tip == 'absol'",
            sliderInput(
              "slider_agro_absol_gen", label = "Interval calcul", min = 1971, 
              max = 2100, value = c(1971, 2000), dragRange = T, ticks = F,
              sep = "", step = 1)
          ),
          sliderInput(
            "transp_agro_gen", "Transparență raster",
            min = 0, max = 1, ticks = F,
            value = 0.8, step = 0.1,
          ),
          actionButton("go_agrogen", "Actualizare harta", icon("sync")),
          radioButtons( # radio button show values
            "radio_agro_gen", label = "Harta click funcționalitate",
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
          h6(textOutput("agro_text_gen"), style = "text-align:center;"),
          wellPanel(
            leafletOutput("agro_map_gen")
          ),
          br(),
          conditionalPanel( # show graphs only when data available
            condition = "input.radio_agro_gen == 2 && output.condpan_agro_gen != 'nas'",
            
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
            condition = "input.radio_agro_gen == 2 && output.condpan_agro_gen == 'nas'",
            wellPanel(
              p("Trebuie selectată o regiune cu valori disponibile")
            )
          )
        )
      )
    ),
    tabPanel(
      id = "tab_agro_det",
      title = h6("Administrativ"),
      fluidRow(
        column(
          width = 2,
          selectInput(
            inputId = "agro_admin_det",
            label = "Unitate administrativă",
            choices = c("Regiuni dezvoltare" = "reg",
                        "Județe" = "jud",
                        "UAT" = "uat"),
            selected =  "uat"
          ),
          selectInput(
            inputId = "agro_ind_det",
            label = "Indicator",
            choices = select_agro_ind,
            selected =  select_agro_ind[1]
          ),
          selectInput(
            inputId = "agro_tip_det",
            label = "Tip",
            choices = list(
              `Valori absolute` = "absol",
              `Schimbare` = "abate"
            ),
            selected = "abate"
          ),
          selectInput(
            inputId = "agro_scen_det",
            label = "Scenariu",
            choices = list(
              `RCP4.5` = "rcp45",
              `RCP8.5` = "rcp85"
            ), 
            selected = " rcp45"
          ),
          selectInput(
            inputId = "agro_perio_det",
            label = "Luna/Sezon",
            choices = select_interv[17], # avem doar anuale
            selected =  select_interv[17]
          ),
          conditionalPanel(
            condition = "input.agro_tip_det == 'abate'",
            sliderInput(
              "slider_agro_abate_det", label = "Interval calcul", min = 2006, 
              max = 2100, value = c(2071, 2100), dragRange = T, ticks = F,
              sep = "", step = 1)
          ),
          conditionalPanel(
            condition = "input.agro_tip_det== 'absol'",
            sliderInput(
              "slider_agro_absol_det", label = "Interval calcul", min = 1971, 
              max = 2100, value = c(1971, 2000), dragRange = T, ticks = F,
              sep = "", step = 1)
          ),
          sliderInput(
            "transp_agro_det", "Transparență poligon",
            min = 0, max = 1, ticks = F,
            value = 0.7, step = 0.1,
          ),
          actionButton("go_agrodet", "Actualizare harta", icon("sync"))
        ),
        column(
          width = 7,
          h6(textOutput("agro_text_det"), style = "text-align:center;"),
          wellPanel(
            leafletOutput("agro_map_det") |> withSpinner(size = 0.5)
          ),
          br(),
          tabsetPanel(
            fluidRow (
              h6(textOutput("condpan_agro_det"), style = "text-align:center;"),
              htmlOutput("agro_det_stat")
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