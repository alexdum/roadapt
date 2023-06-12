ui_industrie <- tabPanel(
  title = "Industrie",icon = icon("industry"),
  value = "#industrie", id = "#industrie",
  h5("Industrie"),
  tabsetPanel(
    tabPanel(
      id = "tab_industrie_gen",
      title = h6("Național"),
      fluidRow(
        column(
          width = 2,
          selectInput(
            inputId = "industrie_ind",
            label = "Indicator",
            choices = select_industrie_ind,
            selected =  select_industrie_ind[1]
          ),
          selectInput(
            inputId = "industrie_tip",
            label = "Tip",
            choices = list(
              `Valori absolute` = "absol",
              `Schimbare` = "abate"
            ),
            selected = "abate"
          ),
          selectInput(
            inputId = "industrie_scen",
            label = "Scenariu",
            choices = list(
              `RCP4.5` = "rcp45",
              `RCP8.5` = "rcp85"
            ), 
            selected = " rcp45"
          ),
          selectInput(
            inputId = "industrie_perio",
            label = "Luna/Sezon",
            choices = select_interv,
            selected =  select_interv[1]
          ),
          conditionalPanel(
            condition = "input.industrie_tip == 'abate'",
            sliderInput(
              "slider_industrie_abate_gen", label = "Interval calcul", min = 2006, 
              max = 2100, value = c(2071, 2100), dragRange = T, ticks = F,
              sep = "", step = 1)
          ),
          conditionalPanel(
            condition = "input.industrie_tip == 'absol'",
            sliderInput(
              "slider_industrie_absol_gen", label = "Interval calcul", min = 1971, 
              max = 2100, value = c(1971, 2000), dragRange = T, ticks = F,
              sep = "", step = 1)
          ),
          sliderInput(
            "transp_industrie_gen", "Transparență raster",
            min = 0, max = 1, ticks = F,
            value = 0.8, step = 0.1,
          ),
          actionButton("go_industriegen", "Actualizare harta", icon("sync")),
          radioButtons( # radio button show values
            "radio_industrie_gen", label = "Harta click funcționalitate",
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
          uiOutput(outputId = "industrie_text_gen"),
          #h6(textOutput("industrie_text_gen"), style = "text-align:center;"),
          wellPanel(
            leafletOutput("industrie_map_gen") |> withSpinner(size = 0.5)
          ),
          br(),
          conditionalPanel( # show graphs only when data available
            condition = "input.radio_industrie_gen == 2 && output.condpan_industrie_gen != 'nas'",
            
            tabsetPanel(
              fluidRow (
                h6(textOutput("condpan_industrie_gen"), style = "text-align:center;")
              ),
              tabPanel(
                value = "Grafic",
                title = h6("Grafic"),
                wellPanel(
                  plotlyOutput("industrie_timeseries_gen_plot") |> withSpinner(size = 0.5),
                  #downloadLink('down_plot_regio_ind', label = 'Download  PNG')
                )
              ), 
              tabPanel(
                value = "Data",
                title = h6("Data"),
                wellPanel(
                  DT::dataTableOutput("industrie_timeseries_gen_data")
                )
              )
            ),
          ),
          conditionalPanel(
            condition = "input.radio_industrie_gen == 2 && output.condpan_industrie_gen == 'nas'",
            wellPanel(
              p("Trebuie selectată o regiune cu valori disponibile")
            )
          )
        )
      )
    ),
    tabPanel(
      id = "tab_industrie_det",
      title = h6("Administrativ"),
      fluidRow(
        column(
          width = 2,
          selectInput(
            inputId = "industrie_admin_det",
            label = "Unitate administrativă",
            choices = c("Regiuni dezvoltare" = "reg",
                        "Județe" = "jud",
                        "UAT" = "uat"),
            selected =  "jud"
          ),
          selectInput(
            inputId = "industrie_ind_det",
            label = "Indicator",
            choices = select_industrie_ind,
            selected =  select_industrie_ind[1]
          ),
          selectInput(
            inputId = "industrie_tip_det",
            label = "Tip",
            choices = list(
              `Valori absolute` = "absol",
              `Schimbare` = "abate"
            ),
            selected = "abate"
          ),
          selectInput(
            inputId = "industrie_scen_det",
            label = "Scenariu",
            choices = list(
              `RCP4.5` = "rcp45",
              `RCP8.5` = "rcp85"
            ), 
            selected = " rcp45"
          ),
          selectInput(
            inputId = "industrie_perio_det",
            label = "Luna/Sezon",
            choices = select_interv, 
            selected =  select_interv[1]
          ),
          conditionalPanel(
            condition = "input.industrie_tip_det == 'abate'",
            sliderInput(
              "slider_industrie_abate_det", label = "Interval calcul", min = 2006, 
              max = 2100, value = c(2071, 2100), dragRange = T, ticks = F,
              sep = "", step = 1)
          ),
          conditionalPanel(
            condition = "input.industrie_tip_det== 'absol'",
            sliderInput(
              "slider_industrie_absol_det", label = "Interval calcul", min = 1971, 
              max = 2100, value = c(1971, 2000), dragRange = T, ticks = F,
              sep = "", step = 1)
          ),
          sliderInput(
            "transp_industrie_det", "Transparență poligon",
            min = 0, max = 1, ticks = F,
            value = 0.7, step = 0.1,
          ),
          actionButton("go_industriedet", "Actualizare harta", icon("sync"))
        ),
        column(
          width = 7,
          uiOutput(outputId = "industrie_text_det"),
          #h6(textOutput("industrie_text_det"), style = "text-align:center;"),
          wellPanel(
            leafletOutput("industrie_map_det") |> withSpinner(size = 0.5)
          ),
          br(),
          tabsetPanel(
            fluidRow (
              h6(textOutput("condpan_industrie_det"), style = "text-align:center;"),
              htmlOutput("industrie_det_stat")
            ),
            tabPanel(
              value = "Grafic",
              title = h6("Grafic"),
              wellPanel(
                plotlyOutput("industrie_timeseries_det_plot") |> withSpinner(size = 0.5),
                #downloadLink('down_plot_regio_ind', label = 'Download  PNG')
              )
            ), 
            tabPanel(
              value = "Data",
              title = h6("Data"),
              wellPanel(
                DT::dataTableOutput("industrie_timeseries_det_data")
              )
            )
          )
        )
      )
    )
  )
)