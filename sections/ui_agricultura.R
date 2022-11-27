ui_agricultura <- tabPanel(
  title = "Agricultura",
  tabsetPanel(
    tabPanel(
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
              `Abatere` = "abate"
            ),
            selected = "absol"
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
              "slider_agro_abate", label = "Interval calcul", min = 2006, 
              max = 2100, value = c(2021, 2050), dragRange = T, ticks = F,
              sep = "", step = 1)
          ),
          conditionalPanel(
            condition = "input.agr_tip == 'absol'",
            sliderInput(
              "slider_agro_absol", label = "Interval calcul", min = 1971, 
              max = 2100, value = c(1971, 2000), dragRange = T, ticks = F,
              sep = "", step = 1)
          ),
          
          actionButton("go_agrgen", "Actualizare harta", icon("sync"))
        ),
        column(
          width = 7,
          textOutput("test"),
          leafletOutput(
            "agr_map"
          )
        )
      )
    ),
    tabPanel(
      title = "Detalii",
      grid_container(
        layout = c(
          "area0 area1",
          ".     .    "
        ),
        row_sizes = c(
          "1fr",
          "1fr"
        ),
        col_sizes = c(
          "0.47fr",
          "1.53fr"
        ),
        gap_size = "10px",
        grid_card(
          area = "area0",
          selectInput(
            inputId = "mySelectInput",
            label = "Select Input",
            choices = list(
              `choice a` = "a",
              `choice b` = "b"
            )
          )
        ),
        grid_card(
          area = "area1",
          plotlyOutput(
            outputId = "plot",
            width = "100%",
            height = "400px"
          )
        )
      )
    )
  )
)