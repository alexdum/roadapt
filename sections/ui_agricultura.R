tabPanel(
  title = "Agricultura",
  tabsetPanel(
    tabPanel(
      title = "General",
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
          "0.3fr",
          "1.5fr"
        ),
        gap_size = "10px",
        grid_card(
          area = "area0",
          selectInput(
            inputId = "agr_param",
            label = "Parametru",
            choices = list(
              `Temperatura medie` = "tmed",
              `Temperatura maxima` = "tmax",
              `Temperatura minimă` = "tmin"
              
            )
          ),
          selectInput(
            inputId = "agr_scen",
            label = "Scenariu",
            choices = list(
              `RCP4.5` = "rcp45",
              `RCP8.5` = "rcp85"
            )
          )
        ),
        grid_card(
          area = "area1",
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