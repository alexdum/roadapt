library(plotly)
library(shiny)
library(gridlayout)

navbarPage(
  title = "Chick Weights",
  selected = "Agricultura",
  collapsible = TRUE,
  theme = bslib::bs_theme(),
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
            "0.79fr",
            "1.21fr"
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
  ),
  tabPanel(
    title = "Energie",
    tabsetPanel(
      tabPanel(title = "General"),
      tabPanel(title = "Detalii")
    )
  ),
  tabPanel(title = "Despre")
)
