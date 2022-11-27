library(plotly)
library(shiny)
library(gridlayout)

source("sections/ui_agricultura.R",  local = TRUE)

navbarPage(
  title = "RO-Adapt explorer",
  selected = "Despre",
  collapsible = TRUE,
  theme = bslib::bs_theme(bootswatch = "minty"),
  
  ui_agricultura

 ,
  tabPanel(
    title = "Energie",
    tabsetPanel(
      tabPanel(title = "General"),
      tabPanel(title = "Detalii")
    )
  ),
  tabPanel(title = "Despre")
)
