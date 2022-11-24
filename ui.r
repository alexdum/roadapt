library(plotly)
library(shiny)
library(gridlayout)

navbarPage(
  title = "RO-Adapt explorer",
  selected = "Despre",
  collapsible = TRUE,
  theme = bslib::bs_theme(bootswatch = "minty"),
  
  source("sections/ui_agricultura.R",  local = TRUE)$value

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
