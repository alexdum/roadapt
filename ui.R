library(plotly)
library(shiny)
library(gridlayout)

source("sections/ui_agricultura.R",  local = TRUE)
source("sections/ui_energie.R",  local = TRUE)
source("sections/ui_despre.R",  local = TRUE)


navbarPage(
  title = "RO-Adapt explorer",
  id = "tabs",
  selected = "#despre",
  fluid = T,
  collapsible = TRUE,
  theme = bslib::bs_theme(bootswatch = "minty"),
  
  ui_agricultura,
  ui_energie,
  ui_despre
)
