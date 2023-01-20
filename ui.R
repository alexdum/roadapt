library(plotly)
library(shiny)
library(gridlayout)

source("sections/ui_climatgeneral.R",  local = TRUE)
source("sections/ui_agricultura.R",  local = TRUE)
source("sections/ui_energie.R",  local = TRUE)
source("sections/ui_hidro.R",  local = TRUE)
source("sections/ui_silvicultura.R",  local = TRUE)
source("sections/ui_despre.R",  local = TRUE)

ui <- shinyUI(
  tagList(  # ascunde titlu navbar
    tags$head(
      tags$style(
        type = 'text/css','.navbar-brand{display:none;}',
        "body { padding-top: 70px;}"
      )
    ),
    useShinyjs(),
    navbarPage(
      
      title = "RO-Adapt explorer",
      id = "tabs",
      selected = "#despre",
      fluid = T,
      position = "fixed-top",
      collapsible = TRUE,
      theme = bslib::bs_theme(
        bootswatch = "minty", version = 5
        ),
      
      ui_climatgeneral,
      ui_agricultura,
      ui_energie,
      ui_hidro,
      ui_silvicultura,
      ui_despre
      
    )
  )
)
