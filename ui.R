library(plotly)
library(shiny)
library(gridlayout)

source("sections/ui_climatgeneral.R",  local = TRUE)
source("sections/ui_agricultura.R",  local = TRUE)
source("sections/ui_energie.R",  local = TRUE)
source("sections/ui_hidro.R",  local = TRUE)
source("sections/ui_silvicultura.R",  local = TRUE)
source("sections/ui_biodivers.R",  local = TRUE)
source("sections/ui_cultura.R",  local = TRUE)
source("sections/ui_urban.R",  local = TRUE)
source("sections/ui_transport.R",  local = TRUE)
source("sections/ui_turism.R",  local = TRUE)
source("sections/ui_sanatate.R",  local = TRUE)
source("sections/ui_industrie.R",  local = TRUE)
source("sections/ui_asigurari.R",  local = TRUE)
source("sections/ui_despre.R",  local = TRUE)

ui <- shinyUI(
  tagList(  # ascunde titlu navbar
    tags$head(
      tags$style(
        type = 'text/css','.navbar-brand{display:none;}',
        "body { padding-top: 120px;}"
      )
    ),
    useShinyjs(),
    navbarPage(
      
      title = "RO-Adapt explorer",
      id = "tabs",
      selected = "#general",
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
      ui_biodivers,
      ui_cultura,
      ui_urban,
      ui_transport,
      ui_turism,
      ui_sanatate,
      ui_industrie,
      ui_asigurari,
      ui_despre
      
    )
  )
)
