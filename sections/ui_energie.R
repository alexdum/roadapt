ui_energie <- tabPanel(
  title = "Energie",icon = icon("solar-panel"),
  value = "#energie", id = "#energie",
  h5("Energia"),
  tabsetPanel(
    tabPanel(title = "Național"),
    tabPanel(title = "Administrativ")
  )
)