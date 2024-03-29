# When we change from one `tabPanel` to another, update the URL hash
observeEvent(input$tabs, {
  
  # menu collapse back menu after selection
  runjs('
      var elem = document.getElementsByClassName("navbar-collapse")[0]
      elem.setAttribute("aria-expanded", "false");
      elem.setAttribute("class", "navbar-collapse collapse");
    ')
  
  # No work to be done if input$tabs and the hash are already the same
  if (getUrlHash() == input$tabs) return()
  
  # The 'push' argument is necessary so that the hash change event occurs and
  # so that the other observer is triggered.
  updateQueryString(
    paste0(getQueryString(), input$tabs),
    "push"
  )
  # Don't run the first time so as to not generate a circular dependency 
  # between the two observers
}, ignoreInit = TRUE)

observeEvent(getUrlHash(), {
  hash <- getUrlHash()

  # No work to be done if input$tabs and the hash are already the same
  if (hash == input$tabs) return()

  valid <- c("#general","#agricultura","#energie","#hidro", "#silvicultura","#biodivers","#cultura","#urban","#transport","#turism","#sanatate", "#asigurari","#despre")

  if (hash %in% valid) {
    updateTabsetPanel(session, "tabs", hash)
  }



})