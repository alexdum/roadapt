


# tip <- "schimbare"
# 
# plots_agro_gen(dd, "schimbare")

plots_gen <- function(dd, tip, indic) {
  
  # pentru titlu axa y
  if (indic %in% c("tasAdjust" , "tasmaxAdjust", "tasminAdjust")) {
    y_lab <- "°C"
  } else if (indic %in% c("prAdjust")) {
    y_lab = "mm"
  } else {
    y_lab = "zile"
  }
  
  if (tip == "abate") {
    
    # schimba in procente pentru anomalii
    if (indic %in% c("pr")) y_lab <- "%"
    
    gg <- ggplot(data = dd, aes(x = data, y = change_med)) + 
      geom_line(color = "black", size = 0.8) +
      geom_ribbon(aes(x = data, ymax = change_max, ymin = change_min), alpha = 0.5, fill = "gray") +
      scale_x_date(breaks = c(as.Date("1971-01-01"), seq(as.Date("2000-01-01"), as.Date("2100-12-31"), by = "20 years")), date_labels = "%Y") +
      xlab("") + ylab(y_lab)
    
    
  } else {
    
    
    gg <- ggplot(data = dd, aes(x = data, y = med)) + 
      geom_line(color = "black", size = 0.8) +
      geom_line(aes(x = data,  y = med_1971_2000), col = "red") +
      geom_ribbon(aes(x = data, ymax = max, ymin = min), alpha = 0.5, fill = "gray") +
      scale_x_date(breaks = c(as.Date("1971-01-01"), seq(as.Date("2000-01-01"), as.Date("2100-12-31"), by = "20 years")), date_labels = "%Y") +
      xlab("") + ylab(y_lab)
  }
  
  gp <-  plotly::ggplotly(gg, dynamicTicks = F)  |>
    plotly::layout(
      autosize=T,
      hovermode = "x") |> 
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "lasso2d", "zoom", "toggleSpikelines", "zoom", "select2d",
                                 "hoverCompareCartesian", "hoverClosestCartesian","autoScale2d"),
      displayModeBar = T
    )
  
  return (list(gg = gg, gp = gp))
  
  
}


