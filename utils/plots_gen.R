


# tip <- "schimbare"
# 
# plots_agro_gen(dd, "schimbare")

plots_agro_gen <- function(dd, tip) {
  


  if (tip == "abate") {
  
 gg <- ggplot(data = dd, aes(x = as.Date(time), y = change_med)) + 
    geom_line(color = "black", size = 0.8) +
    geom_ribbon(aes(x = as.Date(time), ymax = change_max, ymin = change_min), alpha = 0.5, fill = "gray") +
    scale_x_date(breaks = c(as.Date("1971-01-01"), seq(as.Date("2000-01-01"), as.Date("2100-12-31"), by = "20 years")), date_labels = "%Y") +
    xlab("")
  

  } else {
  
  
  gg <- ggplot(data = dd, aes(x = as.Date(time), y = med)) + 
    geom_line(color = "black", size = 0.8) +
    geom_line(aes(x = as.Date(time),  y = mean(med[format(time, "%Y") <= 2000])), col = "red") +
    geom_ribbon(aes(x = as.Date(time), ymax = max, ymin = min), alpha = 0.5, fill = "gray") +
    scale_x_date(breaks = c(as.Date("1971-01-01"), seq(as.Date("2000-01-01"), as.Date("2100-12-31"), by = "20 years")), date_labels = "%Y") +
    xlab("")
  }
 return (gg)
  
  
}


