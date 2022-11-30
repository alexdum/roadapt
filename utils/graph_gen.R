library(ggplot2)

variable <- "prAdjust"
nc_fil <- "www/data/ncs/agro/prAdjust_rcp45_season-50_19710101_21001231.nc"
xy <- c(25, 45)


plots_agro_gen <- funtion(nc_fil, varia,  xy) {
  
  variable <- strsplit(nc_fil, "/|_")[[1]][5] 
  
  dd_50 <- extract_point(fname = nc_fil, lon = xy[1], lat = xy[2], variable = variable) 
  dd_max <-extract_point(fname = gsub("-50", "-max", nc_fil), lon = xy[1], lat = xy[2], variable = variable)
  dd_min <-extract_point(fname = gsub("-50", "-min", nc_fil), lon = xy[1], lat = xy[2], variable = variable)


  
  dd <- dd_50 |> left_join(dd_max, by = c("time")) |> left_join(dd_min, by = c("time")) |>
    dplyr::rename('med' = 'value.x', 'max' = 'value.y', 'min' = 'value')  |>
    mutate(max = round(max, 1), min = round(min, 1), med = round(med, 1)) |>
    dplyr::filter(format(time, "%m") %in% "10") |>
    mutate(
      an = format(time, "%Y"),
      change_med = case_when(
        varia %in% c("pr", "ur") ~  (((med*100)/mean(med[an <= 2000])) - 100)  %>% round(1),
        !varia %in% c("pr", "ur") ~   (med - mean(med[an <= 2000]))  %>% round(1)),
      change_max = case_when(
        varia %in% c("pr", "ur") ~  (((max*100)/mean(med[an <= 2000])) - 100)  %>% round(1),
        !varia %in% c("pr", "ur") ~   (max - mean(med[an <= 2000]))  %>% round(1)),
      change_min = case_when(
        varia %in% c("pr", "ur") ~  (((min*100)/mean(med[an <= 2000])) - 100)  %>% round(1),
        !varia %in% c("pr", "ur") ~   (min - mean(med[an <= 2000]))  %>% round(1))
    )
  
  
  return(dd)
}

  if(tip == schimbare) {
  
 gg <- ggplot(data = dd, aes(x = as.Date(time), y = change_med)) + 
    geom_line(color = "black", size = 0.8) +
    geom_ribbon(aes(x = as.Date(time), ymax = change_max, ymin = change_min), alpha = 0.5, fill = "gray") +
    scale_x_date(breaks = c(as.Date("1971-01-01"), seq(as.Date("2000-01-01"), as.Date("2100-12-31"), by = "20 years")), date_labels = "%Y") +
    xlab("")
  

  } else {
  
  
  ff <- ggplot(data = dd, aes(x = as.Date(time), y = med)) + 
    geom_line(color = "black", size = 0.8) +
    geom_line(aes(x = as.Date(time),  y = mean(med[format(time, "%Y") <= 2000])), col = "red") +
    geom_ribbon(aes(x = as.Date(time), ymax = max, ymin = min), alpha = 0.5, fill = "gray") +
    scale_x_date(breaks = c(as.Date("1971-01-01"), seq(as.Date("2000-01-01"), as.Date("2100-12-31"), by = "20 years")), date_labels = "%Y") +
    xlab("")
  }
 return (gg)
  
  
}


