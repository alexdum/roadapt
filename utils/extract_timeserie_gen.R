# 
# 
# nc_fil <- "www/data/ncs/agro/prAdjust_rcp45_season-50_19710101_21001231.nc"
# xy <- c(25, 45)
# 
# extract_timeser_gen(nc_fil, xy, perio_sub = "year")

extract_timeser_gen <- function(nc_fil,xy, perio_sub) {
  
  varia <- strsplit(nc_fil, "/|_")[[1]][5] # extrage nume variabila
  
  dd_50 <- extract_point(fname = nc_fil, lon = xy[1], lat = xy[2], variable = varia) 
  dd_max <-extract_point(fname = gsub("-50", "-max", nc_fil), lon = xy[1], lat = xy[2], variable = varia)
  dd_min <-extract_point(fname = gsub("-50", "-min", nc_fil), lon = xy[1], lat = xy[2], variable = varia)
  
  dd <- dd_50 |> left_join(dd_max, by = c("time")) |> left_join(dd_min, by = c("time")) |>
    dplyr::rename('med' = 'value.x', 'max' = 'value.y', 'min' = 'value')  |>
    mutate(max = round(max, 1), min = round(min, 1), med = round(med, 1)) 
  
  if (perio_sub != "year")  dd <- dd |> dplyr::filter(format(time, "%m") %in% perio_sub)
  
  dd <- dd |> mutate(
    an = format(time, "%Y"),
    change_med = case_when(
      varia %in% c("prAdjust", "ur") ~  (((med*100)/mean(med[an <= 2000])) - 100)  %>% round(1),
      !varia %in% c("prAdjust", "ur") ~   (med - mean(med[an <= 2000]))  %>% round(1)),
    change_max = case_when(
      varia %in% c("prAdjust", "ur") ~  (((max*100)/mean(med[an <= 2000])) - 100)  %>% round(1),
      !varia %in% c("prAdjust", "ur") ~   (max - mean(med[an <= 2000]))  %>% round(1)),
    change_min = case_when(
      varia %in% c("prAdjust", "ur") ~  (((min*100)/mean(med[an <= 2000])) - 100)  %>% round(1),
      !varia %in% c("prAdjust", "ur") ~   (min - mean(med[an <= 2000]))  %>% round(1))
  )
  
  return(dd)
}



