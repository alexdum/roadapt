# 
# # 
# tab <- read_parquet("www/data/parquet/agro/prAdjust_rcp45_month-50_19710101_21001231.parquet")
# id <- "123371"
# perio_sub <- "01"
# indic <- "pr"
# dd <- extract_timeser_det(tab,id, perio_sub, indic)
# plots_agro_det(dd, tip = "abate", indic = "pr")

extract_timeser_det <- function(tab,id, perio_sub, indic) {
  
  
  tab_sub <- tab |> dplyr::filter(ID == id)
  
  if (perio_sub != "year")  tab_sub <- tab_sub |> dplyr::filter(month == as.integer(perio_sub))
  
  dd <- tab_sub |> collect() |> mutate(
    an = format(date, "%Y"),
    change_med = if (indic %in% c("prAdjust", "hurs","rx1day")) { (((p50*100)/mean(p50[an <= 2000])) - 100)  %>% round(1) } else { (p50 - mean(p50[an <= 2000]))  %>% round(1) },
    change_max = if (indic %in% c("prAdjust", "hurs","rx1day")) { (((pmax*100)/mean(p50[an <= 2000])) - 100)  %>% round(1) } else { (pmax - mean(p50[an <= 2000]))  %>% round(1) },
    change_min = if (indic %in% c("prAdjust", "hurs","rx1day")) { (((pmin*100)/mean(p50[an <= 2000])) - 100)  %>% round(1) } else { (pmin - mean(p50[an <= 2000]))  %>% round(1) },
    med_1971_2000 = mean(p50[an <= 2000]) |> round(1)
  )
  dd <- dd |> # tidy table
    dplyr::select(-year, -month) |>
    dplyr::rename(max = pmax, min = pmin, med = p50)
    
  return(dd)
}



