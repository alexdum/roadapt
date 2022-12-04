# 
# # 
# tab <- read_parquet("www/data/parquet/agro/prAdjust_rcp45_month-50_19710101_21001231.parquet")
# id <- "123371"
# perio_sub <- "01"
# indic <- "pr"
# extract_timeser_gen(tab,id, perio_sub, indic)

extract_timeser_det <- function(tab,id, perio_sub, indic) {
  
  
  tab_sub <- tab |> dplyr::filter(ID == id)
  
  if (perio_sub != "year")  tab_sub <- tab_sub |> dplyr::filter(format(date, "%m") %in% perio_sub)
  
  dd <- tab_sub |> mutate(
    an = format(date, "%Y"),
    change_med = case_when(
      indic %in% c("pr", "ur") ~  (((p50*100)/mean(p50[an <= 2000])) - 100)  %>% round(1),
      !indic %in% c("pr", "ur") ~   (p50 - mean(p50[an <= 2000]))  %>% round(1)),
    change_max = case_when(
      indic %in% c("pr", "ur") ~  (((pmax*100)/mean(p50[an <= 2000])) - 100)  %>% round(1),
      !indic %in% c("pr", "ur") ~   (pmax - mean(p50[an <= 2000]))  %>% round(1)),
    change_min = case_when(
      indic %in% c("pr", "ur") ~  (((pmin*100)/mean(p50[an <= 2000])) - 100)  %>% round(1),
      !indic %in% c("pr", "ur") ~   (pmin - mean(p50[an <= 2000]))  %>% round(1)),
    med_ref = mean(p50[an <= 2000]) |> round(1)
  )
  
  return(dd)
}



