#subseteaza dup
calcul_det <- function(tab, climgen_tip_det, perio_sub, indic, an1_abat, an2_abat, an1_abs, an2_abs) {
  #subseteaza dupa tip/an
  if (climgen_tip_det == "abate") {
    
    
    if (perio_sub != "year") { #daca ai an formateaza data diferit
      tab.sub <- tab |> 
        filter( month ==  as.integer(perio_sub)) |>
        group_by(ID) |>
        mutate(norm = mean(p50[year >= 1971 & year <= 2000])) |> 
        filter(year >= an1_abat & year <= an2_abat) |>
        group_by(ID) |> summarise(p50 = mean(p50) , norm = mean(norm)) |>
        mutate(
          value = case_when(
            indic %in% c("prAdjust", "hurs") ~  (((p50*100)/norm) - 100) |> round(1),
            !indic %in% c("prAdjust", "hurs") ~  (p50 - norm) |> round(1)
          )
        )
      
    } else {
      tab.sub <- tab |> 
        group_by(ID) |>
        mutate(norm = mean(p50[year >= 1971 & year <= 2000])) |> 
        filter(year >= an1_abat & year <= an2_abat) |>
        group_by(ID) |> 
        summarise(p50 = mean(p50) , norm = mean(norm)) |>
        mutate(
          value = case_when(
            indic %in% c("prAdjust", "hurs") ~  (((p50*100)/norm) - 100) |> round(1),
            !indic %in% c("prAdjust", "hurs") ~ (p50 - norm)  |> round(1)
          )
        )
    }
  } else {
    
    if (perio_sub != "year") { #daca ai an formateaza data diferit
      tab.sub <- tab |> 
        filter(month ==  as.integer(perio_sub) & year >= an1_abs & year <= an2_abs) |>
        group_by(ID) |> summarise(value = mean(p50) |> round(1)) 
    } else {
      tab.sub <- tab |> 
        filter(year >= an1_abs & year <= an2_abs) |>
        group_by(ID) |> summarise(value = mean(p50) |> round(1)) 
    }
  }
  return(tab.sub)
}
