#subseteaza dup
calcul_agro_det <- function(tab, agr_tip_det, perio_sub, an1_abat, an2_abat, an1_abs, an2_abs) {
  #subseteaza dupa tip/an
  if (agr_tip_det == "abate") {
 
    
    if (perio_sub != "year") { #daca ai an formateaza data diferit
      tab.sub <- tab |> 
        #filter( month ==  as.integer(perio_sub)) |>
        mutate(norm = mean(p50[year >= 1971 & year <= 2000])) |> 
        filter(year >= an1_abat & year <= an2_abat) |>
        group_by(ID) |> summarise(p50 = mean(p50) , norm = mean(norm)) |>
        mutate(value = p50 - norm)
    } else {
      tab.sub <- tab |> 
        filter(year >= an1_abat & year <= an2_abat ) |>
        group_by(ID) |> summarise(value = mean(p50)) 
    }
  } else {
 
    
    if (perio_sub != "year") { #daca ai an formateaza data diferit
      tab.sub <- tab |> 
        filter(month ==  as.integer(perio_sub) & year >= an1_abs & year <= an2_abs) |>
        group_by(ID) |> summarise(value = mean(p50)) 
    } else {
      tab.sub <- tab |> 
        filter(year >= an1_abs & year <= an2_abs) |>
        group_by(ID) |> summarise(value = mean(p50)) 
    }
  }
  return(tab.sub)
}
