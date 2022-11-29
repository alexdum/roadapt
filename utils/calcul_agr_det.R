#subseteaza dup
calcul_agro_det <- function(agr_tip_det, perio_sub, an1_abat, an2_abat, an1_abs, an2_abs) {
  #subseteaza dupa tip/an
  if (agr_tip_det == "abate") {
    an1 <-  an1_abat
    an2 <-  an2_abat
    
    if (perio_sub != "year") { #daca ai an formateaza data diferit
      tab.sub <- tab |> 
        #filter( month ==  as.integer(perio_sub)) |>
        mutate(norm = mean(p50[year >= 1971 & year <= 2000])) |> 
        filter(year >= an1 & year <= an2) |>
        group_by(ID) |> summarise(p50 = mean(p50) , norm = mean(norm)) |>
        mutate(value = p50 - norm)
    } else {
      tab.sub <- tab |> 
        filter(year >= an1 & year <= an1 ) |>
        group_by(ID) |> summarise(value = mean(p50)) 
    }
  } else {
    an1 <- an1_abs
    an2 <- an2_abs
    
    if (perio_sub != "year") { #daca ai an formateaza data diferit
      tab.sub <- tab |> 
        filter(month ==  as.integer(perio_sub) & year >= an1 & year <= an2) |>
        group_by(ID) |> summarise(value = mean(p50)) 
    } else {
      tab.sub <- tab |> 
        filter(year >= an1 & year <= an2) |>
        group_by(ID) |> summarise(value = mean(p50)) 
    }
  }
  return(tab.sub)
}
