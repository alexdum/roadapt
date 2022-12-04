#subseteaza dup
calcul_agro_gen <- function(nc_fil, agr_tip, perio_sub, indic, an1_abat, an2_abat, an1_abs, an2_abs) {
  
  nc <- rast(nc_fil)
  dats <- names_to_date(nc) # extrage data din nume cu fct utils
  
  # selectare slider in functie de tipul hartii
  if (agr_tip == "abate") {
    
    dats.sub <- dats[dats >= as.Date(paste0(an1_abat, "0101"), format = "%Y%m%d") & dats <= as.Date(paste0(an2_abat , "1231"), format = "%Y%m%d") ]
    dats.norm <- dats[dats >= as.Date("1971-01-01") & dats <= as.Date("2000-12-31")]
    
    if (perio_sub != "year") { #daca ai an formateaza data diferit
      dats.sub <- dats.sub[format(dats.sub, "%m") %in% perio_sub] # daca ai an nu subseta pe perioade
      dats.norm <- dats.norm[format(dats.norm, "%m") %in% perio_sub] # daca ai an nu subseta pe perioade
    }
    
    nc.norm <- nc[[which(dats %in% dats.norm)]] |> mean()
    nc.abs <- nc[[which(dats %in% dats.sub)]] |> mean()
    # calcul abatere in functie de parametru
    if (indic %in% c("pr", "ur")) {
      ncf <- (((nc.abs*100)/nc.norm ) - 100) 
    } else {
      ncf <- nc.abs - nc.norm 
    }
    
  } else {
    
    dats.sub <- dats[dats >= as.Date(paste0(an1_abs, "0101"), format = "%Y%m%d") & dats <= as.Date(paste0(an1_abs , "1231"), format = "%Y%m%d") ]
    
    if (perio_sub != "year") { #daca ai an formateaza data diferit
      dats.sub <- dats.sub[format(dats.sub, "%m") %in% perio_sub] # daca ai an nu subseta pe perioade
    }
    
    ncf <- nc[[which(dats %in% dats.sub)]] 
    ncf <- mean(ncf)
  }
  
  return(ncf) 
  
}

