#subseteaza dup
#nc_fil <- paste0("www/data/ncs/agro/tasAdjust_rcp45_season-25_19710101_21001231.nc")
calcul_gen <- function(nc_fil, climgen_tip, perio_sub, indic, an1_abat, an2_abat, an1_abs, an2_abs) {
  
  nc <- rast(nc_fil)
  template <- nc[[1]]
  
  months_list <- list()
  if (perio_sub != "year") { 
    months_list <- as.integer(perio_sub) 
  }
  
  # selectare slider in functie de tipul hartii
  if (climgen_tip == "abate") {
    
    date1_sub <- paste0(an1_abat, "-01-01")
    date2_sub <- paste0(an2_abat, "-12-31")
    
    date1_norm <- "1971-01-01"
    date2_norm <- "2000-12-31"
    
    arr_abs <- calc_mean_xarray(nc_fil, indic, date1_sub, date2_sub, months_list)
    arr_norm <- calc_mean_xarray(nc_fil, indic, date1_norm, date2_norm, months_list)
    
    nc.abs <- template
    values(nc.abs) <- as.vector(t(arr_abs))
    
    nc.norm <- template
    values(nc.norm) <- as.vector(t(arr_norm))
    
    # calcul abatere in functie de parametru
    if (indic %in% c("prAdjust", "hurs")) {
      ncf <- (((nc.abs*100)/nc.norm ) - 100) 
    } else {
      ncf <- nc.abs - nc.norm 
    }
    
  } else {
    
    date1_abs <- paste0(an1_abs, "-01-01")
    date2_abs <- paste0(an2_abs, "-12-31")
    
    arr_abs <- calc_mean_xarray(nc_fil, indic, date1_abs, date2_abs, months_list)
    
    ncf <- template
    values(ncf) <- as.vector(t(arr_abs))
  }
  
  return(ncf) 
  
}

