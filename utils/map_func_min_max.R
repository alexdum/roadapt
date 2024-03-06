

# functie pentru reclasificare raster/date uat minime maxime pnetru simbolizare coerenta
map_func_min_max <- function(indic = NA, ind_tip = NA, perio_tip = NA) {
  # culori interpolate
  if (indic %in% c("tasAdjust", "tasmaxAdjust", "tasminAdjust")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- NA
      } else if  (perio_tip == "season") {
        min_max <- NA
      } else {
        min_max <- NA
      }
    }  else {
      min_max <- NA
    }
  }
  
  if (indic %in% c("rsds")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- NA
      } else if  (perio_tip == "season") {
        min_max <- NA
      } else {
        min_max <- NA
      }
    }  else {
      min_max <- NA
      
    }
  }
  
  if (indic %in% c("hurs")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- c(55,85)
      } else if  (perio_tip == "season") {
        min_max <- c(40,90)
      } else {
        min_max <-  c(35,90)
      }
    }  else {
      min_max <- c(-50,50)
      
    }
  }
  
  if (indic %in% c("prAdjust")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- c(200, 1400)
      } else if  (perio_tip == "season") {
        min_max <- c(20, 400)
      } else {
        min_max <- c(10, 200)
      }
    } else {
      min_max <- c(-50, 50)
    }
  }
  
  if (indic %in% c("wsgsmax")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- c(6,14)
      } else if (perio_tip == "season") {
        min_max <- c(5,16)
      } else {
        min_max <- c(4,15)
      }
    } else {
      min_max <- c(-5,5)
    }
  }
  
  if (indic %in% c("sfcwind")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- c(1,8)
      } else if (perio_tip == "season") {
        min_max <- c(1,8)
      } else {
        min_max <- c(1,8)
      }
    } else {
      min_max <- c(-5,5)
    }
  }
  
  if (indic %in% c("cdd")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- NA
      }
    } else {
      min_max <- NA
    }
  }
  
  if (indic %in% c("scorchno")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- NA
      }
      
    } else {
      min_max <- NA
    }
  }
  
  if (indic %in% c("gsl")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- NA
      }
    } else {
      min_max <- NA
    }
  }
  
  if (indic %in% c("hddheat15.5")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- NA
      }
    } else {
      min_max <- NA
    }
  }
  
  if (indic %in% c("cddcold22")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- NA
      }
    } else {
      min_max <- NA
    }
  }
  
  if (indic %in% c("r20mm")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- c(0,10)
      }
    } else {
      min_max <- c(-5,5) # pentru plotare cu reclasificare raster valori maxime izolate
    }
  }
  
  if (indic %in% c("hwd")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- c(0,50) # pentru plotare cu reclasificare raster valori maxime izolate
      }
    } else {
      min_max <- c(-3,40) # pentru plotare cu reclasificare raster valori maxime izolate
    }
  }
  
  if (indic %in% c("cwd")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- c(1,15) # pentru plotare cu reclasificare raster valori maxime izolate
      }
    } else {
      min_max <- c(-5,5) # pentru plotare cu reclasificare raster valori maxime izolate
    }
  }
  
  if (indic %in% c("wsdi")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- c(0,100) # pentru plotare cu reclasificare raster valori maxime izolate
      }
    } else {
      min_max <- c(-5,70) # pentru plotare cu reclasificare raster valori maxime izolate
    }
  }
  
  if (indic %in% c("csdi")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- c(0,10) # pentru plotare cu reclasificare raster valori maxime izolate
      }
    } else {
      min_max <- c(-5,5) # pentru plotare cu reclasificare raster valori maxime izolate
    }
  }
  
  if (indic %in% c("rx1day")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- c(0,45) # pentru plotare cu reclasificare raster valori maxime izolate
      }
    } else {
      min_max <- c(-50,50) # pentru plotare cu reclasificare raster valori maxime izolate
    }
  }
  
  if (indic %in% c("txge35")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- c(0,30) # pentru plotare cu reclasificare raster valori maxime izolate
      }
    } else {
      min_max <- c(-6,25) # pentru plotare cu reclasificare raster valori maxime izolate
    }
  }
  
  if (indic %in% c("tr")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- c(0,50) # pentru plotare cu reclasificare raster valori maxime izolate
      }
    } else {
      min_max <- c(-6,50) # pentru plotare cu reclasificare raster valori maxime izolate
    }
  }
  
  if (indic %in% c("fd")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- c(0,150) # pentru plotare cu reclasificare raster valori maxime izolate
      }
    } else {
      min_max <- c(-75,25) # pentru plotare cu reclasificare raster valori maxime izolate
    }
  }
  
  if (indic %in% c("hwf")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- c(0,90) # pentru plotare cu reclasificare raster valori maxime izolate
      }
    } else {
      min_max <- c(-5,100) # pentru plotare cu reclasificare raster valori maxime izolate
    }
  }
  
  if (indic %in% c("txge30")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- c(0,120) # pentru plotare cu reclasificare raster valori maxime izolate
      }
    } else {
      min_max <- c(-5,55) # pentru plotare cu reclasificare raster valori maxime izolate
    }
  }
  
  if (indic %in% c("r99p")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- c(0,100)
      }
    } else {
      min_max <- c(-50,50) # pentru plotare cu reclasificare raster valori maxime izolate
    }
  }
  
  if (indic %in% c("tx90p")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- c(0,35) # pentru plotare cu reclasificare raster valori maxime izolate
      }
    } else {
      min_max <- c(-5,35) # pentru plotare cu reclasificare raster valori maxime izolate
    }
  }
  
  if (indic %in% c("tmm15")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- c(160,360) # pentru plotare cu reclasificare raster valori maxime izolate
      }
    } else {
      min_max <- c(-80,10) # pentru plotare cu reclasificare raster valori maxime izolate
    }
  }
  
  if (indic %in% c("tmm22")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- c(0,100) # pentru plotare cu reclasificare raster valori maxime izolate
      }
    } else {
      min_max <- c(-20,60) # pentru plotare cu reclasificare raster valori maxime izolate
    }
  }
  
  if (indic %in% c("snd1cm", "snd5cm")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- c(0,190) # pentru plotare cu reclasificare raster valori maxime izolate
      }
    } else {
      min_max <- c(-100,70) # pentru plotare cu reclasificare raster valori maxime izolate
    }
  }
  if (indic %in% c("snd30cm")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- c(0,185) # pentru plotare cu reclasificare raster valori maxime izolate
      }
    } else {
      min_max <- c(-160,70) # pentru plotare cu reclasificare raster valori maxime izolate
    }
  }
  
  if (indic %in% c("sndmean")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- c(0,135)
      } else if  (perio_tip == "season") {
        min_max <- c(0,135)
      } else {
        min_max <- c(0,135)
      }
    }  else {
      min_max <-c(-150,10)
      
    }
  }
  
  if (indic %in% c("sndmax")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- c(0,230)
      } else if  (perio_tip == "season") {
        min_max <-  c(0,230)
      } else {
        min_max <- c(0,230)
      }
    }  else {
      min_max <-c(-150,10)
      
    }
  }
  
  
  return(min_max)
}

# indicators_def <- function(indicators) {
#   switch (
#     which(c("heatuspring","heatufall","scorchno","scorchu", "coldu","frostu10", "frostu15","frostu20","prveget", "prfall", "prwinter" ) %in%  indicators),
#     
#     text.desc <- "Cumulative heat units (ΣTmed. > 0°C) in the period 01 February - 10 April",
#     text.desc <- "Cumulative heat units (ΣTmed. > 0°C) in the period 01 September - 31 October",
#     text.desc <- "Scorching heat units (ΣTmax. ≥ 32°C) from 1 June to 31 August",
#     text.desc <- "Scorching heat number of days (Tmax. ≥ 32°C) from 1 June to 31 August",
#     text.desc <- "Cold units (ΣTmed. < 0°C) cumulated during the period 01 November - 31 March",
#     text.desc <- "Frost units (ΣTmin. ≤ -10°C) cumulated in the period 01 December - 28/29 February",
#     text.desc <- "Frost units (ΣTmin. ≤ -15°C) cumulated in the period 01 December - 28/29 February",
#     text.desc <- "Frost units (ΣTmin. ≤ -20°C) cumulated in the period 01 December - 28/29 February",
#     text.desc <- "Precipitatin amounts (l/m²) during the autumn wheat growing season, 01 September to 30 June",
#     text.desc <- "Precipitation amounts (l/m²) during the autumn sowing period, 01 September - 31 October",
#     text.desc <- "Precipitation amounts (l/m²) during the soil water accumulation period, 01 November - 31 March",
#   )
#   return(text.desc)
#   
# }

