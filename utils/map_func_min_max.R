

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
  
  if (indic %in% c("wsgsmax")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- NA
      } else if  (perio_tip == "season") {
        min_max <- NA
      } else {
        min_max <- NA
      }
      min_max <- NA
    } else {
      min_max <- NA
    }
  }
  
  if (indic %in% c("prAdjust")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        min_max <- NA
      } else if (perio_tip == "season") {
        min_max <- NA
      } else {
        min_max <- NA
      }
    } else {
      min_max <- NA
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
        min_max <- NA
      }
    } else {
      min_max <- NA # pentru plotare cu reclasificare raster valori maxime izolate
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
