# culori culori leaflet indicatori agro---------------------------------------------------------
colintYlOrRd <- colorRampPalette( brewer.pal(9,"YlOrRd"),interpolate="linear")
colintRdYlBu <- colorRampPalette(brewer.pal(10,"RdYlBu"),interpolate="linear")
colintBrBG <- colorRampPalette( brewer.pal(11,"BrBG")[1:5],interpolate="linear")
colintBlues <- colorRampPalette(brewer.pal(9,"Blues"), interpolate="linear")
colintBuPu <- colorRampPalette(brewer.pal(9,"BuPu"), interpolate="linear")
colintPuRd <- colorRampPalette(brewer.pal(9,"PuRd"), interpolate="linear")
colintYlOrBr <- colorRampPalette(brewer.pal(9,"YlOrBr"), interpolate="linear")
colintinferno <- colorRampPalette(rev(viridis::inferno(14)), interpolate="linear")
colintGnBu <- colorRampPalette(brewer.pal(9,"GnBu"), interpolate="linear")
colintRdPu <- colorRampPalette(brewer.pal(9,"RdPu"), interpolate="linear")
colintBrBGfull <- colorRampPalette( brewer.pal(11,"BrBG"),interpolate="linear")

map_func_cols <- function (indic = NA, agr_tip = NA, perio_tip = NA, domain = NA) {
  # culori interpolate
  
  if (indic %in% c("tas", "tasmax", "tasmin")) {
    if(agr_tip == 'absol') {
      df.col <- data.frame(
        cols = c(rev(colintBlues(8)), colintYlOrRd(19)), 
        vals = seq(-16,36,2)
      )
      leaflet_titleg <- "°C"
    } else {
      df.col <- data.frame(
        cols = rev(colintRdYlBu(25)), 
        vals = seq(-6,6, 0.5)
      )
      leaflet_titleg <- "°C"
    }
  }
  

  print(head(df.col))
  print(domain)
  ints <- findInterval(domain, df.col$vals, rightmost.closed = T, left.open = F)

  bins <-  df.col$vals[ints[1]:(ints[2] + 1)]
  cols <- df.col$cols[ints[1]:(ints[2])]
  
  print(bins)
  print(cols)
  
  pal <- colorBin(cols, domain = domain, bins = bins, na.color = "transparent")
  pal2 <- colorBin(cols, domain = domain, bins = bins, reverse = T, na.color = "transparent")
  
  return(list(pal = pal, pal_rev = pal2, tit_leg = leaflet_titleg))
  
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

