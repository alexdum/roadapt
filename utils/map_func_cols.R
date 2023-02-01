# culori culori leaflet ---------------------------------------------------------
colintYlOrRd <- colorRampPalette( brewer.pal(9,"YlOrRd"),interpolate = "linear")
colintRdYlBu <- colorRampPalette(brewer.pal(10,"RdYlBu"),interpolate = "linear")
colintBrBG <- colorRampPalette( brewer.pal(11,"BrBG")[1:5],interpolate = "linear")
colintBlues <- colorRampPalette(brewer.pal(9,"Blues"), interpolate = "linear")
colintReds <- colorRampPalette(brewer.pal(9,"Reds"), interpolate = "linear")
colintBuPu <- colorRampPalette(brewer.pal(9,"BuPu"), interpolate = "linear")
colintPuRd <- colorRampPalette(brewer.pal(9,"PuRd"), interpolate = "linear")
colintYlOrBr <- colorRampPalette(brewer.pal(9,"YlOrBr"), interpolate = "linear")
colintinferno <- colorRampPalette(rev(viridis::inferno(14)), interpolate = "linear")
colintGnBu <- colorRampPalette(brewer.pal(9,"GnBu"), interpolate = "linear")
colintRdPu <- colorRampPalette(brewer.pal(9,"RdPu"), interpolate = "linear")
colintBrBG <- colorRampPalette(brewer.pal(11,"BrBG"),interpolate = "linear")
colintYlGn <- colorRampPalette(brewer.pal(9,"YlGn"),interpolate = "linear")
colintPuOr <- colorRampPalette(brewer.pal(9,"PuOr"),interpolate = "linear")
colintOrRd <- colorRampPalette( brewer.pal(9,"OrRd"),interpolate = "linear")
colintPRGn <- colorRampPalette( brewer.pal(11,"PRGn"),interpolate = "linear")
colintPiYG <- colorRampPalette( brewer.pal(11,"PiYG"),interpolate = "linear")
#windabs <- colorRampPalette(rainbow(8)[2:8],interpolate = "linear")
windabs2 <- colorRampPalette(colors = c("#00FF00", "#33FF33", "#66FF66", "#99FF99", "#CCFFCC", "#FFFF00", "#FFCC00", "#FF9900", "#FF6600", "#FF3300", "#FF0000", "#990000"), interpolate = "linear")
colintHWD <- colorRampPalette(colors = c("#FFFF00", "#FFCC00", "#FF9900", "#FF6600", "#FF3300", "#FF0000", "#CC0000", "#990000", "#660000", "#330000"),  interpolate = "linear")
colintCWDabs <- colorRampPalette(colors = c("#00FFFF", "#00CCFF", "#0099FF", "#0066FF", "#0033FF", "#0000FF", "#0000CC", "#000099", "#000066", "#000033"),  interpolate = "linear")
colintCWDano <- colorRampPalette(colors = c("#00FF00", "#33FF33", "#66FF66", "#99FF99", "#CCFFCC", "#FFFFFF", "#CCCCFF", "#9999FF", "#6666FF", "#3333FF", "#0000FF"),  interpolate = "linear")

  
  
#windabs2 <- colorRampPalette(c("#6389B3","#5BB2B8", "#3AB284","#3AB284", "#8DCE6B", "#AEC356", "#CAB942", "#AC4D85", "#AC4D85", "#9645A3","#895CAC", "#9C49D5","#D3B4ED", "#F6DFDF" , "white"),interpolate = "linear")
map_func_cols <- function(indic = NA, ind_tip = NA, perio_tip = NA, domain = NA) {
  # culori interpolate
  if (indic %in% c("tasAdjust", "tasmaxAdjust", "tasminAdjust")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        df.col <- data.frame(
          cols = c(rev(colintBlues(3)), colintYlOrRd(9)), 
          vals = seq(-6,16,2)
        ) 
      } else if  (perio_tip == "season") {
        df.col <- data.frame(
          cols = c(rev(colintBlues(8)), colintYlOrRd(20)), 
          vals = seq(-16,38,2)
        ) 
      } else {
        df.col <- data.frame(
          cols = c(rev(colintBlues(8)), colintYlOrRd(19)), 
          vals = seq(-16,36,2)
        ) 
      }
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "°C","</html>")
    }  else {
      df.col <- data.frame(
        cols = c(rev(colintBlues(12)),colintReds(15)), 
        vals = seq(-6,7, 0.5)
      )
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "°C","</html>")
    }
  }
  
  if (indic %in% c("rsds")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        df.col <- data.frame(
          cols = colintOrRd(8), 
          vals = seq(110,180,10)
        ) 
      } else if  (perio_tip == "season") {
        df.col <- data.frame(
          cols = colintOrRd(15), 
          vals = seq(20,300,20)
        ) 
      } else {
        df.col <- data.frame(
          cols = colintOrRd(16),
          vals = seq(20,320,20)
        ) 
      }
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "W/m²","</html>")
    }  else {
      df.col <- data.frame(
        cols = c(rev(colintBlues(6)),colintReds(7)), 
        vals = seq(-30,30, 5)
      )
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "W/m²","</html>")
    }
  }
  
  if (indic %in% c("wsgsmax")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        df.col <- data.frame(
          cols = windabs2(9), 
          vals =  seq(6,14,1)
        ) 
      } else if  (perio_tip == "season") {
        df.col <- data.frame(
          cols = windabs2(12), 
          vals =  seq(5,16,1)
        ) 
      } else {
        df.col <- data.frame(
          cols = windabs2(12),
          vals =  seq(4,15,1)
        ) 
      }
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "m/s","</html>")
    }  else {
      df.col <- data.frame(
        cols = rev(colintPRGn(13)), 
        vals = c(-5,-4,-3,-2,-1, -0.5, 0, 0.5, 1, 2,3,4,5)
      )
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "m/s","</html>")
    }
  }
  
  if (indic %in% c("prAdjust")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        df.col <- data.frame(
          cols = colintGnBu(13), 
          vals = seq(200,1400, 100)
        ) 
      } else if (perio_tip == "season") {
        df.col <- data.frame(
          cols = colintGnBu(11), 
          vals = c(seq(20,100, 20), seq(150,400, 50))
        )
      } else {
        df.col <- data.frame(
          cols = colintGnBu(11), 
          vals = c(10,20,30,40,seq(50,200, 25))
        )
      }
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "mm","</html>")
    } else {
      df.col <- data.frame(
        cols = colintBrBG(13), 
        vals = c(-50, -40,-30,-20,-10,-5, 0,5,10,20,30,40,50)
      )
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "%","</html>")
    }
  }
  
  if (indic %in% c("cdd")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        df.col <- data.frame(
          cols = colintReds(13), 
          vals = seq(0,60, 5)
        ) 
      }
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "zile","</html>")
    } else {
      df.col <- data.frame(
        cols = rev(brewer.pal(9,"RdYlGn")), 
        vals = seq(-20,20, 5)
      )
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "zile","</html>")
    }
  }
  
  if (indic %in% c("scorchno")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        df.col <- data.frame(
          cols = colintReds(15), 
          vals = seq(0,70, 5)
        ) 
      }
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "zile","</html>")
    } else {
      df.col <- data.frame(
        cols = c(rev(colintBlues(10)[3:4]),brewer.pal(7,"Reds")), 
        vals = seq(-20,60, 10)
      )
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "zile","</html>")
    }
  }
  
  if (indic %in% c("gsl")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        df.col <- data.frame(
          cols = colintYlGn(15), 
          vals = seq(0,365, 25)
        ) 
      }
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "zile","</html>")
    } else {
      df.col <- data.frame(
        cols = c(rev(colintYlOrBr(7)[3:7]),colintYlGn(9)[3:9]), 
        vals = seq(-125,150, 25)
      )
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "zile","</html>")
    }
  }
  
  if (indic %in% c("hddheat15.5")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        df.col <- data.frame(
          cols = colintPuOr(18), 
          vals = c(seq(0,2000, 250), seq(3000,7000, 500))
        ) 
      }
      leaflet_titleg <- "HDD (ΣTmed < 15.5°C)"
    } else {
      df.col <- data.frame(
        cols = c(colintBuPu(20)), 
        vals = c(seq(-2500,-1000, 500), seq(-900, 600, 100))
      )
      leaflet_titleg <- "HDD (ΣTmed < 15.5°C)"
    }
  }
  
  if (indic %in% c("cddcold22")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        df.col <- data.frame(
          cols = colintOrRd(18), 
          vals = c(seq(0, 100, 10),seq(150, 300, 50), seq(400, 600, 100))
        ) 
      }
      leaflet_titleg <- "CDD (ΣTmed > 22°C)"
    } else {
      df.col <- data.frame(
        cols = c(colintPuOr(14)), 
        vals = c(seq(-75,150, 25),seq(200,500, 100))
      )
      leaflet_titleg <- "CDD (ΣTmed > 22°C)"
    }
  }
  
  if (indic %in% c("r20mm")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        df.col <- data.frame(
          cols = colintBuPu(11), 
          vals = seq(0,10, 1)
        ) 
      }
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "zile","</html>")
    } else {
      df.col <- data.frame(
        cols =  colintBrBG(11), 
        vals = seq(-10,10, 2)
      )
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "zile","</html>")
    }
  }
  
  if (indic %in% c("hwd")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        df.col <- data.frame(
          cols = colintHWD(14), 
          vals = c(0,1,2,3,4,5,7.5,10,12.5,15, 20,30,40,50)
        ) 
      }
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "zile","</html>")
    } else {
      df.col <- data.frame(
        cols =  c(rev(colintBuPu(3)),colintYlOrRd(7)), 
        vals = c(-5,-2.5, 0,2.5,5,7.5, 10, 20,30,40)
      )
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "zile","</html>")
    }
  }
  
  if (indic %in% c("cwd")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        df.col <- data.frame(
          cols = colintCWDabs(15), 
          vals = c(1.0,2.0,3.0,4.0,5.0,6,7,8,9,10.0,11,12,13,14,15.0)
        ) 
      }
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "zile","</html>")
    } else {
      df.col <- data.frame(
        cols =  colintCWDano(11), 
        vals = -5:5
      )
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "zile","</html>")
    }
  }
  
  if (indic %in% c("wsdi")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        df.col <- data.frame(
          cols = colintYlOrBr(13), 
          vals = c(seq(0,50,5), 75,100)
        ) 
      }
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "zile","</html>")
    } else {
      df.col <- data.frame(
        cols =  c(rev(colintBuPu(3)),colintYlOrRd(9)), 
        vals = c(seq(-5,5, 2.5),seq(10, 70, 10))
      )
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "zile","</html>")
    }
  }
  
  if (indic %in% c("csdi")) {
    if (ind_tip == 'absol') {
      if (perio_tip == "year") {
        df.col <- data.frame(
          cols = colintBuPu(11), 
          vals = seq(0,10,1)
        ) 
      }
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "zile","</html>")
    } else {
      df.col <- data.frame(
        cols =  rev(c(rev(colintBuPu(4)),colintYlOrRd(10)[1:5])), 
        vals = c(-5,-3,-2,-1,0,1,2,3,5)
      )
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "zile","</html>")
    }
  }
  
  
  
  # print(head(df.col))
  # print(domain)
  ints <- findInterval(domain, df.col$vals, rightmost.closed = T, left.open = F)
  
  bins <-  df.col$vals[ints[1]:(ints[2] + 1)]
  cols <- df.col$cols[ints[1]:(ints[2])]
  
  # print(bins)
  # print(cols)
  # 
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

