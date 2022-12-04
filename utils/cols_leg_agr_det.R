cols_leg_agr_det <- function(indic, perio_tip, perio_sub, domain) {
  
  # intervale si culori agricultura detalii
  if (indic == "pr") {
    if (perio_tip == "year") {
      bins <- seq(round_even(min(domain),2,0), round_even(min(domain),2,1), by = 25)
    } else if (perio_tip == "season") {
      bins <- seq(floor(min(domain)), ceiling(max(domain)), by = 25)
    } else {
      bins <- seq(floor(min(domain)), ceiling(max(domain)), by = 5)
    }
    pal <- colorBin("GnBu", domain = domain, bins = bins, reverse = F)
    pal_rev <- colorBin("GnBu", domain = domain, bins = bins, reverse = T)
    tit_leg <- "mm"
    
  } else {
    bins <- seq(floor(min(domain)), ceiling(max(domain)), by = 2)
    pal <- colorBin("RdYlBu", domain = domain, bins = bins, reverse = T)
    pal_rev <- colorBin("RdYlBu", domain = domain, bins = bins, reverse = F)
    tit_leg <- "°C"
  }
  return(list(pal = pal, pal_rev = pal_rev, tit_leg = tit_leg))
}