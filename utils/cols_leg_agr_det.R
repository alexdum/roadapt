cols_leg_agr_det <- function(indic, perio_tip, domain) {
  
  # intervale si culori agricultura
  if (indic == "pr") {
    if (perio_tip == "year") {
      bins <- seq(floor(min(domain)), ceiling(max(domain)), by = 100)
    } else if (perio_sub == "season") {
      bins <- seq(floor(min(domain)), ceiling(max(domain)), by = 50)
    } else {
      bins <- seq(floor(min(domain)), ceiling(max(domain)), by = 10)
    }
    pal <- colorBin("GnBu", domain = domain, bins = bins)
    pal_rev <- colorBin("GnBu", domain = domain, bins = bins, reverse = T)
    tit_leg <- "mm"
    
  } else {
    bins <- seq(floor(min(domain)), ceiling(max(domain)), by = 2)
    pal <- colorBin("RdYlBu", domain = domain, bins = bins, reverse = T)
    pal_rev <- colorBin("RdYlBu", domain = domain, bins = bins,reverse = F)
    tit_leg <- "°C"
  }
  
  return(list(pal = pal, pal_rev = pal_rev, tit_leg = tit_leg))
}