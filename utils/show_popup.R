#show raster values on the map
show_popup <- function(x = NULL, y = NULL, rdat = NULL, proxy = NULL) {
  cell <- terra::cellFromXY(rdat, cbind(x, y))
  if (!is.na(cell)) {
    xy <- terra::xyFromCell(rdat, cell)
    val = rdat[cell]
    if (!is.na(val)) proxy %>% clearPopups() %>% addPopups(xy[1],xy[2], popup = paste0("lon:", round(x,3)," lat:", round(y,3), " val:", round(val,1)))
  }
}