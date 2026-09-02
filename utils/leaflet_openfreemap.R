# utils/leaflet_openfreemap.R

# Preload OpenFreeMap Positron styles
ofm_style_nolabels <- jsonlite::fromJSON("www/styles/ofm_positron_nolabels.json", simplifyVector = FALSE)
ofm_style_onlylabels <- jsonlite::fromJSON("www/styles/ofm_positron_onlylabels.json", simplifyVector = FALSE)

#' Add OpenFreeMap basemap and overlay labels to Leaflet map
#'
#' @param map A Leaflet map object
#' @param baseGroup Name for the base layer in layers control
#' @param labelGroup Name for the labels layer in layers control
#' @return The modified Leaflet map object
addOpenFreeMap <- function(map, baseGroup = "OpenFreeMap", labelGroup = "Labels") {
  js_code <- '
  function(el, x, data) {
    var map = this;

    // Ensure maplabels pane has pointer-events: none so clicks go through to polygons and raster
    if (map.getPane("maplabels")) {
      map.getPane("maplabels").style.pointerEvents = "none";
    }

    var baseLayer = L.maplibreGL({
      style: data.styleNoLabels,
      pane: "tilePane",
      interactive: false
    });

    var labelsLayer = L.maplibreGL({
      style: data.styleOnlyLabels,
      pane: "maplabels",
      interactive: false
    });

    if (map.layerManager) {
      map.layerManager.addLayer(baseLayer, "tile", "ofm_base_" + el.id, data.baseGroup);
      map.layerManager.addLayer(labelsLayer, "tile", "ofm_labels_" + el.id, data.labelGroup);
    } else {
      baseLayer.addTo(map);
      labelsLayer.addTo(map);
    }
  }
  '

  htmlwidgets::onRender(
    map,
    js_code,
    data = list(
      styleNoLabels = ofm_style_nolabels,
      styleOnlyLabels = ofm_style_onlylabels,
      baseGroup = baseGroup,
      labelGroup = labelGroup
    )
  )
}
