library(shiny)
library(shinyjs)
library(gridlayout)
library(ggplot2)
library(leaflet)
library(terra)
library(RColorBrewer)
library(sf)
library(dplyr)
library(arrow)
library(shinycssloaders)
library(reticulate)
library(ggplot2)
library(plotly)
library(DT)

source_python("utils/extract_point.py")
source("utils/utils.R")
source("utils/names_to_date.R") # c]mp data din fisere netcdf
source("utils/map_func_cols.R") # plot din date raster
source("utils/leaflet_fun_gen.R") # harta tab general
source("utils/leaflet_fun_det.R") # harta tab detalii
source("utils/show_popup.R") # popup  date raster
source("utils/calcul_agr_gen.R") # calcul anomalii normale  detalii agro
source("utils/calcul_agr_det.R") # calcul anomalii normale  detalii agro
source("utils/extract_timeserie_gen.R") # extrage time serii pentru plot din ncs
source("utils/extract_timeserie_det.R") # extrage time serii pentru plot din parquet
source("utils/plots_gen.R") # plot din date raster
source("utils/plots_det.R") # plot din date parquet



# pentru adisare harta
borders <- st_read("www/data/shp/granita.shp", quiet = T) 
# pentru mask raster
mask <- vect("www/data/shp/mask.shp")
         

uat <- st_read("www/data/shp/uat.topojson", quiet = T)
 

select_agro_ind <- read.csv("www/data/tabs/agro/select_agro_ind.csv") 
select_agro_ind  <- setNames(select_agro_ind$choice, select_agro_ind$parameter)

select_interv <- read.csv("www/data/tabs/select_interv.csv") 
select_interv <- setNames(select_interv$choice, select_interv$parameter)



 
