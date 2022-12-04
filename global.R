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
source("utils/leaflet_fun_gen.R") # harta tab general
source("utils/leaflet_fun_det.R") # harta tab detalii
source("utils/show_popup.R") # popup  date raster
source("utils/cols_leg_agr_det.R") # culori legenda detalii
source("utils/calcul_agr_gen.R") # calcul anomalii normale  detalii agro
source("utils/calcul_agr_det.R") # calcul anomalii normale  detalii agro
source("utils/extract_timeserie_gen.R") # extrage time serii pentru plot din ncs
source("utils/plots_gen.R") # plot din date raster




borders <- st_read("www/data/shp/ne_10m_admin_0_boundary_lines_land_rou.shp", quiet = T)
mask <- vect("www/data/shp/rou_border_mbufer.shp") 
mask <- project(mask,  "EPSG:3857")
         

uat <- st_read("www/data/shp/UAT_poli_2019_merc.shp", quiet = T) |>
     st_transform(4326)

select_agro_ind <- read.csv("www/data/tabs/agro/select_agro_ind.csv") 
select_agro_ind  <- setNames(select_agro_ind$choice, select_agro_ind$parameter)

select_interv <- read.csv("www/data/tabs/select_interv.csv") 
select_interv <- setNames(select_interv$choice, select_interv$parameter)



 
