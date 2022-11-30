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

source_python("utils/extract_point.py")
source("utils/names_to_date.R")
source("utils/leaflet_fun_gen.R")
source("utils/leaflet_fun_det.R")
source("utils/cols_leg_agr_det.R")
source("utils/calcul_agr_det.R")
source("utils/show_popup.R")


borders <- st_read("www/data/shp/ne_10m_admin_0_boundary_lines_land_rou.shp")
mask <- vect("www/data/shp/rou_border_mbufer.shp") 
mask <- project(mask,  "EPSG:3857")
         

uat <- st_read("www/data/shp/UAT_poli_2019_merc.shp", quiet = T) |>
     st_transform(4326)

select_agro_ind <- read.csv("www/data/tabs/agro/select_agro_ind.csv") 
select_agro_ind  <- setNames(select_agro_ind$choice, select_agro_ind$parameter)

select_interv <- read.csv("www/data/tabs/select_interv.csv") 
select_interv <- setNames(select_interv$choice, select_interv$parameter)



 
