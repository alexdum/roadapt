library(shiny)
library(gridlayout)
library(ggplot2)
library(leaflet)
library(terra)
library(RColorBrewer)
library(sf)
library(dplyr)
library(arrow)

source("utils/names_to_date.R")
source("utils/leaflet_fun.R")


borders <- st_read("www/data/shp/ne_10m_admin_0_boundary_lines_land_rou.shp")
mask <- vect("www/data/shp/rou_border_mbufer.shp") 
mask <- project(mask,  "EPSG:3857")
         


select_agro_ind <- read.csv("www/data/tabs/agro/select_agro_ind.csv") 
select_agro_ind  <- setNames(select_agro_ind$choice, select_agro_ind$parameter)

select_interv <- read.csv("www/data/tabs/select_interv.csv") 
select_interv <- setNames(select_interv$choice, select_interv$parameter)



 
