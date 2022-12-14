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
source("utils/names_to_date.R") # extrage data din fisere netcdf
source("utils/map_func_cols.R") # plot din date raster
source("utils/leaflet_fun_gen.R") # harta tab national
source("utils/leaflet_fun_det.R") # harta tab administrativ
source("utils/show_popup.R") # popup  date raster
source("utils/extract_timeserie_gen.R") # extrage time serii pentru plot din ncs
source("utils/extract_timeserie_det.R") # extrage time serii pentru plot din parquet
source("utils/plots_gen.R") # plot din date raster
source("utils/plots_det.R") # plot din date parquet
source("utils/calcul_gen.R") # calcul anomalii normale general 
source("utils/calcul_det.R") # calcul anomalii normale  detalii 


borders <- st_read("www/data/shp/granita.shp", quiet = T) # pentru afisare harta
mask <- vect("www/data/shp/mask.shp") # pentru mask raster
         

uat <- st_read("www/data/shp/uat.topojson", quiet = T)
jud <- st_read("www/data/shp/judete.topojson", quiet = T) |> dplyr::rename(natcode = mnemonic)
reg <- st_read("www/data/shp/regiuni.topojson", quiet = T) |> dplyr::rename(natcode = regionId, name = region)
 

indicator_def <- read.csv("www/data/tabs/indicators_definition.csv")
select_climgen_ind <- read.csv("www/data/tabs/climgen/select_climgen_ind.csv") 
select_climgen_ind  <- setNames(select_climgen_ind$choice, select_climgen_ind$parameter)
select_agro_ind <- read.csv("www/data/tabs/agro/select_agro_ind.csv") 
select_agro_ind  <- setNames(select_agro_ind$choice, select_agro_ind$parameter)
select_energie_ind <- read.csv("www/data/tabs/energie/select_energie_ind.csv") 
select_energie_ind  <- setNames(select_energie_ind$choice, select_energie_ind$parameter)

select_interv <- read.csv("www/data/tabs/select_interv.csv") 
select_interv <- setNames(select_interv$choice, select_interv$parameter)



 
