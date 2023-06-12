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
source("utils/map_func_min_max.R") # setare min max raster/value plotate
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
ltser <- st_read("www/data/shp/ltser.topojson", quiet = T) 
sci_spa <- st_read("www/data/shp/sci_spa.topojson", quiet = T) 


 

indicator_def <- read.csv("www/data/tabs/indicators_definition.csv")
select_climgen_ind <- read.csv("www/data/tabs/climgen/select_climgen_ind.csv") 
select_climgen_ind  <- setNames(select_climgen_ind$choice, select_climgen_ind$parameter)
select_agro_ind <- read.csv("www/data/tabs/agro/select_agro_ind.csv") 
select_agro_ind  <- setNames(select_agro_ind$choice, select_agro_ind$parameter)
select_energie_ind <- read.csv("www/data/tabs/energie/select_energie_ind.csv") 
select_energie_ind  <- setNames(select_energie_ind$choice, select_energie_ind$parameter)
select_hidro_ind <- read.csv("www/data/tabs/hidro/select_hidro_ind.csv") 
select_hidro_ind  <- setNames(select_hidro_ind$choice, select_hidro_ind$parameter)
select_silvicultura_ind <- read.csv("www/data/tabs/silvicultura/select_silvicultura_ind.csv") 
select_silvicultura_ind  <- setNames(select_silvicultura_ind$choice, select_silvicultura_ind$parameter)
select_biodivers_ind <- read.csv("www/data/tabs/biodivers/select_biodivers_ind.csv") 
select_biodivers_ind  <- setNames(select_biodivers_ind$choice, select_biodivers_ind$parameter)
select_cultura_ind <- read.csv("www/data/tabs/cultura/select_cultura_ind.csv") 
select_cultura_ind  <- setNames(select_cultura_ind$choice, select_cultura_ind$parameter)
select_turism_ind <- read.csv("www/data/tabs/turism/select_turism_ind.csv") 
select_turism_ind  <- setNames(select_turism_ind$choice, select_turism_ind$parameter)
select_transport_ind <- read.csv("www/data/tabs/transport/select_transport_ind.csv") 
select_transport_ind  <- setNames(select_transport_ind$choice, select_transport_ind$parameter)
select_urban_ind <- read.csv("www/data/tabs/urban/select_urban_ind.csv") 
select_urban_ind  <- setNames(select_urban_ind$choice, select_urban_ind$parameter)
select_sanatate_ind <- read.csv("www/data/tabs/sanatate/select_sanatate_ind.csv") 
select_sanatate_ind  <- setNames(select_sanatate_ind$choice, select_sanatate_ind$parameter)
select_industrie_ind <- read.csv("www/data/tabs/industrie/select_industrie_ind.csv") 
select_industrie_ind  <- setNames(select_industrie_ind$choice, select_industrie_ind$parameter)
select_asigurari_ind <- read.csv("www/data/tabs/asigurari/select_asigurari_ind.csv") 
select_asigurari_ind  <- setNames(select_asigurari_ind$choice, select_asigurari_ind$parameter)

select_interv <- read.csv("www/data/tabs/select_interv.csv") 
select_interv <- setNames(select_interv$choice, select_interv$parameter)



 
