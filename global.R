library(shiny)
library(gridlayout)
library(ggplot2)
library(leaflet)
library(raster)

source("utils/names_to_date.R")

select_agro_ind <- read.csv("www/data/tabs/agro/select_agro_ind.csv") 
select_agro_ind  <- setNames(select_agro_ind$choice, select_agro_ind$parameter)

select_interv <- read.csv("www/data/tabs/select_interv.csv") 
select_interv <- setNames(select_interv$choice, select_interv$parameter)



 
