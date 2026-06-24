library(terra)
library(reticulate)

source_python("utils/xarray_calc.py")
source("utils/calcul_gen.R")

nc_fil <- "www/data/ncs/agro/cdd_rcp45_year-50_19710101_21001231.nc"
climgen_tip <- "abate"
perio_sub <- "year"
indic <- "cdd"
an1_abat <- 2071
an2_abat <- 2100
an1_abs <- 1971
an2_abs <- 2000

print("Calling calcul_gen...")
ncf <- calcul_gen(nc_fil, climgen_tip, perio_sub, indic, an1_abat, an2_abat, an1_abs, an2_abs)
print("calcul_gen completed successfully!")
print(ncf)
