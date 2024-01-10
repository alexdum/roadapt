import xarray as xr
import pandas as pd

# pentru situatiile cand avem zile in netcdf si trebuie duse in integer
indicators = ['cdd', 'gsl', 'r20mm', 'hwd','cwd', 'wsdi', 'csdi','txge35', 'tr', 'fd', 'hwf','txge30']
# 
variables = ["rsds", "wsgsmax", "hurs", "sfcwind", "snd1cm", "snd5cm","snd30cm"]

def extract_point(fname, lon, lat, variable):
  ds = xr.open_dataset(fname)
  ds.close()
  if variable in variables: # cand ai coordonatele denumite alrfel
    dsloc = ds.sel(lon=lon,lat=lat,method='nearest')
  else:
    dsloc = ds.sel(Longitude=lon,Latitude=lat,method='nearest')

  dsloc = dsloc[variable].to_pandas() 
  dsf = dsloc.rename_axis('index1').reset_index() # numele coloanei in coloana
  dsf = dsf.rename({'index1':'time', 0:'value'}, axis = 'columns') # rename columns
  
  if variable in indicators: # cand ai variabile formatate ca zile transforma in integer
     dsf["value"] = dsf["value"].dt.days
     
  return(dsf)
# 
#ds = xr.open_dataset("www/data/ncs/turism/snd1cm_rcp45_year-50_19710101_21001231.nc")

#dsloc = ds.sel( lon = 25, lat = 46, method='nearest')
#dsloc = dsloc["snd1cm"].to_pandas()
# dsf = dsloc.rename_axis('index1').reset_index()
# dsf = dsf.rename({'index1':'time', 0:'value'}, axis = 'columns')



