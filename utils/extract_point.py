import xarray as xr
import pandas as pd

# pentru situatiile cand avem zile in netcdf si trebuie duse in integer
indicators = ['cdd', 'gsl', 'r20mm', 'hwd','cwd', 'wsdi', 'csdi']
variables = ["rsds", "wsgsmax", "hurs"]

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





#ds = xr.open_dataset("www/data/ncs/biodivers)
# dsloc = ds.sel(25,46,method='nearest')
# dsloc = dsloc["rsds"].to_pandas()
# dsf = dsloc.rename_axis('index1').reset_index()
# dsf = dsf.rename({'index1':'time', 0:'value'}, axis = 'columns')



