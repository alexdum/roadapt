import warnings
import xarray as xr
import pandas as pd
import numpy as np

warnings.filterwarnings("ignore", category=FutureWarning)

# pentru situatiile cand avem zile in netcdf si trebuie duse in integer
indicators = ['cdd', 'gsl', 'r20mm', 'hwd','cwd', 'wsdi', 'csdi','txge35', 'tr', 'fd', 'hwf','txge30']
variables = ["rsds", "wsgsmax", "hurs", "sfcwind", "snd1cm", "snd5cm","snd30cm", "sndmean", "sndmax"]

def extract_point(fname, lon, lat, variable):
  with xr.open_dataset(fname) as ds:
    if variable in variables or ('lon' in ds.coords and 'lat' in ds.coords):
      dsloc = ds.sel(lon=lon, lat=lat, method='nearest')
    elif 'Longitude' in ds.coords and 'Latitude' in ds.coords:
      dsloc = ds.sel(Longitude=lon, Latitude=lat, method='nearest')
    elif 'x' in ds.coords and 'y' in ds.coords:
      dsloc = ds.sel(x=lon, y=lat, method='nearest')
    else:
      dsloc = ds.sel(Longitude=lon, Latitude=lat, method='nearest')

    if variable not in dsloc.data_vars:
      candidates = [v for v in dsloc.data_vars if v != "spatial_ref"]
      var_key = candidates[0] if candidates else variable
    else:
      var_key = variable

    dsloc = dsloc[var_key].to_pandas() 

  dsf = dsloc.rename_axis('time').reset_index(name='value')
  
  if variable in indicators: # cand ai variabile formatate ca zile transforma in integer
    if hasattr(dsf["value"].dt, "days"):
      dsf["value"] = dsf["value"].dt.days
    elif np.issubdtype(dsf["value"].dtype, np.timedelta64):
      dsf["value"] = dsf["value"] / np.timedelta64(1, 'D')
     
  return dsf
# 
#ds = xr.open_dataset("www/data/ncs/turism/snd1cm_rcp45_year-50_19710101_21001231.nc")

#dsloc = ds.sel( lon = 25, lat = 46, method='nearest')
#dsloc = dsloc["snd1cm"].to_pandas()
# dsf = dsloc.rename_axis('index1').reset_index()
# dsf = dsf.rename({'index1':'time', 0:'value'}, axis = 'columns')



