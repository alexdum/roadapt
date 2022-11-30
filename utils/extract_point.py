import xarray as xr

def extract_point(fname, lon, lat, variable):
  ds = xr.open_dataset(fname)
  ds.close()
  dsloc = ds.sel(Longitude=lon,Latitude=lat,method='nearest')
  dsloc = dsloc[variable].to_pandas() 
  dsf = dsloc.rename_axis('index1').reset_index() # numele coloanei in coloana
  dsf = dsf.rename({'index1':'time', 0:'value'}, axis = 'columns') # rename columns
  return(dsf)





# ds = xr.open_dataset("www/data/ncs/agro/prAdjust_rcp45_season-50_19710101_21001231.nc")
# dsloc = ds.sel(Longitude=25,Latitude=46,method='nearest')
# dsloc = dsloc["prAdjust"].to_pandas()
# dsf = dsloc.rename_axis('index1').reset_index()
# dsf.rename({'index1':'time', 0:'value'}, axis = 'columns')
