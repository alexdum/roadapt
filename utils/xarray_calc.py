import xarray as xr
import numpy as np

def calc_mean_xarray(nc_fil, var_name, start_date_str, end_date_str, months=None):
    ds = xr.open_dataset(nc_fil)
    
    if var_name not in ds.data_vars:
        var_name = [v for v in ds.data_vars if v != "spatial_ref"][0]
        
    da = ds[var_name]
    
    time_dim = None
    for dim in da.dims:
        if ds[dim].dtype.kind == 'M': 
            time_dim = dim
            break
    if time_dim is None:
        time_dim = da.dims[0]
        
    da_sub = da.sel({time_dim: slice(start_date_str, end_date_str)})
    
    # Map season months if it's a seasonal file
    # select_interv has: Winter=01, Spring=04, Summer=07, Autumn=10
    # But seasonal netcdf files have: Winter=12, Spring=3, Summer=6, Autumn=9
    if months is not None:
        if isinstance(months, int):
            months = [months]
        if "season" in nc_fil.lower():
            months = [12 if m == 1 else 3 if m == 4 else 6 if m == 7 else 9 if m == 10 else m for m in months]

    print("Received months:", months, "Type:", type(months))
    if months is not None and len(months) > 0:
        da_sub = da_sub.sel({time_dim: da_sub[time_dim].dt.month.isin(months)})
        
    # Safety check for empty slice
    if da_sub.sizes[time_dim] == 0:
        spatial_shape = [da_sub.sizes[d] for d in da_sub.dims if d != time_dim]
        return np.full(spatial_shape, np.nan)
        
    da_mean = da_sub.mean(dim=time_dim, skipna=True).compute()
    val = da_mean.values
    
    # Convert timedelta to float days
    if np.issubdtype(val.dtype, np.timedelta64):
        val = val / np.timedelta64(1, 'D')
    # Convert datetime to float
    elif np.issubdtype(val.dtype, np.datetime64):
        val = val.astype(np.float64)
    # Ensure it is a standard numpy array, not a masked array
    if isinstance(val, np.ma.MaskedArray):
        val = val.filled(np.nan)
    elif not np.issubdtype(val.dtype, np.number):
        val = val.astype(np.float64)
        
    return val
