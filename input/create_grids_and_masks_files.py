#!/bin/python
# ---------------------------------------------------------
#             Auteur  (date de creation) :
#         J. Pianezze (   12.04.2024   )
#
#                  ~~~~~~~~~~~~~~~
#     Script used to create grids.nc and masks.nc files
#             to create rmp files for OASIS
#                  ~~~~~~~~~~~~~~~
#
# ---------------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
import netCDF4
import numpy as np
import os, sys
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# #########################################################
# ###           To be defined by user                   ###
# #########################################################

cfg_use_mnh   = True  ; cfg_file_mnh   = '/home/piaj/03_workdir/2J_devel_MNH_WW3_CROCO/tree_tmpl_for_mnh_ww3_croco_coupling/1_input_mnh/PGD_IROISE_5km.nc'
cfg_use_ww3   = False ; cfg_file_ww3   = '/home/piaj/03_workdir/2J_devel_MNH_WW3_CROCO/tree_tmpl_for_mnh_ww3_croco_coupling/A2_frc_ww3_spinup/ww3.20210915.nc'
cfg_use_croco = True  ; cfg_file_croco = '/home/piaj/03_workdir/2J_devel_MNH_WW3_CROCO/tree_tmpl_for_mnh_ww3_croco_coupling/3_input_croco/croco_grd.nc'

# #########################################################

# --------------------------------------------------------
if cfg_use_croco:

  file_croco = netCDF4.Dataset(cfg_file_croco)

  # --- Read lon, lat
  lon_croco  = file_croco.variables['lon_rho'] [1:-1,1:-1]
  lat_croco  = file_croco.variables['lat_rho'] [1:-1,1:-1]
  nlon_croco = np.size(lon_croco[0,:]) ; print('nlon_croco =', nlon_croco)
  nlat_croco = np.size(lat_croco[:,0]) ; print('nlat_croco =', nlat_croco)
  
  # --- Read mask and invert 0 to 1 for OASIS
  mask_croco_ini = file_croco.variables['mask_rho'][1:-1,1:-1]
  mask_croco     = np.zeros((nlat_croco,nlon_croco))

  for ind_lon in range(nlon_croco):
    for ind_lat in range(nlat_croco):
      if mask_croco_ini[ind_lat,ind_lon] == 1.0 :
        mask_croco[ind_lat,ind_lon]=0
      else:
        mask_croco[ind_lat,ind_lon]=1

# --------------------------------------------------------
if cfg_use_mnh:

  file_mnh = netCDF4.Dataset(cfg_file_mnh)

  # --- Read lon, lat
  lon_mnh  = file_mnh.variables['longitude'][1:-1,1:-1]
  lat_mnh  = file_mnh.variables['latitude'][1:-1,1:-1]
  nlon_mnh = np.size(lon_mnh[0,:]) ; print('nlon_mnh=', nlon_mnh)
  nlat_mnh = np.size(lat_mnh[:,0]) ; print('nlat_mnh=', nlat_mnh)  
  
  # --- Read mask and invert 0 to 1 for OASIS
  frac_sea_mnh = file_mnh.variables['FRAC_SEA'][1:-1,1:-1]

  mask_mnh = np.zeros((nlat_mnh,nlon_mnh))

  for ind_lon in range(nlon_mnh):
    for ind_lat in range(nlat_mnh):
      if frac_sea_mnh[ind_lat,ind_lon] == 0.0 or frac_sea_mnh[ind_lat,ind_lon]>=999.0 :
        mask_mnh[ind_lat,ind_lon]=1
      else:
        mask_mnh[ind_lat,ind_lon]=0  	

# --------------------------------------------------------
if cfg_use_ww3:

  file_ww3 = netCDF4.Dataset(cfg_file_ww3)

  #------ Read variables
  lon_ww3      = file_ww3.variables['longitude'][:,:]
  lat_ww3      = file_ww3.variables['latitude'][:,:]
  mask_ww3_ori = file_ww3.variables['MAPSTA'][:,:]

  nlon_ww3  = np.size(lon_ww3[0,:]) ;  print('nlon_ww3=', nlon_ww3)
  nlat_ww3  = np.size(lat_ww3[:,0]) ;  print('nlat_ww3=', nlat_ww3)
  
  print('---- mask and var send by toy')
  mask_ww3 = np.zeros((nlat_ww3,nlon_ww3))

  for ind_lon in range(nlon_ww3):
    for ind_lat in range(nlat_ww3):
      if mask_ww3_ori[ind_lat,ind_lon] == 1.0 or mask_ww3_ori[ind_lat,ind_lon] == 2.0:
        mask_ww3[ind_lat,ind_lon]=0
      else:
        mask_ww3[ind_lat,ind_lon]=1

print('---------------------------------------------------')
print('   Creating grids.nc                               ')
print('---------------------------------------------------')

grids_file             = netCDF4.Dataset('grids.nc','w')
grids_file.Description ='Grids file used to create rmp files for OASIS coupling'

if cfg_use_croco:
  grids_file.createDimension ('x_ocnt', nlon_croco)
  grids_file.createDimension ('y_ocnt', nlat_croco)  
  grids_file.createVariable  ('ocnt.lon','d',('y_ocnt','x_ocnt'))
  grids_file.createVariable  ('ocnt.lat','d',('y_ocnt','x_ocnt'))
  grids_file.variables       ['ocnt.lon'][:,:] = lon_croco[:,:]
  grids_file.variables       ['ocnt.lat'][:,:] = lat_croco[:,:]    
  
if cfg_use_mnh:
  grids_file.createDimension ('x_atmt', nlon_mnh)
  grids_file.createDimension ('y_atmt', nlat_mnh)
  grids_file.createVariable  ('atmt.lon','d',('y_atmt','x_atmt'))
  grids_file.createVariable  ('atmt.lat','d',('y_atmt','x_atmt'))  
  grids_file.variables       ['atmt.lon'][:,:] = lon_mnh[:,:]
  grids_file.variables       ['atmt.lat'][:,:] = lat_mnh[:,:]

if cfg_use_ww3:
  grids_file.createDimension ('x_wavt', nlon_ww3)
  grids_file.createDimension ('y_wavt', nlat_ww3)
  grids_file.createVariable  ('wavt.lon','d',('y_wavt','x_wavt'))
  grids_file.createVariable  ('wavt.lat','d',('y_wavt','x_wavt'))  
  grids_file.variables       ['wavt.lon'][:,:] = lon_ww3[:,:]
  grids_file.variables       ['wavt.lat'][:,:] = lat_ww3[:,:]

grids_file.close()

print('---------------------------------------------------')
print('   Creating masks.nc                               ')
print('---------------------------------------------------')

masks_file             = netCDF4.Dataset('masks.nc','w')
masks_file.Description = 'Masks file used to create rmp files for OASIS coupling'

if cfg_use_croco:
  masks_file.createDimension       ('x_ocnt', nlon_croco)
  masks_file.createDimension       ('y_ocnt', nlat_croco)    
  varout=masks_file.createVariable ('ocnt.msk','i',('y_ocnt','x_ocnt'))  
  masks_file.variables             ['ocnt.msk'][:,:] = mask_croco[:,:]  
  
if cfg_use_mnh:
  masks_file.createDimension       ('x_atmt', nlon_mnh)
  masks_file.createDimension       ('y_atmt', nlat_mnh)
  varout=masks_file.createVariable ('atmt.msk','i',('y_atmt','x_atmt'))
  masks_file.variables             ['atmt.msk'][:,:] = mask_mnh[:,:]
  
if cfg_use_ww3:
  masks_file.createDimension       ('x_wavt', nlon_ww3)
  masks_file.createDimension       ('y_wavt', nlat_ww3)
  varout=masks_file.createVariable ('wavt.msk','i',('y_wavt','x_wavt'))
  masks_file.variables             ['wavt.msk'][:,:] = mask_ww3[:,:]

masks_file.close()
