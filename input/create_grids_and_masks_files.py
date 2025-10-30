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
import os, sys, shutil
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# #########################################################
# ###           To be defined by user                   ###
# #########################################################

cfg_dir_config = '../../config_TUTORIAL/'

cfg_use_MNH   = True  ; cfg_file_MNH   = cfg_dir_config + '1_input_MNH/PGD_BENGUELA_10km.nc'
cfg_use_CROCO = True  ; cfg_file_CROCO = cfg_dir_config + '2_input_CROCO/croco_grd.nc'
cfg_use_WW3   = False ; cfg_file_WW3   = cfg_dir_config + 'A2_frc_WW3_spinup/ww3.20220914.nc'

# #########################################################

print('---------------------------------------------------')
print('   Read files                                      ')
print('---------------------------------------------------')

# --------------------------------------------------------
if cfg_use_CROCO:

  file_CROCO = netCDF4.Dataset(cfg_file_CROCO)

  # --- Read lon, lat
  lon_CROCO  = file_CROCO.variables['lon_rho'] [1:-1,1:-1]
  lat_CROCO  = file_CROCO.variables['lat_rho'] [1:-1,1:-1]
  nlon_CROCO = np.size(lon_CROCO[0,:]) ; print('nlon_CROCO =', nlon_CROCO)
  nlat_CROCO = np.size(lat_CROCO[:,0]) ; print('nlat_CROCO =', nlat_CROCO)
  
  # --- Read mask and invert 0 to 1 for OASIS
  mask_CROCO = file_CROCO.variables['mask_rho'][1:-1,1:-1]
  mask_CROCO = np.where((mask_CROCO == 1), 0, 1)

# --------------------------------------------------------
if cfg_use_MNH:

  file_MNH = netCDF4.Dataset(cfg_file_MNH)

  # --- Read lon, lat
  lon_MNH  = file_MNH.variables['longitude'][1:-1,1:-1]
  lat_MNH  = file_MNH.variables['latitude'] [1:-1,1:-1]
  nlon_MNH = np.size(lon_MNH[0,:]) ; print('nlon_MNH=', nlon_MNH)
  nlat_MNH = np.size(lat_MNH[:,0]) ; print('nlat_MNH=', nlat_MNH)  
  
  # --- Read mask and invert 0 to 1 for OASIS
  mask_MNH = file_MNH.variables['FRAC_SEA'][1:-1,1:-1]
  mask_MNH = np.where((mask_MNH == 0.0) | (mask_MNH >= 999.0), 1, 0)

# --------------------------------------------------------
if cfg_use_WW3:

  file_WW3 = netCDF4.Dataset(cfg_file_WW3)

  #------ Read lon, lat
  lon_WW3  = file_WW3.variables['longitude'][:,:]
  lat_WW3  = file_WW3.variables['latitude'] [:,:]
  nlon_WW3 = np.size(lon_WW3[0,:]) ; print('nlon_WW3=', nlon_WW3)
  nlat_WW3 = np.size(lat_WW3[:,0]) ; print('nlat_WW3=', nlat_WW3)

  # --- Read mask and invert 0 to 1 for OASIS
  mask_WW3 = file_WW3.variables['MAPSTA'][:,:]
  mask_WW3 = np.where((mask_WW3 == 1.0) | (mask_MNH == 2.0), 0, 1)

print('---------------------------------------------------')
print('   Creating grids.nc                               ')
print('---------------------------------------------------')

grids_file             = netCDF4.Dataset('grids.nc','w')
grids_file.Description ='Grids file used to create rmp files for OASIS coupling'

if cfg_use_CROCO:
  grids_file.createDimension ('ocnt.nlon', nlon_CROCO)
  grids_file.createDimension ('ocnt.nlat', nlat_CROCO)  
  grids_file.createVariable  ('ocnt.lon','d',('ocnt.nlat','ocnt.nlon'))
  grids_file.createVariable  ('ocnt.lat','d',('ocnt.nlat','ocnt.nlon'))
  grids_file.variables       ['ocnt.lon'][:,:] = lon_CROCO[:,:]
  grids_file.variables       ['ocnt.lat'][:,:] = lat_CROCO[:,:]    
  
if cfg_use_MNH:
  grids_file.createDimension ('atmt.nlon', nlon_MNH)
  grids_file.createDimension ('atmt.nlat', nlat_MNH)
  grids_file.createVariable  ('atmt.lon','d',('atmt.nlat','atmt.nlon'))
  grids_file.createVariable  ('atmt.lat','d',('atmt.nlat','atmt.nlon'))  
  grids_file.variables       ['atmt.lon'][:,:] = lon_MNH[:,:]
  grids_file.variables       ['atmt.lat'][:,:] = lat_MNH[:,:]

if cfg_use_WW3:
  grids_file.createDimension ('wavt.nlon', nlon_WW3)
  grids_file.createDimension ('wavt.nlat', nlat_WW3)
  grids_file.createVariable  ('wavt.lon','d',('wavt.nlat','wavt.nlon'))
  grids_file.createVariable  ('wavt.lat','d',('wavt.nlat','wavt.nlon'))  
  grids_file.variables       ['wavt.lon'][:,:] = lon_WW3[:,:]
  grids_file.variables       ['wavt.lat'][:,:] = lat_WW3[:,:]

grids_file.close()

print('---------------------------------------------------')
print('   Creating masks.nc                               ')
print('---------------------------------------------------')

masks_file             = netCDF4.Dataset('masks.nc','w')
masks_file.Description = 'Masks file used to create rmp files for OASIS coupling'

if cfg_use_CROCO:
  masks_file.createDimension       ('ocnt.nlon', nlon_CROCO)
  masks_file.createDimension       ('ocnt.nlat', nlat_CROCO)    
  varout=masks_file.createVariable ('ocnt.msk','i',('ocnt.nlat','ocnt.nlon'))  
  masks_file.variables             ['ocnt.msk'][:,:] = mask_CROCO[:,:]  
  
if cfg_use_MNH:
  masks_file.createDimension       ('atmt.nlon', nlon_MNH)
  masks_file.createDimension       ('atmt.nlat', nlat_MNH)
  varout=masks_file.createVariable ('atmt.msk','i',('atmt.nlat','atmt.nlon'))
  masks_file.variables             ['atmt.msk'][:,:] = mask_MNH[:,:]
  
if cfg_use_WW3:
  masks_file.createDimension       ('wavt.nlat', nlon_WW3)
  masks_file.createDimension       ('wavt.nlon', nlat_WW3)
  varout=masks_file.createVariable ('wavt.msk','i',('wavt.nlat','wavt.nlon'))
  masks_file.variables             ['wavt.msk'][:,:] = mask_WW3[:,:]

masks_file.close()

print('---------------------------------------------------')
print('   Edit namcouple                                  ')
print('---------------------------------------------------')

if cfg_use_MNH and cfg_use_CROCO:

  shutil.copy('namcouple_tmpl', 'namcouple_atmt_ocnt') 
  replacement = f"{nlat_MNH} {nlon_MNH} {nlat_CROCO} {nlon_CROCO} atmt ocnt"
  os.system(f"sed -i 's|nlat_model1 nlon_model1 nlat_model2 nlon_model2 id_model1 id_model2|{replacement}|g' namcouple_atmt_ocnt")

  shutil.copy('namcouple_tmpl', 'namcouple_ocnt_atmt')
  replacement = f"{nlat_CROCO} {nlon_CROCO} {nlat_MNH} {nlon_MNH} ocnt atmt"
  os.system(f"sed -i 's|nlat_model1 nlon_model1 nlat_model2 nlon_model2 id_model1 id_model2|{replacement}|g' namcouple_ocnt_atmt")
