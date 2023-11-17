#!/usr/bin/python
# -*- coding: utf-8 -*-
#
###################################################
#=================================================#
#  Creating grid and restart file for toy model
#  Author : J. Pianezze
#  Date   :        2020
#=================================================#
###################################################
#
import netCDF4
import numpy as np
import scipy
import matplotlib.pyplot as plt
import math
from   pylab import *
import os
#
curdir_path=os.getcwd()+'/'
home_path=os.environ['HOME']
print('###   Dossier courant:', curdir_path)
#
script_name=os.path.splitext(os.path.basename(sys.argv[0]))[0]
#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

######### BEG USER PART ###############################

cfg_file_mnh   = '../../../1_input_mnh_gpu_sans_oasis/PGD_ALEX_DX6400M_64x64.nc'
cfg_file_ww3   = '../../../A2_frc_ww3_spinup/ww3.20201001.nc'

######### END USER PART ###############################

# -------------------------------------------------
#  MNH
# -------------------------------------------------

file_orog = netCDF4.Dataset(cfg_file_mnh)

#------ Read variables
lon_MNH  = file_orog.variables['longitude'][1:-1,1:-1]
lat_MNH  = file_orog.variables['latitude'][1:-1,1:-1]
frac_sea_MNH = file_orog.variables['FRAC_SEA'][1:-1,1:-1]

nlon_MNH=np.size(lon_MNH[0,:]) ;  print('nlon_MNH=', nlon_MNH)
nlat_MNH=np.size(lat_MNH[:,0]) ;  print('nlat_MNH=', nlat_MNH)
ncorn_MNH=4                     ;  print('ncorn_MNH=', ncorn_MNH)

print('---- corners longitude/latitude')
clo_MNH=np.zeros((ncorn_MNH,nlat_MNH,nlon_MNH))
cla_MNH=np.zeros((ncorn_MNH,nlat_MNH,nlon_MNH))

deltax=lon_MNH[0,1]-lon_MNH[0,0] ; print('deltax=', deltax)
clo_MNH[0,:,:]=lon_MNH[:,:]+deltax/2.0
clo_MNH[1,:,:]=lon_MNH[:,:]-deltax/2.0
clo_MNH[2,:,:]=lon_MNH[:,:]-deltax/2.0
clo_MNH[3,:,:]=lon_MNH[:,:]+deltax/2.0

deltay=lat_MNH[1,0]-lat_MNH[0,0] ; print('deltay=', deltay)
cla_MNH[0,:,:]=lat_MNH[:,:]+deltay/2.0
cla_MNH[1,:,:]=lat_MNH[:,:]+deltay/2.0
cla_MNH[2,:,:]=lat_MNH[:,:]-deltay/2.0
cla_MNH[3,:,:]=lat_MNH[:,:]-deltay/2.0

print('---- area')
area_MNH=np.zeros((nlat_MNH,nlon_MNH))
area_MNH[:,:]=deltax*deltay


print('---- mask and var send by toy')
mask_MNH = np.zeros((nlat_MNH,nlon_MNH))

for ind_lon in range(nlon_MNH):
  for ind_lat in range(nlat_MNH):
    if frac_sea_MNH[ind_lat,ind_lon] == 0.0 or frac_sea_MNH[ind_lat,ind_lon]>=999.0 :
      mask_MNH[ind_lat,ind_lon]=1
    else:
      mask_MNH[ind_lat,ind_lon]=0  	

# -------------------------------------------------
#  WW3
# -------------------------------------------------

file_WW3 = netCDF4.Dataset(cfg_file_ww3)

#------ Read variables
lon_WW3      = file_WW3.variables['longitude'][:,:]
lat_WW3      = file_WW3.variables['latitude'][:,:]
mask_WW3_ori = file_WW3.variables['MAPSTA'][:,:]

nlon_WW3  = np.size(lon_WW3[0,:]) ;  print('nlon_WW3=', nlon_WW3)
nlat_WW3  = np.size(lat_WW3[:,0]) ;  print('nlat_WW3=', nlat_WW3)
ncorn_WW3 = 4                     ;  print('ncorn_WW3=', ncorn_WW3)

print('---- corners longitude/latitude')
clo_WW3=np.zeros((ncorn_WW3,nlat_WW3,nlon_WW3))
cla_WW3=np.zeros((ncorn_WW3,nlat_WW3,nlon_WW3))

deltax=lon_WW3[0,1]-lon_WW3[0,0] ; print('deltax=', deltax)
clo_WW3[0,:,:]=lon_WW3[:,:]+deltax/2.0
clo_WW3[1,:,:]=lon_WW3[:,:]-deltax/2.0
clo_WW3[2,:,:]=lon_WW3[:,:]-deltax/2.0
clo_WW3[3,:,:]=lon_WW3[:,:]+deltax/2.0

deltay=lat_WW3[1,0]-lat_WW3[0,0] ; print('deltay=', deltay)
cla_WW3[0,:,:]=lat_WW3[:,:]+deltay/2.0
cla_WW3[1,:,:]=lat_WW3[:,:]+deltay/2.0
cla_WW3[2,:,:]=lat_WW3[:,:]-deltay/2.0
cla_WW3[3,:,:]=lat_WW3[:,:]-deltay/2.0

print('---- area')
area_WW3      = np.zeros((nlat_WW3,nlon_WW3))
area_WW3[:,:] = deltax*deltay


print('---- mask and var send by toy')
mask_WW3 = np.zeros((nlat_WW3,nlon_WW3))

for ind_lon in range(nlon_WW3):
  for ind_lat in range(nlat_WW3):
    if mask_WW3_ori[ind_lat,ind_lon] == 1.0 or mask_WW3_ori[ind_lat,ind_lon] == 2.0:
      mask_WW3[ind_lat,ind_lon]=0
    else:
      mask_WW3[ind_lat,ind_lon]=1

##################################################
print('------------------------------------------')
print(' Creating netcdf file : grids.nc')

grids_file=netCDF4.Dataset(curdir_path+'grids.nc','w',format='NETCDF3_64BIT')
grids_file.Description='Grid file for OASIS coupling'

# ----------------------------------
# Create the dimensions of the files
# ----------------------------------
grids_file.createDimension ('x_atmt', nlon_MNH)
grids_file.createDimension ('y_atmt', nlat_MNH)
grids_file.createDimension ('crn_atmt', 4 )

grids_file.createDimension ('x_wavt', nlon_WW3)
grids_file.createDimension ('y_wavt', nlat_WW3)
grids_file.createDimension ('crn_wavt', 4 )

# ----------------------------------
# Create the variables of the files
# ----------------------------------
varout=grids_file.createVariable('atmt.lon','d',('y_atmt','x_atmt'))
varout=grids_file.createVariable('atmt.lat','d',('y_atmt','x_atmt'))
varout=grids_file.createVariable('atmt.clo','d',('crn_atmt','y_atmt','x_atmt'))
varout=grids_file.createVariable('atmt.cla','d',('crn_atmt','y_atmt','x_atmt'))

varout=grids_file.createVariable('wavt.lon','d',('y_wavt','x_wavt'))
varout=grids_file.createVariable('wavt.lat','d',('y_wavt','x_wavt'))
varout=grids_file.createVariable('wavt.clo','d',('crn_wavt','y_wavt','x_wavt'))
varout=grids_file.createVariable('wavt.cla','d',('crn_wavt','y_wavt','x_wavt'))

# ---------------------------------------
# Write out the data arrays into the file
# ---------------------------------------
grids_file.variables['atmt.lon'][:,:] = lon_MNH[:,:]
grids_file.variables['atmt.lat'][:,:] = lat_MNH[:,:]
grids_file.variables['atmt.clo'][:,:] = clo_MNH[:,:,:]
grids_file.variables['atmt.cla'][:,:] = cla_MNH[:,:,:]

grids_file.variables['wavt.lon'][:,:] = lon_WW3[:,:]
grids_file.variables['wavt.lat'][:,:] = lat_WW3[:,:]
grids_file.variables['wavt.clo'][:,:] = clo_WW3[:,:,:]
grids_file.variables['wavt.cla'][:,:] = cla_WW3[:,:,:]

# ---------------------------------------
# close the file
# ---------------------------------------
grids_file.close()

print(' Closing netcdf file : grids.nc')
print('------------------------------------------')
##################################################

##################################################
print('------------------------------------------')
print(' Creating netcdf file : areas.nc')

areas_file=netCDF4.Dataset(curdir_path+'areas.nc','w',format='NETCDF3_64BIT')
areas_file.Description='Grid file for OASIS coupling'

# ----------------------------------
# Create the dimensions of the files
# ----------------------------------
areas_file.createDimension ('x_atmt', nlon_MNH)
areas_file.createDimension ('y_atmt', nlat_MNH)
areas_file.createDimension ('x_wavt', nlon_WW3)
areas_file.createDimension ('y_wavt', nlat_WW3)

# ----------------------------------
# Create the variables of the files
# ----------------------------------
varout=areas_file.createVariable('atmt.srf','d',('y_atmt','x_atmt'))
varout=areas_file.createVariable('wavt.srf','d',('y_wavt','x_wavt'))

# ---------------------------------------
# Write out the data arrays into the file
# ---------------------------------------
areas_file.variables['atmt.srf'][:,:] = area_MNH[:,:]
areas_file.variables['wavt.srf'][:,:] = area_WW3[:,:]

# ---------------------------------------
# close the file
# ---------------------------------------
areas_file.close()

print(' Closing netcdf file : areas.nc')
print('-----------------------------------------')
####################################################

##################################################
print('------------------------------------------')
print(' Creating netcdf file : masks.nc')

masks_file=netCDF4.Dataset(curdir_path+'masks.nc','w',format='NETCDF3_64BIT')
masks_file.Description='Grid file for OASIS coupling'

# ----------------------------------
# Create the dimensions of the files
# ----------------------------------
masks_file.createDimension ('x_atmt', nlon_MNH)
masks_file.createDimension ('y_atmt', nlat_MNH)
masks_file.createDimension ('x_wavt', nlon_WW3)
masks_file.createDimension ('y_wavt', nlat_WW3)

# ----------------------------------
# Create the variables of the files
# ----------------------------------
varout=masks_file.createVariable('atmt.msk','i',('y_atmt','x_atmt'))
varout=masks_file.createVariable('wavt.msk','i',('y_wavt','x_wavt'))

# ---------------------------------------
# Write out the data arrays into the file
# ---------------------------------------
masks_file.variables['atmt.msk'][:,:] = mask_MNH  [:,:]
masks_file.variables['wavt.msk'][:,:] = mask_WW3  [:,:]

# ---------------------------------------
# close the file
# ---------------------------------------
masks_file.close()

print(' Closing netcdf file : masks.nc')
print('-----------------------------------------')
