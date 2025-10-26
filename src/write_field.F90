! #########################################################
SUBROUTINE write_field (nlon,nlat,data_filename,field_name, &
                        w_unit,FILE_Debug,lon,lat,array     )
! #########################################################

! ---------------------------------------------------------
!
!        Write field (array)
!
! --------------------------------------------------------

USE netcdf
IMPLICIT NONE

INTEGER,                    INTENT(IN   ) :: nlon, nlat     
CHARACTER(LEN=30),          INTENT(IN   ) :: data_filename
CHARACTER(LEN=30),          INTENT(IN   ) :: field_name
INTEGER,                    INTENT(IN   ) :: w_unit, FILE_Debug
REAL, DIMENSION(nlon,nlat), INTENT(INOUT) :: lon, lat, array

INTEGER :: file_id, array_id
INTEGER :: lon_id, lat_id 
INTEGER :: nlon_id, nlat_id
        
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Open netcdf file
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( NF90_CREATE(data_filename, NF90_CLOBBER, file_id), __LINE__, __FILE__ )

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Define dimensions
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( NF90_DEF_DIM(file_id, "lon", nlon, nlon_id), __LINE__, __FILE__ )
CALL handle_netcdf_errors( NF90_DEF_DIM(file_id, "lat", nlat, nlat_id), __LINE__, __FILE__ )

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Define variables
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( NF90_DEF_VAR(file_id, "lon", NF90_DOUBLE, (/nlon_id, nlat_id/), lon_id), &
                           __LINE__, __FILE__ )
CALL handle_netcdf_errors( NF90_DEF_VAR(file_id, "lat", NF90_DOUBLE, (/nlon_id, nlat_id/), lat_id), &
                           __LINE__, __FILE__ )
CALL handle_netcdf_errors( NF90_DEF_VAR(file_id, TRIM(field_name), NF90_DOUBLE, (/nlon_id, nlat_id/), array_id), &
                           __LINE__, __FILE__ )
                             
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Define attributes
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( NF90_PUT_ATT(file_id, lon_id, "units", "degrees_east"),      __LINE__, __FILE__ )
CALL handle_netcdf_errors( NF90_PUT_ATT(file_id, lon_id, "standard_name", "longitude"), __LINE__, __FILE__ )
CALL handle_netcdf_errors( NF90_PUT_ATT(file_id, lat_id, "units", "degrees_north"),     __LINE__, __FILE__ )
CALL handle_netcdf_errors( NF90_PUT_ATT(file_id, lat_id, "standard_name", "latitude"),  __LINE__, __FILE__ )

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   End of definition
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( NF90_ENDDEF(file_id), __LINE__, __FILE__ )

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Write data
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( NF90_PUT_VAR (file_id, lon_id, lon),     __LINE__, __FILE__ )
CALL handle_netcdf_errors( NF90_PUT_VAR (file_id, lat_id, lat),     __LINE__, __FILE__ )
CALL handle_netcdf_errors( NF90_PUT_VAR (file_id, array_id, array), __LINE__, __FILE__ )
     
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Close netcdf file
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( NF90_CLOSE(file_id), __LINE__, __FILE__ )

IF (FILE_Debug >= 2) THEN
  WRITE(w_unit,*) 'End of routine write field'
  CALL FLUSH(w_unit)
ENDIF

END SUBROUTINE write_field
