! #########################################################
MODULE read_data
! #########################################################
  
USE netcdf
IMPLICIT NONE
  
CONTAINS

! #########################################################
SUBROUTINE read_dimgrid (nlon,nlat,data_filename,cl_grd,w_unit,FILE_Debug)
! #########################################################

! ---------------------------------------------------------
!
!   Read grid dimensions (nlon, nlat)
!
! --------------------------------------------------------

USE netcdf
IMPLICIT NONE
 
CHARACTER(len=30), INTENT(IN   ) :: data_filename
CHARACTER(len=4),  INTENT(IN   ) :: cl_grd
INTEGER,           INTENT(IN   ) :: w_unit
INTEGER,           INTENT(IN   ) :: FILE_Debug
INTEGER,           INTENT(  OUT) :: nlon, nlat
  
INTEGER          :: file_id, nlon_id, nlat_id
CHARACTER(len=8) :: cl_nam
  
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Open netcdf file
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( NF90_OPEN(data_filename, NF90_NOWRITE, file_id), __LINE__, __FILE__)

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Read grid dim id.
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cl_nam="x_"//cl_grd
CALL handle_netcdf_errors( NF90_INQ_DIMID(file_id, cl_nam, nlon_id), __LINE__, __FILE__ )
cl_nam="y_"//cl_grd
CALL handle_netcdf_errors( NF90_INQ_DIMID(file_id, cl_nam, nlat_id), __LINE__, __FILE__ )

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Read grid dim
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( NF90_INQUIRE_DIMENSION(file_id, nlon_id, len=nlon), __LINE__, __FILE__ )
CALL handle_netcdf_errors( NF90_INQUIRE_DIMENSION(file_id, nlat_id, len=nlat), __LINE__, __FILE__ )

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Close netcdf file
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors(NF90_CLOSE(file_id), __LINE__, __FILE__ )

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Print for DEBUG
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IF (FILE_Debug >= 2) THEN
  WRITE(w_unit,*) '-- read_dimgrid'
  WRITE(w_unit,*) 'Data_filename :',data_filename
  WRITE(w_unit,*) 'Longitudes :',cl_nam
  WRITE(w_unit,*) 'Latitudes  :',cl_nam   
  WRITE(w_unit,*) 'Reading input file ',data_filename
  WRITE(w_unit,*) 'Global dimensions nlon=',nlon,' nlat=',nlat         
  CALL FLUSH(w_unit)
ENDIF

END SUBROUTINE read_dimgrid

! #########################################################
SUBROUTINE read_grid (nlon,nlat,data_filename,cl_grd, &
                      w_unit,FILE_Debug,lon,lat       )
! #########################################################

! ---------------------------------------------------------
!
!        Read grid (lon, lat)
!
! --------------------------------------------------------
   
INTEGER,                       INTENT(IN   ) :: nlon, nlat     
CHARACTER(LEN=30),             INTENT(IN   ) :: data_filename
CHARACTER(len=4) ,             INTENT(IN   ) :: cl_grd
INTEGER,                       INTENT(IN   ) :: w_unit, FILE_Debug
REAL,    DIMENSION(nlon,nlat), INTENT(INOUT) :: lon, lat

INTEGER          :: file_id, lon_id, lat_id
CHARACTER(len=8) :: cl_nam

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Open netcdf file
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors(NF90_OPEN(data_filename, NF90_NOWRITE, file_id), __LINE__, __FILE__ )

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Read grid id.
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cl_nam=cl_grd//".lon"
CALL handle_netcdf_errors(NF90_INQ_VARID(file_id, cl_nam,  lon_id), __LINE__, __FILE__ )
cl_nam=cl_grd//".lat" 
CALL handle_netcdf_errors(NF90_INQ_VARID(file_id, cl_nam,  lat_id), __LINE__, __FILE__ )

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Read grid data
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( NF90_GET_VAR(file_id, lon_id, lon), __LINE__, __FILE__ )
CALL handle_netcdf_errors( NF90_GET_VAR(file_id, lat_id, lat), __LINE__, __FILE__ )

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Close netcdf file
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( NF90_CLOSE(file_id), __LINE__, __FILE__ )

IF (FILE_Debug >= 2) THEN
  WRITE(w_unit,*) 'End of routine read_grid'
  CALL FLUSH(w_unit)
ENDIF
  
END SUBROUTINE read_grid

! #########################################################
SUBROUTINE read_mask (nlon,nlat,data_filename,cl_grd, &
                      w_unit, FILE_Debug,mask         )
! #########################################################

! ---------------------------------------------------------
!
!        Read mask (msk)
!
! --------------------------------------------------------
   
INTEGER,                       INTENT(IN   ) :: nlon, nlat     
CHARACTER(LEN=30),             INTENT(IN   ) :: data_filename
CHARACTER(len=4) ,             INTENT(IN   ) :: cl_grd
INTEGER,                       INTENT(IN   ) :: w_unit, FILE_Debug
INTEGER, DIMENSION(nlon,nlat), INTENT(INOUT) :: mask

INTEGER          :: file_id, mask_id
CHARACTER(len=8) :: cl_nam

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Open netcdf file
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors(NF90_OPEN(data_filename, NF90_NOWRITE, file_id), __LINE__, __FILE__ )

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Read grid id.
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cl_nam=cl_grd//".msk"
CALL handle_netcdf_errors(NF90_INQ_VARID(file_id, cl_nam,  mask_id), __LINE__, __FILE__ )

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Read grid data
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( NF90_GET_VAR(file_id, mask_id, mask), __LINE__, __FILE__ )

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Close netcdf file
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( NF90_CLOSE(file_id), __LINE__, __FILE__ )

IF (FILE_Debug >= 2) THEN
  WRITE(w_unit,*) 'End of routine read_mask'
  CALL FLUSH(w_unit)
ENDIF

END SUBROUTINE read_mask

! #########################################################
END MODULE read_data
! #########################################################
