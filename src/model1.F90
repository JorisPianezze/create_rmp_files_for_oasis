! #########################################################
PROGRAM model1
! #########################################################

! ---------------------------------------------------------
!
!             Model 1 to create rmp files
!
! --------------------------------------------------------

! ========================================================
!                List of modifications
!
!        2014-10 : CERFACS             - OASIS's tuto
!        2025-10 : J. Pianezze (LAERO) - Refactoring/Cleaning
!
! ========================================================

USE netcdf
USE mod_oasis
USE read_data

IMPLICIT NONE

INCLUDE 'mpif.h'

CHARACTER(len=30), PARAMETER   :: data_gridname='grids.nc' ! file with the grids
CHARACTER(len=30), PARAMETER   :: data_maskname='masks.nc' ! file with the masks

! Component name (6 characters) same as in the namcouple
CHARACTER(len=6)   :: comp_name = 'model1'
CHARACTER(len=128) :: comp_out       ! name of the output log file
CHARACTER(len=3)   :: chout
CHARACTER(len=4)   :: cl_grd_src     ! name of the source grid
CHARACTER(len=11)  :: cl_remap       ! type of remapping

NAMELIST /grid_source_characteristics/cl_grd_src
NAMELIST /grid_source_characteristics/cl_remap

! Global grid parameters : 
INTEGER :: nlon, nlat    ! dimensions in the 2 directions of space
INTEGER :: il_size
REAL, DIMENSION(:,:), POINTER  :: gg_lon,gg_lat ! lon, lat of the points
INTEGER, DIMENSION(:,:), POINTER         :: gg_mask ! mask, 0 == valid point, 1 == masked point 

INTEGER :: mype, npes ! rank and  number of pe
INTEGER :: localComm  ! local MPI communicator and Initialized
INTEGER :: comp_id    ! component identification

INTEGER, DIMENSION(:), ALLOCATABLE :: il_paral ! Decomposition for each proc

INTEGER :: ierror, rank, w_unit
INTEGER :: FILE_Debug=2

! Names of exchanged Fields
CHARACTER(len=8), PARAMETER :: var_name = 'FSENDANA' ! 8 characters field sent by model1

! Used in oasis_def_var and oasis_def_var
INTEGER                       :: var_id
INTEGER                       :: var_nodims(2) 
INTEGER                       :: var_type

! Grid parameters definition
INTEGER                       :: part_id  ! use to connect the partition to the variables 
INTEGER                       :: var_sh(4) ! local dimensions of the arrays; 2 x rank (=4)
INTEGER :: ibeg, iend, jbeg, jend

! Exchanged local fields arrays
REAL,   POINTER     :: field_send(:,:)

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  Initialization 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CALL oasis_init_comp (comp_id, comp_name, ierror )
IF (ierror /= 0) WRITE(0,*) 'oasis_init_comp abort by model1 compid ',comp_id
  
CALL MPI_Comm_Rank ( MPI_COMM_WORLD, rank, ierror )
IF (ierror /= 0) WRITE(0,*) 'MPI_Comm_Rank abort by model1 compid ',comp_id

CALL oasis_get_localcomm ( localComm, ierror )
IF (ierror /= 0) WRITE (0,*) 'oasis_get_localcomm abort by model1 compid ',comp_id
  
CALL MPI_Comm_Size ( localComm, npes, ierror )
IF (ierror /= 0) WRITE(0,*) 'MPI_comm_size abort by model1 compid ',comp_id

CALL MPI_Comm_Rank ( localComm, mype, ierror )
IF (ierror /= 0) WRITE (0,*) 'MPI_Comm_Rank abort by model1 compid ',comp_id

IF ((FILE_Debug == 1) .AND. (mype == 0)) FILE_Debug=2

IF (FILE_Debug <= 1) THEN
  IF (mype == 0) THEN
    w_unit = 103 + rank
    WRITE(chout,'(I3)') w_unit
    comp_out=comp_name//'.root_'//chout
    OPEN(w_unit,file=TRIM(comp_out),form='formatted')
  ELSE
    w_unit = 15
    comp_out=comp_name//'.notroot'
    OPEN(w_unit,file=TRIM(comp_out),form='formatted',position='append')
  ENDIF
ELSE
  w_unit = 103 + rank
  WRITE(chout,'(I3)') w_unit
  comp_out=comp_name//'.out_'//chout
  OPEN(w_unit,file=TRIM(comp_out),form='formatted')
ENDIF
 
IF (FILE_Debug >= 2) THEN
  WRITE(w_unit,*) '-----------------------------------------------------------'
  WRITE (w_unit,*) 'I am component ', TRIM(comp_name), ' global rank :',rank
  WRITE(w_unit,*) '----------------------------------------------------------'
  WRITE(w_unit,*) 'I am the ', TRIM(comp_name), ' ', 'component identifier', comp_id, 'local rank', mype
  WRITE (w_unit,*) 'Number of processors :',npes
  WRITE(w_unit,*) '----------------------------------------------------------'
  CALL FLUSH(w_unit)
ENDIF

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  Grid definition 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

OPEN(UNIT=70,FILE='name_grids.dat',FORM='FORMATTED',STATUS='OLD')
READ(UNIT=70,NML=grid_source_characteristics)
CLOSE(70)

IF (FILE_Debug >= 2) THEN
  WRITE(w_unit,*) 'Source grid name : ',cl_grd_src
  WRITE(w_unit,*) 'Remapping : ',cl_remap
  CALL flush(w_unit)
ENDIF
  
! Reading dimensions of the global grid
CALL read_dimgrid(nlon,nlat,data_gridname,cl_grd_src,w_unit,FILE_Debug)
  
! Allocate grid arrays
ALLOCATE(gg_lon(nlon,nlat), STAT=ierror )
ALLOCATE(gg_lat(nlon,nlat), STAT=ierror )
ALLOCATE(gg_mask(nlon,nlat), STAT=ierror )

! Read global grid longitudes, latitudes, corners, mask 
CALL read_grid(nlon,nlat, data_gridname, cl_grd_src, w_unit, FILE_Debug, gg_lon, gg_lat)
CALL read_mask(nlon,nlat, data_maskname, cl_grd_src, w_unit, FILE_Debug, gg_mask)

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  Partition definition 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

il_size = 5
ALLOCATE(il_paral(il_size))

CALL decomp_def (il_paral, il_size, nlon, nlat, mype, npes, w_unit)
CALL oasis_def_partition (part_id, il_paral, ierror)

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  Coupling field declaration  
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

var_nodims(1) = 2    ! Rank of the field array is 2
var_nodims(2) = 1    ! Bundles always 1 for OASIS3
var_type = OASIS_Real

var_sh(1) = 1
var_sh(2) = il_paral(3)
var_sh(3) = 1 
var_sh(4) = il_paral(4)

! Declaration of the field associated with the partition
CALL oasis_def_var (var_id, var_name, part_id, &
                    var_nodims, OASIS_Out, var_sh, var_type, ierror)
IF (ierror /= 0) WRITE(w_unit,*) 'oasis_def_var abort by model1 compid ',comp_id

DEALLOCATE(il_paral)

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Termination of definition phase 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CALL oasis_enddef ( ierror )
IF (ierror /= 0) WRITE(w_unit,*) 'oasis_enddef abort by model1 compid ',comp_id

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Send array
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ALLOCATE(field_send(nlon,nlat), STAT=ierror )
IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating field1_send'

CALL function_ana(nlon, nlat, gg_lon, gg_lat, field_send)

ibeg=1 ; iend=nlon
jbeg=((nlat/npes)*mype)+1 

IF (mype .LT. npes - 1) THEN
  jend = (nlat/npes)*(mype+1)
ELSE
  jend = nlat 
ENDIF

CALL oasis_put(var_id, 0, &
               RESHAPE(field_send(ibeg:iend,jbeg:jend),(/var_sh(2),var_sh(4)/)), &
               ierror )

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Termination 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CALL oasis_terminate (ierror)
IF (ierror /= 0) WRITE(w_unit,*) 'oasis_terminate abort by model1 compid ',comp_id

! #########################################################
END PROGRAM MODEL1
! #########################################################
