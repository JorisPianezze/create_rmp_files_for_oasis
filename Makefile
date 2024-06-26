#
ifndef OASIS_INC_DIR
$(error Please set OASIS_INC_DIR environment variable)
endif
$(echo You chose to compile with ${OASIS_INC_DIR})
#
ifndef NETCDF_CONFIG
$(error Please set NETCDF_CONFIG environment variable)
endif
#
$(info ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~)
$(info You will compile with :                      )
$(info - OASIS version  : ${OASIS_INC_DIR}          )
$(info - NETCDF version : ${NETCDF_CONFIG}          )
$(info ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~)
#
include header_Makefile
LIBPSMILE = $(ARCHDIR)/lib/libpsmile.${CHAN}.a $(ARCHDIR)/lib/libmct.a $(ARCHDIR)/lib/libmpeu.a $(ARCHDIR)/lib/libscrip.a
#
##### User configurable options #####
#
# CPP keys for model 1 (_M1) and for model 2 (_M2)
# type of decomposition :
# DECOMP_APPLE for 1D decomposition
# DECOMP_BOX for 2D decomposition
CPPKEYDECOMP_M1=DECOMP_APPLE #DECOMP_BOX
CPPKEYDECOMP_M2=DECOMP_APPLE
#
### End User configurable options ###
#
OBJ_M1 =  routine_hdlerr.o read_all_data.o \
          decomp_def.o gradient_bicubic.o \
          function_ana.o distance_rad.o \
          gradient_conserv.o
OBJ_M2 =  routine_hdlerr.o read_all_data.o \
          decomp_def_m2.o function_ana.o \
          write_all_fields.o
#-------------------------------------------------------------------------------
# General rules
#-------------------------------------------------------------------------------
#
default: all
#
all: model1 model2
#
#-------------------------------------------------------------------------------
# Rules for executables
#-------------------------------------------------------------------------------
#
model1: $(OBJ_M1) model1.o $(LIBPSMILE) Makefile
	$(LD) $(LDFLAGS) -o $@ $(OBJ_M1) model1.o $(LIBPSMILE) $(FLIBS)
model2: $(OBJ_M2) model2.o $(LIBPSMILE) Makefile
	$(LD) $(LDFLAGS) -o $@ $(OBJ_M2) model2.o $(LIBPSMILE) $(FLIBS)
#
#-------------------------------------------------------------------------------
# Rules for compilation
#-------------------------------------------------------------------------------
#
routine_hdlerr.o :		routine_hdlerr.f90
				$(F90) $(F90FLAGS) -c routine_hdlerr.f90
read_all_data.o :		read_all_data.F90
				$(F90) $(F90FLAGS) -c read_all_data.F90
write_all_fields.o :		write_all_fields.F90
				$(F90) $(F90FLAGS) -c write_all_fields.F90
function_ana.o :		function_ana.f90
				$(F90) $(F90FLAGS) -c function_ana.f90
decomp_def.o : 			decomp_def.F90
				$(F90) $(F90FLAGS) -D${CPPKEYDECOMP_M1} -c decomp_def.F90
decomp_def_m2.o : 		decomp_def.F90
				$(F90) $(F90FLAGS) -D${CPPKEYDECOMP_M2} -o decomp_def_m2.o -c decomp_def.F90
gradient_bicubic.o :	gradient_bicubic.f90
				$(F90) $(F90FLAGS) -c gradient_bicubic.f90
distance_rad.o :		distance_rad.f90
				$(F90) $(F90FLAGS) -c distance_rad.f90
gradient_conserv.o :		gradient_conserv.f90
				$(F90) $(F90FLAGS) -c gradient_conserv.f90

model1.o :	model1.F90 Makefile
		$(F90) $(F90FLAGS) -D${CPPKEYDECOMP_M1} -c model1.F90
model2.o :	model2.f90 Makefile
		$(F90) $(F90FLAGS) -D${CPPKEYDECOMP_M2} -c model2.f90
#
#-------------------------------------------------------------------------------
# Utilities
#-------------------------------------------------------------------------------
#
help:
	more Make.help
#
# Clean directory
#
clean:
	-rm -f *.o *.mod *.MOD model1 model2
	-rm -f i.*.F90 *.L
	-rm -f core core.* 
