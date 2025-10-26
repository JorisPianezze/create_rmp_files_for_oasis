#
ifndef OASISDIR
$(error Please set OASISDIR environment variable)
endif
$(echo You chose to compile with ${OASISDIR})
#
ifndef NETCDF_CONFIG
$(error Please set NETCDF_CONFIG environment variable)
endif
#
$(info ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~)
$(info You will compile with :                      )
$(info - OASIS version  : ${OASISDIR}               )
$(info - NETCDF version : ${NETCDF_CONFIG}          )
$(info ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~)
#
include header_Makefile
LIBPSMILE = $(ARCHDIR)/lib/libpsmile.${CHAN}.a $(ARCHDIR)/lib/libmct.a $(ARCHDIR)/lib/libmpeu.a $(ARCHDIR)/lib/libscrip.a
#
OBJ_M1 = handle_netcdf_errors.o read_data.o decomp_def.o function_ana.o
OBJ_M2 = handle_netcdf_errors.o read_data.o decomp_def.o function_ana.o write_field.o

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
handle_netcdf_errors.o: 	handle_netcdf_errors.F90
				$(F90) -Wall $(F90FLAGS) -c handle_netcdf_errors.F90
read_data.o:			read_data.F90
				$(F90) -Wall $(F90FLAGS) -c read_data.F90
write_field.o:			write_field.F90
				$(F90) -Wall $(F90FLAGS) -c write_field.F90
function_ana.o:			function_ana.F90
				$(F90) -Wall $(F90FLAGS) -c function_ana.F90
decomp_def.o:			decomp_def.F90
				$(F90) -Wall $(F90FLAGS) -c decomp_def.F90

model1.o :	model1.F90 Makefile
		$(F90) -Wall $(F90FLAGS) -c model1.F90
model2.o :	model2.F90 Makefile
		$(F90) -Wall $(F90FLAGS) -c model2.F90
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
