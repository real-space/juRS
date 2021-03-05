###########################################################################################
#######                                                        ############################
#######  Makefile for juRS  v15.12                             ############################
#######  ---------------------------                           ############################
#######  Juelich Real-Space project                            ############################
#######                                                        ############################
###########################################################################################
#######  Paul F. Baumeister  28. Feb 2014                      ############################
#######  Shigeru Tsukamoto   23. Dec 2015                      ############################
###########################################################################################
OPENMP ?= TRUE
include ./system.make
#include ./system.make.MACHINE

### the system dependent include file must define the following variables
#
#CC             ### C Compiler
#CFLAGS         ### C Compiler options
#
#CPP            ### C-preprocessor
#CPPOUT         ### its output syntax
#CPPFLAGS       ### system dependent proprocessing flags
#
#FC             ### Fortran Compiler
#FCFLAGS        ### Fortran Compiler options
#
#LDFLAGS        ### Linker options
#LIBS           ### Libraries for linking
#

### name of the execution file
EXECUTABLE = paw

### directory containing source files
SRCDIR = src

### durectory containing header files
INCDIR = src

### directory for temporary preprocessed files
CPPDIR = prp

### use the MPI compiler specified in ./system.make
ifdef MPIFC
    FC = $(MPIFC)
endif
ifdef CFLAGS_OPT
	FCFLAGS = $(CFLAGS_OPT)
#  	FCFLAGS = $(CFLAGS_DEV)
endif

### the Linker is usually the same as the Compiler
LD = $(FC)

### add some preprocessor flags if necessary
# CPPFLAGS += -D NaN_SEARCH
# CPPFLAGS += -D DEBUG_ALL
# CPPFLAGS += -D DEBUG_GGA
CPPFLAGS += -D USE_SYMMETRY
CPPFLAGS += -D CONSTR=4


##################################################################
### define all objects
##################################################################

MODULES = \
mod_type_item.o \
mod_type_rgrid.o \
mod_constants.o \
mod_MPIconst.o \
mod_configuration.o \
mod_sorting.o \
mod_type_criteria.o \
mod_type_spline.o \
mod_GGA_tools.o \
mod_type_info.o \
mod_radial_interpolation.o \
mod_type_comp_desc.o \
mod_type_bfun.o \
mod_type_element.o \
mod_unitsystem.o \
mod_LAPACK.o \
mod_density_functionals.o \
mod_MPItools.o \
mod_type_grid.o \
mod_type_kpoint.o \
mod_harmonics.o  \
mod_toolbox.o \
mod_paw_XMLwriter.o \
mod_pawdatafile.o \
mod_type_state.o \
mod_communicators.o \
mod_atomicomm.o \
mod_spherical.o \
mod_boundary.o \
mod_input.o \
mod_control.o \
mod_type_proj.o \
mod_ScaLAPACK.o \
mod_broyden_second.o \
mod_FermiDirac.o \
mod_interpolation.o \
mod_Laplacian.o \
mod_type_species.o  \
mod_type_comp.o \
mod_Poissonsolver.o \
mod_paw_XMLfile.o \
mod_type_atom.o \
mod_pawdata.o \
mod_symmetry.o \
mod_analysis.o \
mod_operators.o \
mod_chemistry.o \
mod_display.o \
mod_dynamics.o \
mod_mixing.o \
mod_inout.o \
mod_radial_Hartree.o \
mod_diis_eigensolver.o \
mod_dav_eigensolver.o \
mod_cg_eigensolver.o \
mod_subspace.o \
mod_prepare.o \
mod_forces.o \
mod_init.o \
mod_selfcon.o \


ifeq ($(EXTENDED),TRUE)
	### for EXTENDED=TRUE make sure that your source directory contains the corresponding source files
	### otherwise set EXTENDED=FALSE above!
	MODULES += \
		mod_FFT_tools.o \
		mod_grace.o \
		mod_debugtools.o \
		mod_type_llist.o \
		mod_radial_integrator.o \
		mod_LebedevLaikov.o \
		mod_radial_potential.o \
		mod_all_electron.o \
		mod_relaxedcore.o \
		mod_cylindrical.o \
		mod_Gaussian_overlap.o \
		mod_constraints.o \
		mod_sd_eigensolver.o \
		mod_bandstructure.o 
	### this preprocessor flag makes effective changes to the code
	CPPFLAGS += -D EXTENDED 
endif

ifeq ($(CC),)
	## no c-compiler
else
	COBJECTS += pawxmlreader.o
	MODULES += mod_pawxmlreader.o
	CPPFLAGS += -D USE_PAWXMLREADER
	### add XLM parser library
	LIBS += -lexpat
endif

### add project-external routines here
ROUTINES  =

MAIN      = paw_main.o



### gather all objects into $(OBJECTS)
OBJECTS   := $(CONSTANTS) $(MODULES) $(ROUTINES) $(COBJECTS) $(MAIN)
##################################################################


##################################################################
### define search path for each file type ( .h .f90 .F90 .F95 .c )
##################################################################
vpath %.h   $(SRCDIR)
vpath %.f90 $(SRCDIR)
vpath %.F90 $(SRCDIR)
vpath %.F95 $(SRCDIR)
vpath %.c   $(SRCDIR)/c
##################################################################


##################################################################
### define all src files
##################################################################

SRC = $(foreach file, $(OBJECTS), $(foreach suffix, .f90 .F90 .F95, $(wildcard $(SRCDIR)/$(file:%.o=%$(suffix)))))
SRC += $(foreach file, $(OBJECTS), $(foreach suffix, .c, $(wildcard $(SRCDIR)/c/$(file:%.o=%$(suffix)))))

##################################################################


###############################
## PATTERNS  ##################
###############################
# the pattern how to create .o-object out of a .F90-source file

%.o: %.c
	$(CC) -I$(INCDIR) $(CFLAGS) -c $<


### simply compile regular .f90-source files
%.o: %.f90
	$(FC) -I$(INCDIR) $(FCFLAGS) -c $<

### proprocess .F90-source files to .f90 source files and compile
%.o: %.F90
	$(CPP) -I$(INCDIR) $(CPPFLAGS) $< $(CPPOUT) $(CPPDIR)/$*.f90
	$(FC) $(FCFLAGS) -c $(CPPDIR)/$*.f90

### DoublePreprocessing: .F95-files will be preprocessed twice,
### first with R1_C2 == 1, and then with R1_C2 == 2.
### Finally the two parts are concatenated to one .f90-source file.
%.o: %.F95
	$(CPP) -I$(INCDIR) $(CPPFLAGS) -D R1_C2=1 $< $(CPPOUT) $(CPPDIR)/$*.f91
	$(CPP) -I$(INCDIR) $(CPPFLAGS) -D R1_C2=2 $< $(CPPOUT) $(CPPDIR)/$*.f92
	cat $(CPPDIR)/$*.f91 $(CPPDIR)/$*.f92 > $(CPPDIR)/$*.f90
	$(FC) $(FCFLAGS) -c $(CPPDIR)/$*.f90
	rm -f $(CPPDIR)/$*.f91 $(CPPDIR)/$*.f92

###############################
### TARGETS   #################
###############################
### first target (is taken when calling make without a target argument)

$(EXECUTABLE): $(OBJECTS)
	$(LD)  $(LDFLAGS) $^ $(LIBS)  -o $(EXECUTABLE)

### open all source files using kate from the KDEwrite packet
edit:
	rm -f *~
	kate TODO system.make Makefile $(SRCDIR)/config.h $(SRCDIR)/*.f90 $(SRCDIR)/*.F90 $(SRCDIR)/*.F95 &

### remove exec
clean_all: clean
	rm -f $(EXECUTABLE)
	rm -f mod.deps
	touch mod.deps

### remove old object files, modules, Preprocessor-temp-dir
clean:
	rm -f -r $(CPPDIR)
	mkdir $(CPPDIR)
	rm -f *.o *.mod *.lst
	rm -f $(SRCDIR)/*~
	rm -f *~ fort.* 

backup:
	rm -f $(SRCDIR)/*~
	tar -czvf backup.tgz $(INCDIR)/*.h $(SRCDIR)/c/* $(SRCDIR)/*.f90 $(SRCDIR)/*.F90 $(SRCDIR)/*.F95 doc/manual.tex Makefile system.make.* 

.PHONY: deps backup clean clean_all edit

deps: $(SRC)
	./create_deps.sh $^ > mod.deps

mod.deps: ;

include mod.deps
