# CPT Makefile
INSTALL_DIR=${HOME}/bin
LIB_DIR=${HOME}/lib/
MAJOR_VERSION=11
MINOR_VERSION=03
SHARED_LIB=0
FORTRAN_ONLY=1

ifeq ($(FORTRAN_ONLY),1)

  ONLY_FORTRAN=-DONLY_FORTRAN

else

fortran_c_mix_interface=fortran_c_mix_interface.f90
fortran_c_wrapper=
data_input_c_mix=data_input_c_mix.f90
data_output_c_wrapper=data_output_c_wrapper.f90
fortran_c_mix_interface_obj=fortran_c_mix_interface.o
data_input_c_mix_obj=data_input_c_mix.o
data_output_c_wrapper_obj=data_output_c_wrapper.o
fortran_c_wrapper_obj=

IO_CLIB=-L./dataio_c -ldataio_c

endif

# User should change the "FC" to reflect the compiler location on your machine.  
#
# FC=gfortran 
# 
FC=/usr/bin/f95
#FCFLAGS= -g -Wall ${ONLY_FORTRAN} -DGFORTRAN
FCFLAGS= -O ${ONLY_FORTRAN} -DGFORTRAN

#
# ifort
# 
#FC=ifort
#FCFLAGS=-debug all -warn all -g -DONLY_FORTRAN -DIFORT -heap-arrays -assume minus0 -fpe0  -fPIC

#
# NAG f95
# -f95 is default -ieee=stop is default
#FC= nagfor
#FCFLAGS=-C -DONLY_FORTRAN -PIC -DNAG

#
# pgf90
#
#FC=pgf90
#FCFLAGS=-O -Kieee -DONLY_FORTRAN -DPGI

#
# pgf95
#
#FC=pgf95
#FCFLAGS=-O -Kieee -DONLY_FORTRAN -DPGI

FCINCLUDES=-I./LAPACK -I./CPT_Data -I./

SRCS= \
labels.f90\
gui.f90\
errors.f90\
arrays.f90\
distribs.f90\
settings.f90\
iofiles.f90\
fields.f90\
analysis.f90\
statistics.f90\
categories.f90\
missing.f90\
bootstrap.f90\
cca.f90\
pcs.f90\
${fortran_c_mix_interface}\
get_input_file.F90\
data_input.F90\
${data_input_c_mix} \
data_output.F90\
${data_output_c_wrapper}\
pcr.f90\
maps.f90\
scree.f90\
validate.f90\
verif.f90\
gcm.f90\
forecast.f90\
user.f90 \
tables.f90\
run_analysis.f90\
init_CPT.f90\
CPT.f90

OBJS=\
labels.o\
gui.o\
errors.o\
arrays.o\
distribs.o\
iofiles.o\
settings.o\
fields.o\
analysis.o\
statistics.o\
categories.o\
missing.o\
pcs.o\
${fortran_c_mix_interface_obj}\
get_input_file.o\
data_input.o\
${data_input_c_mix_obj} \
data_output.o\
${data_output_c_wrapper_obj}\
bootstrap.o\
cca.o\
pcr.o\
maps.o\
scree.o\
validate.o\
verif.o\
gcm.o\
forecast.o\
user.o\
tables.o\
run_analysis.o\
init_CPT.o


LIBS= ${IO_CLIB} -L./CPT_Data/ -lcptutil -lcptdata -L./LAPACK -llapack

.SUFFIXES: .f90 .mod .F90
.f90.o:
		${FC} -c $< ${FCFLAGS} ${FCINCLUDES}

.F90.o:
		${FC} -c $< ${FCFLAGS} ${FCINCLUDES}

.mod.o:
		${FC} -c $*.f90 ${FCFLAGS} ${FCINCLUDES}

CPT.x:${OBJS} CPT.o ${fortran_c_wrapper_obj}
		${FC} -o $@ CPT.o ${OBJS} ${fortran_c_wrapper_obj} ${FCFLAGS} ${LIBS}

makes_labels.x:${OBJS} makes_labels.o ${fortran_c_wrapper_obj}
		${FC} -o $@ makes_labels.o ${OBJS} ${fortran_c_wrapper_obj} ${FCFLAGS} ${LIBS}

testDGESDD:${OBJS} testDGESDD.o ${fortran_c_wrapper_obj}
		${FC} -o $@ testDGESDD.o ${OBJS} ${fortran_c_wrapper_obj} ${FCFLAGS} ${LIBS}

libcptcore.a : ${OBJS}
		${AR} -cr $@ ${OBJS}
		ranlib $@

all:
	cd ./LAPACK; make; cd ../;
	cd ./CPT_Data; make all; cd ..;
ifeq ($(FORTRAN_ONLY),0)
	cd dataio_c; make; cd ..;
	make libcptcore.a
endif
	make CPT.x;

tests:
	cd test; make all; cd ${OLDPWD};

release:
	/bin/rm -fR ../CPT; mkdir -p ../CPT;
	mkdir -p ../CPT/${MAJOR_VERSION}.${MINOR_VERSION};
	mkdir -p ../CPT/${MAJOR_VERSION}.${MINOR_VERSION}/LAPACK;
	mkdir -p ../CPT/${MAJOR_VERSION}.${MINOR_VERSION}/CPT_Data;
	mkdir -p ../CPT/${MAJOR_VERSION}.${MINOR_VERSION}/LABELS;
	#mkdir -p ../CPT/${MAJOR_VERSION}.${MINOR_VERSION}/dataio_c;
	mkdir -p ../CPT/${MAJOR_VERSION}.${MINOR_VERSION}/DATA;
	cp ./LAPACK/*.f* ../CPT/${MAJOR_VERSION}.${MINOR_VERSION}/LAPACK; cp LAPACK/Makefile ../CPT/${MAJOR_VERSION}.${MINOR_VERSION}/LAPACK;
	cp ./CPT_Data/*.f* ../CPT/${MAJOR_VERSION}.${MINOR_VERSION}/CPT_Data; cp CPT_Data/Makefile ../CPT/${MAJOR_VERSION}.${MINOR_VERSION}/CPT_Data;
	#cp ./dataio_c/*.[ch] ../CPT/${MAJOR_VERSION}.${MINOR_VERSION}/dataio_c; cp dataio_c/Makefile ../CPT/${MAJOR_VERSION}.${MINOR_VERSION}/dataio_c;
	cp V10Data/cptv10*.tsv ../CPT/${MAJOR_VERSION}.${MINOR_VERSION}/DATA;
	cp LABELS/*.dat ../CPT/${MAJOR_VERSION}.${MINOR_VERSION}/LABELS;
	cp *.f90 ../CPT/${MAJOR_VERSION}.${MINOR_VERSION}; cp Makefile ../CPT/${MAJOR_VERSION}.${MINOR_VERSION}; cp revisions.txt ../CPT/${MAJOR_VERSION}.${MINOR_VERSION};cp CPT_disclaimer.txt ../CPT/${MAJOR_VERSION}.${MINOR_VERSION};
	cp CPT.ini ../CPT/${MAJOR_VERSION}.${MINOR_VERSION};
	cp *.F90 ../CPT/${MAJOR_VERSION}.${MINOR_VERSION};
	rm ../CPT/${MAJOR_VERSION}.${MINOR_VERSION}/data_input_core.F90;
	rm ../CPT/${MAJOR_VERSION}.${MINOR_VERSION}/set_variables.f90;
	rm ../CPT/${MAJOR_VERSION}.${MINOR_VERSION}/run_analysis_core.f90;
	cd ../; tar -cvzf CPT.${MAJOR_VERSION}.${MINOR_VERSION}.tar.gz CPT; /bin/rm -fR CPT; cd NEW_CPT_SOURCE;

install:
	mkdir -p     ${INSTALL_DIR};
	cp CPT.x     ${INSTALL_DIR}; 
	cp CPT.ini   ${INSTALL_DIR};
	cp -R LABELS ${INSTALL_DIR};
ifeq ($(SHARED_LIB),1)
	mkdir -p     ${LIB_DIR};
endif

deepclean:
	cd LAPACK; make clean; cd ..;
	cd CPT_Data; make clean; cd ..;
ifeq ($(FORTRAN_ONLY),0)
	cd dataio_c; make clean; cd ..;
endif
#	cd test; make clean; cd ..;
#	/bin/rm -fR *.o *.mod CPT.x core* *.a *.so
	/bin/rm -fR *.o *.mod CPT.x core* *.a *.s cca_*.txt  pcr_*.txt mlr_*.txt gcm_*.txt *~

clean:
#		/bin/rm -fR *.o *.mod CPT.x core* *.a *.so
	/bin/rm -fR *.o *.mod CPT.x core* *.a *.so *.s cca_*.txt  pcr_*.txt mlr_*.txt gcm_*.txt *~

depend .depend:
		makedepf90  *.f90 >.depend
