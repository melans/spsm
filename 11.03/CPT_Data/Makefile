# CPT util makefile
FORTRAN_ONLY=1

ifeq (${FORTRAN_ONLY},0)
    time_c_wrapper=time_c_wrapper.f90
    make_coors_c=make_coors_c.f90
endif

# GNU compiler
FC=gfortran
#FCFLAGS=-g -DONLY_FORTRAN -Wall -Wconversion -Wimplicit-interface -fdefault-real-8 -fdefault-double-8
#FCFLAGS=-g -DONLY_FORTRAN -Wall -Wconversion -fdefault-real-8 -fdefault-double-8
FCFLAGS=-g -DONLY_FORTRAN -Wall -Wconversion
#FCFLAGS=-O -DONLY_FORTRAN

#
# ifort
# 
#FC=ifort
#FCFLAGS=-O -assume minus0 -fpe0 
#FCFLAGS=-debug all -warn all -g -heap-arrays -assume minus0 -fpe0  -fPIC -DONLY_FORTRAN


#
# NAG f95
# -f95 is default
#FC=nagfor
#FCFLAGS=-C -g -ieee=stop -DONLY_FORTRAN
#FCFLAGS=-C -DONLY_FORTRAN -PIC  #-ieee=stop is default

#
# pgf90
#
#FC=pgf90
#FCFLAGS=-O -Kieee -DONLY_FORTRAN

#
# pgf95
#
#FC=pgf95
#FCFLAGS=-O -Kieee -DONLY_FORTRAN

FCINCLUDES=-I../CPT_SOURCE/LAPACK -I./

CPTDATA_SRCS=\
numbers.f90\
version.f90\
gui_constants.f90\
IO_constants.f90\
CPT_constants.f90\
CPT_text.f90\
time_constants.f90

CPTDATA_OBJS=${CPTDATA_SRCS:.f90=.o}

CPTUTIL_SRCS=\
time.f90\
${time_c_wrapper}\
maths.f90\
make_coors.f90 ${make_coors_c}

CPTUTIL_OBJS=${CPTUTIL_SRCS:.f90=.o}

INSTALL_LIB_DIR = ../../lib

.SUFFIXES: .f90 .mod
.f90.o:
		${FC} -c $< ${FCFLAGS} ${FCINCLUDES}

.mod.o:
		${FC} -c $*.f90 ${FCFLAGS} ${FCINCLUDES}

libcptdata.so:${CPTDATA_OBJS}
		${FC} -shared -o $@ ${CPTDATA_OBJS}

libcptdata.a : ${CPTDATA_OBJS}
	${AR} -cr $@ ${CPTDATA_OBJS}
	ranlib $@

libcptutil.so:${CPTUTIL_OBJS}
		${FC} -shared -o $@ ${CPTUTIL_OBJS} -L ${CPTDATA_OBJS}

libcptutil.a : ${CPTUTIL_OBJS}
	${AR} -cr $@ ${CPTUTIL_OBJS}
	ranlib $@

all: ${CPTDATA_OBJS} ${CPTUTIL_OBJS}
	make libcptdata.a;
#	make libcptdata.so;
	make libcptutil.a;
#	make libcptutil.so;

clean:
		/bin/rm -fR *.o *.mod *.a *.so *.s

install:
#cp *.so  ${INSTALL_LIB_DIR}
	cp *.a  ${INSTALL_LIB_DIR}
	ranlib ${INSTALL_LIB_DIR}/libcptdata.a
	ranlib ${INSTALL_LIB_DIR}/libcptutil.a

depend .depend:
		makedepf90  *.f90 >.depend
