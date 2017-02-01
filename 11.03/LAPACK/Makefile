# LAPACK makefile

#
# GNU compiler
#
FC=gfortran
FCFLAG=-g
FCFLAGS=-g

#FCFLAG=-g -fdefault-real-8 -fdefault-double-8
#FCFLAGS=-g -fdefault-real-8 -fdefault-double-8

#
#   ifort
#FC=ifort
#FCFLAGS=-g -assume minus0 -fixed -fpe0 
#FCFLAGS=-g -check -assume minus0 -fixed -fpe0 -prec_div

#
# NAG f95 
#
#FC=nagfor
# -dusty: Downgrades a number of common errors found in legacy code
# -mismatch: Downgrades errors resulting from mismatch in argument lists to
#  warnings. Calls to routines in the same file must still be correct. 
#  -ieee=stop is default
#FCFLAGS=-O -ieee=stop -dusty -mismatch -w
#FCFLAGS=-O -dusty -mismatch -w

#
#   pgf90
#
#FC=pgf90
#FCFLAG=-g -Kieee
#FCFLAGS=-g -Kieee


#
#   pgf95
#
#FC=pgf95
#FCFLAGS=-O -Kieee

FCINCLUDES=-I./
SRCS= dbdsdc.f dbdsqr.f dcopy.f ddot.f dgebd2.f\
dgebrd.f dgelq2.f dgelqf.f dgemm.f dgemv.f\
dgeqr2.f dgeqrf.f dger.f dgesdd.f disnan.f\
dlabrd.f dlacpy.f dlaed6.f dlaisnan.f dlamch.f\
dlamrg.f dlange.f dlanst.f dlapy2.f dlarf.f\
dlarfb.f dlarfg.f dlarft.f dlartg.f dlas2.f\
dlascl.f dlasd0.f dlasd1.f dlasd2.f dlasd3.f\
dlasd4.f dlasd5.f dlasd6.f dlasd7.f dlasd8.f\
dlasda.f dlasdq.f dlasdt.f dlaset.f dlasq1.f\
dlasq2.f dlasq3.f dlasq4.f dlasq5.f dlasq6.f\
dlasr.f dlasrt.f dlassq.f dlasv2.f\
dnrm2.f dorg2r.f dorgbr.f dorgl2.f\
dorglq.f dorgqr.f dorm2r.f dormbr.f dorml2.f\
dormlq.f dormqr.f drot.f dscal.f dswap.f\
dtrmm.f dtrmv.f ieeeck.f iladlc.f iladlr.f\
ilaenv.f iparmq.f lsame.f xerbla.f


INSTALL_LIB_DIR = ../../lib
OBJS=${SRCS:.f=.o}

.SUFFIXES: .f.f90
.f.o:
		${FC} -c $< ${FCFLAGS}

.f90.o:
		${FC} -c $< ${FCFLAGS}

liblapack.a : ${OBJS}
		${FC} -c lapack.f90 ${FCFLAGS}
		${AR} -cr $@ ${OBJS} lapack.o
		ranlib $@ 
		
clean:
	/bin/rm -fR *.o *.a *.mod *.s

#	cp *.so  ${INSTALL_LIB_DIR}
install:
	cp *.a  ${INSTALL_LIB_DIR}
	ranlib ${INSTALL_LIB_DIR}/liblapack.a
