! $Id: version.f90 1249 2011-03-14 15:31:47Z lsong $
! Author: Simon Mason
MODULE version
!
! Modules
  USE numbers, ONLY: sp
!
! Implicit declarations
  IMPLICIT NONE
!
! Parameters
!
! Version
  REAL(KIND=sp), PARAMETER, PUBLIC :: ver=11.03 ! - version number -
!
! Integer parameters
  INTEGER, PARAMETER, PUBLIC :: lver=37 ! - length of window title -
!
! Character parameters
  CHARACTER(LEN=17), PARAMETER, PUBLIC :: date_ver1= & ! - date written -
     '17 December, 2002'
  CHARACTER(LEN=18), PARAMETER, PUBLIC :: date_this= & ! - date of this version -
     '14 March, 2011'
  CHARACTER(LEN=42), PARAMETER, PUBLIC :: CPT_url= &   ! - CPT's URL -
     'http://iri.columbia.edu/climate/tools/cpt/'
  CHARACTER(LEN=20), PARAMETER, PUBLIC :: CPT_email= & ! - CPT's email -
     'cpt@iri.columbia.edu'
!
! Scalars
!
! Character scalars
  CHARACTER(LEN=lver), PUBLIC :: cver ! - window title -
  CHARACTER(LEN=   4), PUBLIC :: cyr1 ! - first copyright year -
  CHARACTER(LEN=   4), PUBLIC :: cyr2 ! - last copyright year -
!
END MODULE version
