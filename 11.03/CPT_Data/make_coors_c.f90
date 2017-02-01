!
! Fortran function with character string as returned value can not be interoperal with C.
! make_coors_c is C compatible wrapper for the Fortran function make_coors
! $Id: make_coors_c.f90 1028 2010-09-02 20:10:48Z lsong $
!
! Define C-bound procedure. 
 SUBROUTINE make_coors_c(rlat,rlng,str,len) BIND(C)
!
! Modules
  USE numbers, ONLY: dp
  USE, INTRINSIC :: ISO_C_BINDING
!
! Input scalars
  REAL(KIND=C_DOUBLE), INTENT(IN) :: rlat ! - latitude -
  REAL(KIND=C_DOUBLE), INTENT(IN) :: rlng ! - longitude -
! Output scalars
  CHARACTER(KIND=C_CHAR), INTENT(OUT) ::str(15)
  INTEGER(KIND=C_INT), INTENT(OUT) :: len
!
! Functions and Subroutines
!
  !CHARACTER(KIND=C_CHAR,LEN=15), EXTERNAL :: make_coors
  CHARACTER(KIND=C_CHAR), EXTERNAL :: make_coors

! Intrinsic functions
  INTRINSIC LEN_TRIM
!
! Local Scalar
  CHARACTER(LEN=15) :: locstr
!
  locstr=make_coors(rlat,rlng)
  str=locstr
  len=LEN_TRIM(locstr)

 END SUBROUTINE make_coors_c
