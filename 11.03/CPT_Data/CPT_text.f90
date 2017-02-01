!$Id: CPT_text.f90 1208 2011-02-25 16:11:55Z lsong $
! Author: Simon Mason
MODULE CPT_text
!
! Parameters
!
! Integer parameters
  INTEGER, PARAMETER, PUBLIC :: nlang=4 ! - number of languages -
!
! Character parameters
  CHARACTER(LEN=10), DIMENSION(nlang), PARAMETER, PUBLIC :: clang= & ! - languages -
     (/'English   ', &
       'Español  ', &
       'Français ', &
       'Português'/)
!
! Scalars
!
! Integer scalars
  INTEGER, PUBLIC :: ilang ! - current language -
!
END MODULE CPT_text
